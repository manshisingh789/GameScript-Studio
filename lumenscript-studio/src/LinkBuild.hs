-- | Builds the 'SourceLink.LinkTable' for the REAL compiler output
-- (tokens from Lexer.lexer, AST from Parser.parseProgram) without touching
-- Lexer/Parser: it recovers each AST node's source span from the token
-- stream, using the Position every Var/Member/Unary/Binary/Call/Decl/If/
-- OnEvent node already stores, plus in-order token matching for literals
-- (which carry no Position) and brace/paren matching for blocks.
module LinkBuild
  ( buildLinkTable
  , tokenSpans
  , tokenLength
  , sameShape
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

import AST
import Bytecode.Instruction (Instr(..))
import SourceLink
import Token

------------------------------------------------------------------------
-- Tokens -> spans

-- | Character length of a token's lexeme (same table GUI.Panel uses for
-- the hover-select feature).
tokenLength :: Token -> Int
tokenLength token = case tokenType token of
  TInt n -> length (show n)
  TString s -> length s + 2
  TBool b -> length (show b)
  TIdent s -> length s
  TKwLet -> 3
  TKwConst -> 5
  TKwIf -> 2
  TKwElse -> 4
  TKwElif -> 4
  TKwOn -> 2
  TKwKeyPress -> 9
  TKwCollision -> 9
  TKwUpdate -> 6
  TKwInteract -> 8
  TEq -> 2
  TNotEqual -> 2
  TLe -> 2
  TGe -> 2
  TEOF -> 0
  TError s -> length s
  _ -> 1

-- | Offset-based spans (0-based, half-open) for every token, computed from
-- its (line, column) exactly like GUI.Panel does (1-based line/column).
tokenSpans :: String -> [Token] -> [Span]
tokenSpans src = map one
  where
    n      = length src
    starts = scanl (+) 0 (map ((+ 1) . length) (lines src))
    startOf l = case drop (l - 1) starts of (x : _) -> x; [] -> n
    one t =
      let Position l c = tokenPosition t
          s = max 0 (min n (startOf l + c - 1))
          e = max s (min n (s + tokenLength t))
      in Span s e

------------------------------------------------------------------------
-- Environment over the token stream

data Env = Env
  { eToks :: IM.IntMap (TokenPayload, Span)
  , ePos  :: M.Map (Int, Int) Int          -- (line, column) -> token index
  }

mkEnv :: String -> [Token] -> Env
mkEnv src toks = Env
  { eToks = IM.fromList (zip [0 ..] (zip (map tokenType toks) (tokenSpans src toks)))
  , ePos  = M.fromList [ ((line p, column p), i) | (i, t) <- zip [0 ..] toks, let p = tokenPosition t ]
  }

payloadAt :: Env -> Int -> Maybe TokenPayload
payloadAt env i = fst <$> IM.lookup i (eToks env)

spanOfTok :: Env -> Int -> Maybe Span
spanOfTok env i = snd <$> IM.lookup i (eToks env)

idxAt :: Env -> Position -> Maybe Int
idxAt env p = M.lookup (line p, column p) (ePos env)

-- | First token index >= cur whose payload satisfies the predicate.
findFrom :: Env -> Int -> (TokenPayload -> Bool) -> Maybe Int
findFrom env cur ok =
  case [ i | (i, (pl, _)) <- IM.toAscList (snd (IM.split (cur - 1) (eToks env))), ok pl ] of
    (i : _) -> Just i
    []      -> Nothing

-- | Nearest token before i (within 4) satisfying the predicate.
backFind :: Env -> Int -> (TokenPayload -> Bool) -> Maybe Int
backFind env i ok =
  case [ j | j <- [i - 1, i - 2 .. max 0 (i - 4)], maybe False ok (payloadAt env j) ] of
    (j : _) -> Just j
    []      -> Nothing

-- | Index of the bracket closing the one at 'open'.
matchFrom :: Env -> Int -> TokenPayload -> TokenPayload -> Maybe Int
matchFrom env open o c = go (open + 1) (1 :: Int)
  where
    go i d = case payloadAt env i of
      Nothing -> Nothing
      Just pl
        | pl == c   -> if d == 1 then Just i else go (i + 1) (d - 1)
        | pl == o   -> go (i + 1) (d + 1)
        | otherwise -> go (i + 1) d

-- | Next token at or after i that is not a newline/blank.
nextSig :: Env -> Int -> Maybe Int
nextSig env i = findFrom env i (\p -> p /= TNewline && p /= TBlank)

semiAt :: Env -> Int -> Maybe Int
semiAt env i = case payloadAt env i of
  Just TSemicolon -> Just i
  _               -> Nothing

-- | Closing brace of an if / elif / else chain starting at token 'start'.
endOfChain :: Env -> Int -> Maybe Int
endOfChain env start = findFrom env start (== TBraceL) >>= loop
  where
    loop open = do
      close <- matchFrom env open TBraceL TBraceR
      case nextSig env (close + 1) of
        Just ni | payloadAt env ni `elem` [Just TKwElse, Just TKwElif] ->
          case findFrom env ni (== TBraceL) of
            Just open' -> loop open'
            Nothing    -> Just close
        _ -> Just close

------------------------------------------------------------------------
-- AST walk (source order, threading a token cursor)

data Node = Node Int String (Maybe Span)

-- | Result of walking one construct: its span, its AST nodes (pre-order),
-- the instructions Bytecode.Generator emits for it (FORWARD order, label
-- names blank) each tagged with the source span it came from, and the
-- token cursor after it.
data W = W { wSpan :: Maybe Span, wNodes :: [Node], wCode :: [(Instr, Maybe Span)], wCur :: Int }

hullM :: [Maybe Span] -> Maybe Span
hullM = hullSpans . catMaybes

walkExprs :: Env -> Int -> Int -> [Expr] -> W
walkExprs env d cur = foldl step (W Nothing [] [] cur)
  where step acc e = let w = walkExpr env d (wCur acc) e
                     in W (hullM [wSpan acc, wSpan w]) (wNodes acc ++ wNodes w) (wCode acc ++ wCode w) (wCur w)

walkExpr :: Env -> Int -> Int -> Expr -> W
walkExpr env d cur ex = case ex of
  LitInt k  -> lit (TInt k)  ("LitInt " ++ show k) (PUSH_INT k)
  LitStr s  -> lit (TString s) ("LitStr " ++ show s) (PUSH_STR s)
  LitBool b -> lit (TBool b) ("LitBool " ++ show b) (PUSH_BOOL b)

  Var x p ->
    let label = "Var " ++ x
    in case locate p (TIdent x) of
         Just i  -> W (sp i) [Node d label (sp i)] [(LOAD_VAR x, sp i)] (i + 1)
         Nothing -> W Nothing [Node d label Nothing] [(LOAD_VAR x, Nothing)] cur

  Member e x p ->
    let we = walkExpr env (d + 1) cur e
        identIdx = case idxAt env p of
          Just i | payloadAt env i == Just TDot        -> Just (i + 1)
                 | payloadAt env i == Just (TIdent x)  -> Just i
          _ -> findFrom env (wCur we) (== TIdent x)
        s = hullM [wSpan we, identIdx >>= sp]
        code = case e of
          Var o _ -> [(LOAD o x, s)]
          _       -> (LOAD (calleeLabel e) x, s) : wCode we
    in W s (Node d ("Member ." ++ x) s : wNodes we) code (maybe (wCur we) (+ 1) identIdx)

  Unary op e p ->
    let opIdx = idxAt env p
        we = walkExpr env (d + 1) (maybe cur (+ 1) opIdx) e
        s = hullM [opIdx >>= sp, wSpan we]
    in W s (Node d ("Unary " ++ show op) s : wNodes we) (wCode we ++ [(UNOP op, s)]) (wCur we)

  Binary op l r p ->
    let wl = walkExpr env (d + 1) cur l
        opIdx = idxAt env p
        wr = walkExpr env (d + 1) (maybe (wCur wl) (+ 1) opIdx) r
        s = hullM [wSpan wl, opIdx >>= sp, wSpan wr]
    in W s (Node d ("Binary " ++ show op) s : wNodes wl ++ wNodes wr)
         (wCode wl ++ wCode wr ++ [(BINOP op, s)]) (wCur wr)

  Call callee args p ->
    let wc = walkExpr env (d + 1) cur callee
        lp = case idxAt env p of
               Just i | payloadAt env i == Just TLParen -> Just i
               _ -> findFrom env (wCur wc) (== TLParen)
        wa = walkExprs env (d + 1) (maybe (wCur wc) (+ 1) lp) args
        close = lp >>= \i -> matchFrom env i TLParen TRParen
        s = hullM [wSpan wc, lp >>= sp, wSpan wa, close >>= sp]
        n = length args
        callInstr = case callee of
          Member (Var o _) f _ -> [(CALL o f n, s)]
          Var f _              -> [(CALL "" f n, s)]
          _                    -> []      -- Generator rejects other callees
    in W s (Node d "Call" s : wNodes wc ++ wNodes wa) (wCode wa ++ callInstr) (maybe (wCur wa) (+ 1) close)
  where
    sp = spanOfTok env
    lit pl label ins = case findFrom env cur (== pl) of
      Just i  -> W (sp i) [Node d label (sp i)] [(ins, sp i)] (i + 1)
      Nothing -> W Nothing [Node d label Nothing] [(ins, Nothing)] cur
    locate p pl = case idxAt env p of
      Just i | payloadAt env i == Just pl -> Just i
      _ -> findFrom env cur (== pl)

walkStmts :: Env -> Int -> Int -> [Stmt] -> W
walkStmts env d cur = foldl step (W Nothing [] [] cur)
  where step acc s = let w = walkStmt env d (wCur acc) s
                     in W (hullM [wSpan acc, wSpan w]) (wNodes acc ++ wNodes w) (wCode acc ++ wCode w) (wCur w)

walkStmt :: Env -> Int -> Int -> Stmt -> W
walkStmt env d cur st = case st of
  Decl x mut e p ->
    let identIdx = idxAt env p
        keyIdx = identIdx >>= \i -> backFind env i (\pl -> pl == TKwLet || pl == TKwConst)
        we = walkExpr env (d + 1) (maybe cur (+ 1) identIdx) e
        semi = semiAt env (wCur we)
        s = hullM [keyIdx >>= sp, identIdx >>= sp, wSpan we, semi >>= sp]
        kw = if mut == Mutable then "let" else "const"
    in W s (Node d ("Decl " ++ kw ++ " " ++ x) s : wNodes we)
         (wCode we ++ [(STORE_VAR x, s)]) (maybe (wCur we) (+ 1) semi)

  Assign t v _ ->
    let wt = walkExpr env (d + 1) cur t
        eq = findFrom env (wCur wt) (== TAssign)
        wv = walkExpr env (d + 1) (maybe (wCur wt) (+ 1) eq) v
        semi = semiAt env (wCur wv)
        s = hullM [wSpan wt, eq >>= sp, wSpan wv, semi >>= sp]
        code = case t of
          Var name _ -> wCode wv ++ [(STORE_VAR name, s)]
          _          -> []           -- Generator emits nothing for member targets
    in W s (Node d "Assign" s : wNodes wt ++ wNodes wv) code (maybe (wCur wv) (+ 1) semi)

  ExprStmt e ->
    let w = walkExpr env (d + 1) cur e
        semi = semiAt env (wCur w)
        s = hullM [wSpan w, semi >>= sp]
    in W s (Node d "ExprStmt" s : wNodes w) (wCode w ++ [(POP, s)]) (maybe (wCur w) (+ 1) semi)

  If c ts me p ->
    let ifIdx = case idxAt env p of
                  Just i -> Just i
                  Nothing -> findFrom env cur (\pl -> pl == TKwIf || pl == TKwElif)
        wc = walkExpr env (d + 1) (maybe cur (+ 1) ifIdx) c
        open = findFrom env (wCur wc) (== TBraceL)
        wt = walkStmts env (d + 1) (maybe (wCur wc) (+ 1) open) ts
        close = open >>= \i -> matchFrom env i TBraceL TBraceR
        elseKw = close >>= \i -> nextSig env (i + 1) >>= \j ->
                   if payloadAt env j `elem` [Just TKwElse, Just TKwElif] then Just j else Nothing
        we = case (me, elseKw) of
               (Just es, Just j) -> walkStmts env (d + 1) (j + 1) es
               (Just es, Nothing) -> walkStmts env (d + 1) (wCur wt) es
               _ -> W Nothing [] [] (wCur wt)
        endIdx = ifIdx >>= endOfChain env
        s = hullM [ifIdx >>= sp, endIdx >>= sp, wSpan wc]
        code = case me of
          Nothing -> wCode wc ++ [(JUMP_IF_FALSE "", wSpan wc)] ++ wCode wt ++ [(LABEL "", s)]
          Just _  -> wCode wc ++ [(JUMP_IF_FALSE "", wSpan wc)] ++ wCode wt
                       ++ [(JUMP "", s), (LABEL "", s)] ++ wCode we ++ [(LABEL "", s)]
    in W s (Node d "If" s : wNodes wc ++ wNodes wt ++ wNodes we) code
         (maybe (max (wCur wt) (wCur we)) (+ 1) endIdx)

  OnEvent spec ss p ->
    let onIdx = idxAt env p
        open = onIdx >>= \i -> findFrom env (i + 1) (== TBraceL)
        close = open >>= \i -> matchFrom env i TBraceL TBraceR
        wb = walkStmts env (d + 1) (maybe (maybe cur (+ 1) onIdx) (+ 1) open) ss
        s = hullM [onIdx >>= sp, close >>= sp, wSpan wb]
        code = case spec of
          KeyPress _ -> [(JUMP "", s), (LABEL "", s)] ++ wCode wb ++ [(LABEL "", s)]
          _          -> []           -- Generator has no clause for other events
    in W s (Node d ("OnEvent " ++ describe spec) s : wNodes wb) code (maybe (wCur wb) (+ 1) close)
  where
    sp = spanOfTok env
    describe (KeyPress k)       = "key_press " ++ k
    describe (GenericEvent n a) = unwords (n : a)

calleeLabel :: Expr -> String
calleeLabel (Var name _)        = name
calleeLabel (Member o f _)      = calleeLabel o ++ "." ++ f
calleeLabel other               = show other

-- | Same instruction, ignoring label names (the generator numbers labels
-- itself; we only need to know WHICH instruction is which).
sameShape :: Instr -> Instr -> Bool
sameShape (LABEL _) (LABEL _)                   = True
sameShape (JUMP _) (JUMP _)                     = True
sameShape (JUMP_IF_FALSE _) (JUMP_IF_FALSE _)   = True
sameShape a b                                   = a == b

-- | Source text + real tokens + real AST (+ the real generated
-- instructions, if compilation succeeded) -> the table
-- 'SourceLink.resolve' works on.  Instruction index == VM instruction
-- pointer.  If the instructions cannot be aligned with the AST (e.g.
-- semantic analysis rewrote the tree) the bytecode column is still
-- listed, just without links, and 'ltNote' says so.
buildLinkTable :: String -> [Token] -> Program -> Maybe [Instr] -> LinkTable
buildLinkTable src toks prog mInstrs = LinkTable
  { ltSource = src
  , ltTokens = [ Item (show (tokenType t)) (Just s) | (t, s) <- zip toks (tokenSpans src toks) ]
  , ltAst    = [ Item (replicate (2 * dep) ' ' ++ lbl) sp | Node dep lbl sp <- Node 0 "Program" (wSpan w) : wNodes w ]
  , ltCode   = codeItems
  , ltNote   = note
  }
  where
    env = mkEnv src toks
    w   = walkStmts env 1 0 prog
    origins = wCode w
    row i ins = pad (show i) ++ "  " ++ show ins
    pad x = replicate (3 - length x) ' ' ++ x
    (codeItems, note) = case mInstrs of
      Nothing -> ([], "")
      Just instrs
        | length instrs == length origins && and (zipWith sameShape instrs (map fst origins)) ->
            ( [ Item (row i ins) sp | (i, ins, (_, sp)) <- zip3 [0 :: Int ..] instrs origins ], "" )
        | otherwise ->
            ( [ Item (row i ins) Nothing | (i, ins) <- zip [0 :: Int ..] instrs ]
            , "Bytecode could not be aligned with the AST, so instruction links are unavailable." )