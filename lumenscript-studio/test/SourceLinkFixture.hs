-- | Hand-built link table for:   let x = a + b;  print(x);
-- Replace with the real pipeline output once your stages carry spans.
module SourceLinkFixture (demoSource, demoTable) where

import Data.List (isPrefixOf, tails)
import SourceLink

demoSource :: String
demoSource = "let x = a + b;\nprint(x);"

-- | n-th (0-based) occurrence of a substring as a Span.
sub :: Int -> String -> Span
sub n needle = Span p (p + length needle)
  where p = [ i | (i, t) <- zip [0 ..] (tails demoSource), needle `isPrefixOf` t ] !! n

demoTable :: LinkTable
demoTable = LinkTable
  { ltSource = demoSource
  , ltTokens =                                   -- ids
      [ Item "KW let"       (Just (sub 0 "let"))   -- 0
      , Item "IDENT x"      (Just (sub 0 "x"))     -- 1
      , Item "ASSIGN ="     (Just (sub 0 "="))     -- 2
      , Item "IDENT a"      (Just (sub 0 "a"))     -- 3
      , Item "PLUS +"       (Just (sub 0 "+"))     -- 4
      , Item "IDENT b"      (Just (sub 0 "b"))     -- 5
      , Item "SEMI ;"       (Just (sub 0 ";"))     -- 6
      , Item "IDENT print"  (Just (sub 0 "print")) -- 7
      , Item "LPAREN ("     (Just (sub 0 "("))     -- 8
      , Item "IDENT x"      (Just (sub 1 "x"))     -- 9
      , Item "RPAREN )"     (Just (sub 0 ")"))     -- 10
      , Item "SEMI ;"       (Just (sub 1 ";"))     -- 11
      ]
  , ltAst =
      [ Item "Program"        (Just (Span 0 (length demoSource)))   -- 0
      , Item "Let x"          (Just (sub 0 "let x = a + b;"))       -- 1
      , Item "BinOp +"        (Just (sub 0 "a + b"))                -- 2
      , Item "Var a"          (Just (sub 0 "a"))                    -- 3
      , Item "Var b"          (Just (sub 0 "b"))                    -- 4
      , Item "Call print"     (Just (sub 0 "print(x);"))            -- 5
      , Item "Var x"          (Just (sub 1 "x"))                    -- 6
      ]
  , ltCode =
      [ Item "LOAD_VAR a"     (Just (sub 0 "a"))                    -- 0
      , Item "LOAD_VAR b"     (Just (sub 0 "b"))                    -- 1
      , Item "ADD"            (Just (sub 0 "a + b"))                -- 2
      , Item "STORE x"        (Just (sub 0 "let x = a + b;"))       -- 3
      , Item "LOAD_VAR x"     (Just (sub 1 "x"))                    -- 4
      , Item "CALL print 1"   (Just (sub 0 "print(x);"))            -- 5
      , Item "HALT"           Nothing                               -- 6
      ]
  , ltNote = ""
  }