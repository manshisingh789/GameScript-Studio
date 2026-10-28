-- | Native engine API catalog: what the script can call on player / enemy /
-- npc (and the global functions), browsable and searchable, with live values
-- and the data autocomplete needs.
--
-- The member list is NOT written out here: it is built from
-- 'HostBindings.hostObjectMembers' and 'HostBindings.hostGlobalFunctions', so
-- a member added there shows up automatically (with a placeholder
-- description until you add one to 'memberDocs').  Pure and GUI-independent.
module ApiCatalog
  ( ApiKind(..), ApiMember(..), ApiObject(..)
  , apiCatalog, allMembers
  , qualifiedName, signature, insertText
  , tyName, kindLabel, objectTitle
  , searchApi, completions
  , liveValue
  , fallbackDoc
  ) where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (intercalate, isInfixOf, isPrefixOf, nub)

import qualified HostBindings as HB
import Simulation.GridWorld (GridWorld, gridLoad)

------------------------------------------------------------------------
-- Types

data ApiKind = KindProperty | KindMethod | KindFunction
  deriving (Eq, Show)

data ApiMember = ApiMember
  { amObject :: String                    -- ^ "" for global functions
  , amName   :: String
  , amKind   :: ApiKind
  , amParams :: [(String, HB.LumType)]
  , amType   :: Maybe HB.LumType          -- ^ property type / return type
  , amDoc    :: String
  } deriving (Eq, Show)

data ApiObject = ApiObject
  { aoName    :: String                   -- ^ "" = the global functions
  , aoDoc     :: String
  , aoMembers :: [ApiMember]
  } deriving (Eq, Show)

------------------------------------------------------------------------
-- Catalog (built from HostBindings)

apiCatalog :: [ApiObject]
apiCatalog = objects ++ [ ApiObject "" (objectDoc "") globals | not (null globals) ]
  where
    objNames = nub [ o | (o, _, _, _) <- HB.hostObjectMembers ]
    objects  = [ ApiObject o (objectDoc o)
                   [ objectMember o m k as | (o', m, k, as) <- HB.hostObjectMembers, o' == o ]
               | o <- objNames ]
    globals  = [ globalMember n as r | HB.GlobalFunction n as r <- HB.hostGlobalFunctions ]

allMembers :: [ApiMember]
allMembers = concatMap aoMembers apiCatalog

objectMember :: String -> String -> HB.MemberKind -> [HB.LumType] -> ApiMember
objectMember o m k as = ApiMember
  { amObject = o
  , amName   = m
  , amKind   = case k of HB.Field -> KindProperty; HB.Method -> KindMethod
  , amParams = zip (paramNames d) as ++ [ ("arg" ++ show i, t) | (i, t) <- drop (length (paramNames d)) (zip [1 :: Int ..] as) ]
  , amType   = case (k, dType d) of
                 (_, Just t)       -> Just t
                 (HB.Method, _)    -> Just HB.TyVoid
                 (HB.Field, _)     -> Nothing
  , amDoc    = dText d
  }
  where d = lookupDoc o m

globalMember :: String -> [HB.LumType] -> HB.LumType -> ApiMember
globalMember n as r = ApiMember
  { amObject = ""
  , amName   = n
  , amKind   = KindFunction
  , amParams = zip (paramNames d ++ map (\i -> "arg" ++ show i) [length (paramNames d) + 1 ..]) as
  , amType   = Just r
  , amDoc    = dText d
  }
  where d = lookupDoc "" n

------------------------------------------------------------------------
-- Descriptions (what GridWorld really does)

data Doc = Doc { paramNames :: [String], dType :: Maybe HB.LumType, dText :: String }

fallbackDoc :: String
fallbackDoc = "(no description yet - add one to memberDocs in ApiCatalog.hs)"

lookupDoc :: String -> String -> Doc
lookupDoc o m = maybe (Doc [] Nothing fallbackDoc) id (lookup (o, m) memberDocs)

objectDoc :: String -> String
objectDoc "player" = "The controllable character. Starts in the middle of the grid."
objectDoc "enemy"  = "A single patrolling enemy. Starts in the top-left corner moving right."
objectDoc "npc"    = "A single non-player character. Starts in the bottom-right corner."
objectDoc ""       = "Functions you can call without an object."
objectDoc o        = "Host object `" ++ o ++ "`."

objectTitle :: ApiObject -> String
objectTitle o = if null (aoName o) then "global functions" else aoName o

memberDocs :: [((String, String), Doc)]
memberDocs =
  [ (("player", "distance"), Doc [] (Just HB.TyInt)
      "Cumulative progress. Goes up by 1 on every player.jump() and by 2 on every dash().")
  , (("player", "level"), Doc [] (Just HB.TyInt)
      "Starts at 1 and goes up by 1 on every third jump.")
  , (("player", "jump"), Doc [] Nothing
      "Jump once: adds 1 to jumps and distance, and every third jump raises level by 1. Does not move the player on the grid.")
  , (("enemy", "patrol"), Doc [] Nothing
      "Move the enemy one cell along the top row; it turns around at either edge of the grid.")
  , (("enemy", "attack"), Doc [] Nothing
      "Record an attack in the event log. It does not change the world yet.")
  , (("npc", "say"), Doc ["text"] Nothing
      "Make the NPC say a line. The text is kept as the NPC's last dialogue.")
  , (("", "print"), Doc ["text"] Nothing
      "Print a string. Accepted by the engine; it has no effect on the world.")
  , (("", "jump"), Doc [] Nothing
      "Same as player.jump().")
  , (("", "dash"), Doc [] Nothing
      "Add 2 to the player's distance. It does not change jumps or level.")
  ]

------------------------------------------------------------------------
-- Display helpers

tyName :: HB.LumType -> String
tyName HB.TyInt    = "int"
tyName HB.TyString = "string"
tyName HB.TyBool   = "bool"
tyName HB.TyVoid   = "void"

kindLabel :: ApiKind -> String
kindLabel KindProperty = "property"
kindLabel KindMethod   = "method"
kindLabel KindFunction = "function"

qualifiedName :: ApiMember -> String
qualifiedName m = if null (amObject m) then amName m else amObject m ++ "." ++ amName m

-- | "player.distance: int"  /  "npc.say(text: string) -> void"
signature :: ApiMember -> String
signature m = qualifiedName m ++ case amKind m of
  KindProperty -> maybe "" (\t -> ": " ++ tyName t) (amType m)
  _            -> "(" ++ intercalate ", " [ n ++ ": " ++ tyName t | (n, t) <- amParams m ] ++ ")"
                  ++ " -> " ++ maybe "void" tyName (amType m)

-- | Text inserted into the editor when a member is clicked.
insertText :: ApiMember -> String
insertText m = qualifiedName m ++ case amKind m of
  KindProperty -> ""
  _            -> "(" ++ intercalate ", " (map (placeholder . snd) (amParams m)) ++ ")"
  where
    placeholder HB.TyString = "\"text\""
    placeholder HB.TyInt    = "0"
    placeholder HB.TyBool   = "true"
    placeholder HB.TyVoid   = ""

------------------------------------------------------------------------
-- Search and completion

-- | Members whose qualified name contains the query (case-insensitive).
-- An empty query returns the whole catalog.
searchApi :: String -> [ApiObject]
searchApi query
  | null q    = apiCatalog
  | otherwise = [ o { aoMembers = ms } | o <- apiCatalog
                , let ms = [ m | m <- aoMembers o, q `isInfixOf` map toLower (qualifiedName m) ]
                , not (null ms) ]
  where q = map toLower (trim query)
        trim = f . f where f = reverse . dropWhile isSpace

-- | Autocomplete groundwork: the text before the caret -> (label, detail)
-- candidates.  @"player.ju"@ -> jump; @"pl"@ -> player; @"j"@ -> the global jump().
completions :: String -> [(String, String)]
completions before = case break (== '.') word of
  (obj, '.' : prefix) ->
    [ (amName m, signature m)
    | o <- apiCatalog, aoName o == obj, not (null obj)
    , m <- aoMembers o, prefix `isPrefixOf` amName m ]
  (prefix, _)
    | null prefix -> []
    | otherwise ->
        [ (aoName o, "object") | o <- apiCatalog, not (null (aoName o)), prefix `isPrefixOf` aoName o ]
        ++ [ (amName m, signature m) | o <- apiCatalog, null (aoName o), m <- aoMembers o, prefix `isPrefixOf` amName m ]
  where
    word = reverse (takeWhile (\c -> isAlphaNum c || c == '_' || c == '.') (reverse before))

------------------------------------------------------------------------
-- Live values

-- | Current value of a property in the running simulation, if it is one.
liveValue :: GridWorld -> ApiMember -> Maybe String
liveValue world m
  | amKind m == KindProperty = show <$> gridLoad (amObject m) (amName m) world
  | otherwise                = Nothing