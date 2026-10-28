-- | "Engine API Explorer": browse player / enemy / npc (and the global
-- functions), search them, see signatures, descriptions and LIVE property
-- values from the running simulation, and click a member to insert it into
-- the editor at the caret.
--
-- All data and logic live in the pure module 'ApiCatalog'.
module GUI.ApiExplorer
  ( ApiExplorer(..)
  , createApiExplorer
  , updateApiExplorerLive
  ) where

import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import ApiCatalog
import Simulation.GridWorld (GridWorld)

data ApiExplorer = ApiExplorer
  { axRoot   :: Element
  , axList   :: Element
  , axSource :: Element                       -- ^ the editor textarea
  , axLive   :: IORef [(ApiMember, Element)]  -- ^ property rows that show a live value
  , axWorld  :: IORef (Maybe GridWorld)       -- ^ last world seen from the simulation
  }

createApiExplorer :: Element -> UI ApiExplorer
createApiExplorer sourceArea = do
  hint   <- UI.div # set UI.text "Click any member to insert it into the editor at the caret."
                   # set UI.style [("font-style", "italic")]
  search <- UI.input # set (UI.attr "placeholder") "Search the engine API (try: jump, npc, level)..."
                     # set UI.style [("width", "100%"), ("margin", "6px 0"), ("font-family", "monospace")]
  list   <- UI.div # set UI.style
              [ ("max-height", "360px"), ("overflow-y", "auto")
              , ("border", "1px solid #444"), ("padding", "6px") ]
  root   <- UI.div #+ [ UI.h2 # set UI.text "Engine API Explorer"
                      , element hint, element search, element list ]
  liveRef  <- liftIO (newIORef [])
  worldRef <- liftIO (newIORef Nothing)
  let ax = ApiExplorer root list sourceArea liveRef worldRef
  render ax ""
  on UI.keyup search $ \_ -> do
    q <- get UI.value search
    render ax q
  pure ax

-- | Show the current simulation values next to the properties.
updateApiExplorerLive :: ApiExplorer -> GridWorld -> UI ()
updateApiExplorerLive ax world = do
  liftIO (writeIORef (axWorld ax) (Just world))
  applyLive ax

render :: ApiExplorer -> String -> UI ()
render ax query = do
  blocks <- mapM (objectBlock ax) (searchApi query)
  empty  <- UI.div # set UI.text "No API members match your search."
  void $ element (axList ax) # set children (if null blocks then [empty] else map fst blocks)
  liftIO (writeIORef (axLive ax) (concatMap snd blocks))
  applyLive ax

applyLive :: ApiExplorer -> UI ()
applyLive ax = do
  mworld <- liftIO (readIORef (axWorld ax))
  live   <- liftIO (readIORef (axLive ax))
  mapM_ (\(m, sp) -> void $ element sp # set UI.text (liveText mworld m)) live

liveText :: Maybe GridWorld -> ApiMember -> String
liveText mworld m = case mworld >>= \w -> liveValue w m of
  Just v  -> "   = " ++ v ++ "  (live)"
  Nothing -> "   = \8212"

objectBlock :: ApiExplorer -> ApiObject -> UI (Element, [(ApiMember, Element)])
objectBlock ax o = do
  title <- UI.div # set UI.text (objectTitle o)
                  # set UI.style [("font-weight", "bold"), ("font-size", "16px"), ("margin-top", "8px")]
  doc   <- UI.div # set UI.text (aoDoc o) # set UI.style [("opacity", "0.75"), ("margin-bottom", "4px")]
  rows  <- forM (aoMembers o) (memberRow ax)
  box   <- UI.div #+ ([element title, element doc] ++ map (element . fst) rows)
  pure (box, [ (m, sp) | (m, (_, Just sp)) <- zip (aoMembers o) rows ])

kindColour :: ApiKind -> String
kindColour KindProperty = "#1565c0"
kindColour KindMethod   = "#2e7d32"
kindColour KindFunction = "#6a1b9a"

memberRow :: ApiExplorer -> ApiMember -> UI (Element, Maybe Element)
memberRow ax m = do
  badge <- UI.span # set UI.text (kindLabel (amKind m))
                   # set UI.style
                       [ ("display", "inline-block"), ("min-width", "64px"), ("padding", "0 6px")
                       , ("margin-right", "8px"), ("border-radius", "8px")
                       , ("border", "1px solid " ++ kindColour (amKind m))
                       , ("color", kindColour (amKind m)), ("font-size", "11px"), ("text-align", "center") ]
  sig   <- UI.span # set UI.text (signature m)
                   # set UI.style [("font-family", "monospace"), ("font-weight", "bold")]
  live  <- if amKind m == KindProperty
             then Just <$> (UI.span # set UI.text (liveText Nothing m)
                                    # set UI.style [("font-family", "monospace"), ("color", "#1565c0")])
             else pure Nothing
  top   <- UI.div #+ ([element badge, element sig] ++ maybe [] (\s -> [element s]) live)
  desc  <- UI.div # set UI.text (amDoc m)
                  # set UI.style [("opacity", "0.8"), ("font-size", "13px"), ("margin-left", "78px")]
  row   <- UI.div # set UI.style [ ("cursor", "pointer"), ("padding", "4px 0")
                                 , ("border-bottom", "1px solid rgba(128,128,128,0.25)") ]
                  #+ [element top, element desc]
  on UI.click row $ \_ ->
    runFunction $ ffi "%1.focus(); %1.setRangeText(%2, %1.selectionStart, %1.selectionEnd, 'end');"
                      (axSource ax) (insertText m)
  pure (row, live)