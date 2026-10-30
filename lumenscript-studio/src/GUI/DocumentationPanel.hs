module GUI.DocumentationPanel
  ( DocumentationPanel(..)
  , createDocumentationPanel
  ) where

import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Documentation.LanguageManual

data DocumentationPanel = DocumentationPanel
  { dpRoot     :: Element
  , dpPrevious :: Element
  , dpNext     :: Element
  }

createDocumentationPanel :: UI DocumentationPanel
createDocumentationPanel = do
  indexRef <- liftIO (newIORef 0)
  titleEl <- UI.h1 # set UI.style [("margin-top", "0")]
  sectionContent <- UI.div
    # set UI.style
        [ ("white-space", "pre-wrap")
        , ("font-family", "times")
        , ("line-height", "1.5")
        , ("padding", "16px 0")
        , ("min-height", "360px")
        , ("overflow-y", "auto")
        ]

  navItems <- forM (zip [0 ..] manualSections) $ \(index, section) -> do
    button <- UI.button
      # set UI.text (sectionTitle section)
      # set UI.style sectionButtonStyle
    pure (button, index)
  sidebar <- UI.div
    # set UI.style
        [ ("width", "220px")
        , ("flex", "0 0 220px")
        , ("padding-right", "16px")
        , ("border-right", "1px solid #444")
        , ("display", "flex")
        , ("flex-direction", "column")
        , ("gap", "4px")
        ]
    #+ map (element . fst) navItems

  previous <- UI.button # set UI.text "Previous"
  next <- UI.button # set UI.text "Next"
  mapM_ (attachNavHandler indexRef titleEl sectionContent navItems previous next) navItems
  on UI.click previous $ \_ -> do
    current <- liftIO (readIORef indexRef)
    renderSection indexRef titleEl sectionContent navItems previous next
      (max 0 (current - 1))
  on UI.click next $ \_ -> do
    current <- liftIO (readIORef indexRef)
    renderSection indexRef titleEl sectionContent navItems previous next
      (min (length manualSections - 1) (current + 1))

  controls <- UI.div
    # set UI.style [("display", "flex"), ("gap", "8px"), ("margin-top", "12px")]
    #+ [element previous, element next]
  article <- UI.div
    # set UI.style [("flex", "1"), ("min-width", "0"), ("padding-left", "24px")]
    #+ [element titleEl, element sectionContent, element controls]
  root <- UI.div
    #+ [ UI.h2 # set UI.text "Language Manual"
       , UI.div
           # set UI.style [("display", "flex"), ("align-items", "flex-start")]
           #+ [element sidebar, element article]
       ]

  renderSection indexRef titleEl sectionContent navItems previous next 0
  pure DocumentationPanel
    { dpRoot = root
    , dpPrevious = previous
    , dpNext = next
    }

attachNavHandler
  :: IORef Int
  -> Element
  -> Element
  -> [(Element, Int)]
  -> Element
  -> Element
  -> (Element, Int)
  -> UI ()
attachNavHandler indexRef titleEl content navItems previous next (button, index) =
  on UI.click button $ \_ ->
    renderSection indexRef titleEl content navItems previous next index

renderSection
  :: IORef Int
  -> Element
  -> Element
  -> [(Element, Int)]
  -> Element
  -> Element
  -> Int
  -> UI ()
renderSection indexRef title content navItems previous next index = do
  let section = manualSections !! index
  liftIO (writeIORef indexRef index)
  void $ element title # set UI.text (sectionTitle section)
  void $ element content # set UI.text (sectionContent section)
  void $ element previous # set UI.enabled (index > 0)
  void $ element next # set UI.enabled (index < length manualSections - 1)
  mapM_ (updateNavItem index) navItems
  where
    updateNavItem selected (button, itemIndex) =
      void $ element button # set UI.style
        (sectionButtonStyle ++
          [ ("background", if selected == itemIndex then "#2e5d50" else "transparent")
          , ("color", if selected == itemIndex then "#fff" else "inherit")
          ])

sectionButtonStyle :: [(String, String)]
sectionButtonStyle =
  [ ("border", "0")
  , ("border-radius", "4px")
  , ("cursor", "pointer")
  , ("padding", "8px 10px")
  , ("text-align", "left")
  , ("font-size", "14px")
  , ("background", "transparent")
  ]