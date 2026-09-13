module GUI.SimulationViewer
  ( SimulationViewer(..)
  , createSimulationViewer
  , updateSimulationViewer
  , dummySimulationLines
  ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import GUI.Panel (createListPanel, updateListPanel)

-- | The simulation viewer panel.
data SimulationViewer = SimulationViewer
  { simRoot    :: Element
  , simContent :: Element
  }

createSimulationViewer :: UI SimulationViewer
createSimulationViewer = do
  (root, content) <- createListPanel "Simulation State" placeholderMsg dummySimulationLines
  pure (SimulationViewer root content)

updateSimulationViewer :: SimulationViewer -> [String] -> UI ()
updateSimulationViewer sv = updateListPanel (simContent sv) placeholderMsg

placeholderMsg :: String
placeholderMsg = "(no simulation running)"

-- | Static placeholder rows shown before any run/step has happened.
dummySimulationLines :: [String]
dummySimulationLines =
  [ "Grid: 10x10"
  , "Player @ (0, 0)"
  ]