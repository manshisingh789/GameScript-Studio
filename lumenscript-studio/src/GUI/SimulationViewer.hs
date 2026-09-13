module GUI.SimulationViewer
  ( SimulationViewer(..)
  , createSimulationViewer
  , updateSimulationViewer
  , formatSimState
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import qualified Data.Map                    as Map
import           GUI.Panel                   (createListPanel, updateListPanel)
import           Simulation.SimState         (SimState (..))
import           Simulation.GridWorld        (GridWorld(..), PlayerState(..), EnemyState(..), NpcState(..), Position(..))

-- | The simulation viewer panel.
data SimulationViewer = SimulationViewer
  { simRoot    :: Element
  , simContent :: Element
  }

createSimulationViewer :: UI SimulationViewer
createSimulationViewer = do
  (root, content) <- createListPanel "Simulation State" placeholderMsg []
  pure (SimulationViewer root content)

updateSimulationViewer :: SimulationViewer -> SimState -> UI ()
updateSimulationViewer sv st = updateListPanel (simContent sv) placeholderMsg (formatSimState st)

formatSimState :: SimState -> [String]
formatSimState st =
  let world = simWorld st
      player = gwPlayer world
      enemy = gwEnemy world
      npc = gwNpc world

      Position px py = playerPos player
      Position ex ey = enemyPos enemy
      Position nx ny = npcPos npc

      vars = Map.toList (simVars st)

      variableLines =
        if null vars
          then ["  (none)"]
          else map (\(name, value) -> "  " ++ name ++ " = " ++ show value) vars

      npcDialogue =
        case npcLastLine npc of
          Nothing -> "(none)"
          Just msg -> msg

  in
    [ "WORLD"
    , "  Grid: " ++ show (gwWidth world) ++ "x" ++ show (gwHeight world)
    , ""
    , "PLAYER"
    , "  Position: (" ++ show px ++ ", " ++ show py ++ ")"
    , "  Distance: " ++ show (playerDistance player)
    , "  Level: " ++ show (playerLevel player)
    , "  Jumps: " ++ show (playerJumps player)
    , ""
    , "ENEMY"
    , "  Position: (" ++ show ex ++ ", " ++ show ey ++ ")"
    , "  Patrol direction: " ++ show (enemyPatrolDir enemy)
    , ""
    , "NPC"
    , "  Position: (" ++ show nx ++ ", " ++ show ny ++ ")"
    , "  Last dialogue: " ++ npcDialogue
    , ""
    , "VARIABLES"
    ] ++ variableLines

placeholderMsg :: String
placeholderMsg = "(no simulation running)"