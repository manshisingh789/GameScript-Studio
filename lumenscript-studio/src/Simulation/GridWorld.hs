module Simulation.GridWorld
  ( Position(..)
  , Direction(..)
  , PlayerState(..)
  , EnemyState(..)
  , NpcState(..)
  , GridWorld(..)
  , initGridWorld
  , movePlayer
  , gridLoad
  , gridCall
  ) where

import AST (Ident)
import VM.Stack (Value(..))

-- | A cell coordinate on the simulation grid. (0,0) is the top-left.
data Position = Position { posX :: Int, posY :: Int }
  deriving (Show, Eq)

data Direction = DUp | DDown | DLeft | DRight
  deriving (Show, Eq)

-- | The player's state. distance/level are exactly what
-- Semantic.SymbolTable exposes as player.distance / player.level.
data PlayerState = PlayerState
  { playerPos      :: Position
  , playerDistance :: Int  -- ^ cumulative steps/jumps taken
  , playerLevel    :: Int  -- ^ increases every few jumps
  , playerJumps    :: Int  -- ^ raw jump counter driving leveling
  } deriving (Show, Eq)

-- | A single patrolling enemy. Only one enemy instance exists, matching
-- the built-in symbol table's single fixed "enemy" name (not a
-- collection of enemies).
data EnemyState = EnemyState
  { enemyPos       :: Position
  , enemyPatrolDir :: Direction
  } deriving (Show, Eq)

-- | A single NPC. npcLastLine is what it most recently said via
-- npc.say(...), kept so a GUI can show a speech bubble.
data NpcState = NpcState
  { npcPos      :: Position
  , npcLastLine :: Maybe String
  } deriving (Show, Eq)

data GridWorld = GridWorld
  { gwWidth  :: Int
  , gwHeight :: Int
  , gwPlayer :: PlayerState
  , gwEnemy  :: EnemyState
  , gwNpc    :: NpcState
  } deriving (Show, Eq)

-- | A fresh world: player centered, enemy at the top-left corner
-- (patrolling right), npc at the bottom-right corner.
initGridWorld :: Int -> Int -> GridWorld
initGridWorld w h = GridWorld
  { gwWidth  = w
  , gwHeight = h
  , gwPlayer = PlayerState (Position (w `div` 2) (h `div` 2)) 0 1 0
  , gwEnemy  = EnemyState (Position 0 0) DRight
  , gwNpc    = NpcState (Position (w - 1) (h - 1)) Nothing
  }

-- | Move the player one cell in a direction, clamped to the grid
-- bounds, bumping distance only if the move actually changed position
-- (walking into a wall doesn't count as progress). Not currently
-- wired to any built-in call -- reserved for a future direct
-- movement event (e.g. arrow-key handlers) once that's in scope.
movePlayer :: Direction -> GridWorld -> GridWorld
movePlayer dir gw =
  let PlayerState (Position x y) dist lvl jumps = gwPlayer gw
      (x', y') = case dir of
        DUp    -> (x, max 0 (y - 1))
        DDown  -> (x, min (gwHeight gw - 1) (y + 1))
        DLeft  -> (max 0 (x - 1), y)
        DRight -> (min (gwWidth gw - 1) (x + 1), y)
      moved = (x', y') /= (x, y)
      dist' = if moved then dist + 1 else dist
  in gw { gwPlayer = PlayerState (Position x' y') dist' lvl jumps }

-- | player.jump(): advances the jump counter and distance; every 3
-- jumps the player levels up. Doesn't move the player spatially --
-- a jump is progress, not a position change.
playerJump :: PlayerState -> PlayerState
playerJump p =
  let jumps' = playerJumps p + 1
      dist'  = playerDistance p + 1
      lvl'   = playerLevel p + (if jumps' `mod` 3 == 0 then 1 else 0)
  in p { playerJumps = jumps', playerDistance = dist', playerLevel = lvl' }

-- | enemy.patrol(): bounces the enemy left/right along the top row,
-- reversing direction at either grid edge.
enemyPatrol :: GridWorld -> EnemyState -> EnemyState
enemyPatrol gw e =
  let Position x y = enemyPos e
      atRightEdge  = x >= gwWidth gw - 1
      atLeftEdge   = x <= 0
      dir'
        | enemyPatrolDir e == DRight && atRightEdge = DLeft
        | enemyPatrolDir e == DLeft  && atLeftEdge  = DRight
        | otherwise                                 = enemyPatrolDir e
      x' = case dir' of
        DRight -> x + 1
        DLeft  -> x - 1
        _      -> x
  in e { enemyPos = Position x' y, enemyPatrolDir = dir' }

-- | Resolve a LOAD instruction (obj.field) against the current world.
-- Returns Nothing for anything not in Semantic.SymbolTable's built-in
-- member list -- callers should treat that as an internal error, since
-- the semantic checker should have already rejected it.
gridLoad :: Ident -> Ident -> GridWorld -> Maybe Value
gridLoad "player" "distance" gw = Just (VInt (playerDistance (gwPlayer gw)))
gridLoad "player" "level"    gw = Just (VInt (playerLevel (gwPlayer gw)))
gridLoad _        _          _  = Nothing

-- | Apply a CALL instruction (obj.method(args)) to the world, returning
-- the updated world and the call's result value (VUnit for all current
-- built-ins, since none of player.jump/enemy.attack/enemy.patrol/
-- npc.say produce a value per Semantic.SymbolTable's TFunction return
-- types, all TVoid).
gridCall :: Ident -> Ident -> [Value] -> GridWorld -> (GridWorld, Value)
gridCall "player" "jump" _ gw =
  (gw { gwPlayer = playerJump (gwPlayer gw) }, VUnit)
gridCall "enemy" "patrol" _ gw =
  (gw { gwEnemy = enemyPatrol gw (gwEnemy gw) }, VUnit)
gridCall "enemy" "attack" _ gw =
  (gw, VUnit)  -- no world-state effect yet; log entry is what marks it happening
gridCall "npc" "say" args gw =
  case args of
    [VStr msg] -> (gw { gwNpc = (gwNpc gw) { npcLastLine = Just msg } }, VUnit)
    _          -> (gw, VUnit)  -- wrong arity/type already caught by Semantic.TypeCheck
gridCall _ _ _ gw = (gw, VUnit)  -- unrecognized call; semantic checker should prevent this