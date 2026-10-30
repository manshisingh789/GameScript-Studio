module ApiIntegrationSpec (spec) where

import qualified Data.Map as Map
import Data.IORef (IORef, newIORef, readIORef)
import Test.Hspec

import AST (EventSpec(..))
import Compiler (CompilationResult(..), compile)
import Simulation.GridWorld
import Simulation.SimState
import VM.EventTrigger (CompiledProgram(..))
import VM.Stack (Value(..))

apiScriptPath :: FilePath
apiScriptPath = "demos/api_integration.lum"

loadApiScript :: IO String
loadApiScript = readFile apiScriptPath

compileApiScript :: IO CompiledProgram
compileApiScript = do
  source <- loadApiScript
  case compile source of
    CompilationSuccess program -> pure program
    other -> fail
      ("API integration script did not compile: " ++ show other)

spec :: Spec
spec = describe "engine API integration script" $ do
  it "compiles the documented property, method, global, and event calls" $ do
    program <- compileApiScript
    length (cpHandlers program) `shouldBe` 6

  it "reads updated player properties after jump and dash calls" $ do
    program <- compileApiScript
    let initialVars = Map.fromList
          [ ("progress", VInt 0)
          , ("level", VInt 1)
          ]
    ref <- newSimState initialVars

    stepEvent ref program (KeyPress "j")
    afterJump <- readSimState ref
    simVars afterJump `shouldSatisfy` hasValues
      [ ("progress", VInt 1)
      , ("level", VInt 1)
      ]
    playerDistance (gwPlayer (simWorld afterJump)) `shouldBe` 1
    simLog afterJump `shouldContain` ["[sim] print[\"Player jumped!\"]"]

    stepEvent ref program (KeyPress "d")
    afterDash <- readSimState ref
    simVars afterDash `shouldSatisfy` hasValues
      [ ("progress", VInt 1)
      , ("level", VInt 1)
      ]
    playerDistance (gwPlayer (simWorld afterDash)) `shouldBe` 3

  it "updates player level after every third jump" $ do
    program <- compileApiScript
    ref <- newSimState Map.empty

    stepEvent ref program (KeyPress "j")
    stepEvent ref program (KeyPress "j")
    stepEvent ref program (KeyPress "j")

    state <- readSimState ref

    playerDistance (gwPlayer (simWorld state)) `shouldBe` 3
    playerLevel (gwPlayer (simWorld state)) `shouldBe` 2

  it "increments player distance on global jump call" $ do
    program <- compileApiScript
    ref <- newSimState Map.empty

    stepEvent ref program (KeyPress "g")
    state <- readSimState ref

    playerDistance (gwPlayer (simWorld state)) `shouldBe` 1

  it "executes the enemy and NPC API handlers" $ do
    program <- compileApiScript
    ref <- newSimState Map.empty

    stepEvent ref program (KeyPress "e")
    stepEvent ref program (KeyPress "a")
    stepEvent ref program (KeyPress "n")
    state <- readSimState ref

    enemyPos (gwEnemy (simWorld state)) `shouldBe` Position 1 0
    npcLastLine (gwNpc (simWorld state)) `shouldBe` Just "Hello, traveler!"
    simLog state `shouldContain` ["[sim] enemy.attack[]"]

newSimState :: Map.Map String Value -> IO (IORef SimState)
newSimState vars = newIORef (initSimState 10 8 vars)

readSimState :: IORef SimState -> IO SimState
readSimState = readIORef

hasValues :: [(String, Value)] -> Map.Map String Value -> Bool
hasValues expected actual = all (\(name, value) -> Map.lookup name actual == Just value) expected