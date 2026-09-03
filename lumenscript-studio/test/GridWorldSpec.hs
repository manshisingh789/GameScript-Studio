module GridWorldSpec (spec) where

import Test.Hspec

import Simulation.GridWorld
import VM.Stack (Value(..))

spec :: Spec
spec = do
  describe "initGridWorld" $ do
    it "sets the requested width and height" $ do
      let gw = initGridWorld 10 8
      gwWidth gw `shouldBe` 10
      gwHeight gw `shouldBe` 8

    it "starts the player centered on the grid" $ do
      let gw = initGridWorld 10 8
      playerPos (gwPlayer gw) `shouldBe` Position 5 4

    it "starts the player with zero distance, level 1, and zero jumps" $ do
      let gw = initGridWorld 10 8
          p  = gwPlayer gw
      playerDistance p `shouldBe` 0
      playerLevel p `shouldBe` 1
      playerJumps p `shouldBe` 0

    it "starts the enemy at the top-left corner, patrolling right" $ do
      let gw = initGridWorld 10 8
          e  = gwEnemy gw
      enemyPos e `shouldBe` Position 0 0
      enemyPatrolDir e `shouldBe` DRight

    it "starts the npc at the bottom-right corner with no last line" $ do
      let gw = initGridWorld 10 8
          n  = gwNpc gw
      npcPos n `shouldBe` Position 9 7
      npcLastLine n `shouldBe` Nothing

  describe "movePlayer" $ do
    it "moves the player right and increments distance" $ do
      let gw  = initGridWorld 10 8
          gw' = movePlayer DRight gw
      playerPos (gwPlayer gw') `shouldBe` Position 6 4
      playerDistance (gwPlayer gw') `shouldBe` 1

    it "moves the player left and increments distance" $ do
      let gw  = initGridWorld 10 8
          gw' = movePlayer DLeft gw
      playerPos (gwPlayer gw') `shouldBe` Position 4 4
      playerDistance (gwPlayer gw') `shouldBe` 1

    it "moves the player up and increments distance" $ do
      let gw  = initGridWorld 10 8
          gw' = movePlayer DUp gw
      playerPos (gwPlayer gw') `shouldBe` Position 5 3
      playerDistance (gwPlayer gw') `shouldBe` 1

    it "moves the player down and increments distance" $ do
      let gw  = initGridWorld 10 8
          gw' = movePlayer DDown gw
      playerPos (gwPlayer gw') `shouldBe` Position 5 5
      playerDistance (gwPlayer gw') `shouldBe` 1

    it "clamps at the left edge without increasing distance" $ do
      let gw  = initGridWorld 10 8
          atEdge = gw { gwPlayer = (gwPlayer gw) { playerPos = Position 0 4 } }
          gw' = movePlayer DLeft atEdge
      playerPos (gwPlayer gw') `shouldBe` Position 0 4
      playerDistance (gwPlayer gw') `shouldBe` playerDistance (gwPlayer atEdge)

    it "clamps at the right edge without increasing distance" $ do
      let gw  = initGridWorld 10 8
          atEdge = gw { gwPlayer = (gwPlayer gw) { playerPos = Position 9 4 } }
          gw' = movePlayer DRight atEdge
      playerPos (gwPlayer gw') `shouldBe` Position 9 4
      playerDistance (gwPlayer gw') `shouldBe` playerDistance (gwPlayer atEdge)

    it "clamps at the top edge without increasing distance" $ do
      let gw  = initGridWorld 10 8
          atEdge = gw { gwPlayer = (gwPlayer gw) { playerPos = Position 5 0 } }
          gw' = movePlayer DUp atEdge
      playerPos (gwPlayer gw') `shouldBe` Position 5 0
      playerDistance (gwPlayer gw') `shouldBe` playerDistance (gwPlayer atEdge)

    it "clamps at the bottom edge without increasing distance" $ do
      let gw  = initGridWorld 10 8
          atEdge = gw { gwPlayer = (gwPlayer gw) { playerPos = Position 5 7 } }
          gw' = movePlayer DDown atEdge
      playerPos (gwPlayer gw') `shouldBe` Position 5 7
      playerDistance (gwPlayer gw') `shouldBe` playerDistance (gwPlayer atEdge)

  describe "gridCall \"player\" \"jump\"" $ do
    it "increments distance and the jump counter" $ do
      let gw = initGridWorld 10 8
          (gw', result) = gridCall "player" "jump" [] gw
      playerDistance (gwPlayer gw') `shouldBe` 1
      playerJumps (gwPlayer gw') `shouldBe` 1
      result `shouldBe` VUnit

    it "does not change the player's position" $ do
      let gw = initGridWorld 10 8
          (gw', _) = gridCall "player" "jump" [] gw
      playerPos (gwPlayer gw') `shouldBe` playerPos (gwPlayer gw)

    it "levels up every third jump" $ do
      let gw = initGridWorld 10 8
          jumpN n g = foldr (\_ acc -> fst (gridCall "player" "jump" [] acc)) g [1 .. n]
          afterTwo   = jumpN 2 gw
          afterThree = jumpN 3 gw
      playerLevel (gwPlayer afterTwo) `shouldBe` 1
      playerLevel (gwPlayer afterThree) `shouldBe` 2

    it "levels up again on the sixth jump" $ do
      let gw = initGridWorld 10 8
          jumpN n g = foldr (\_ acc -> fst (gridCall "player" "jump" [] acc)) g [1 .. n]
          afterSix = jumpN 6 gw
      playerLevel (gwPlayer afterSix) `shouldBe` 3

  describe "gridCall \"enemy\" \"patrol\"" $ do
    it "moves the enemy right from the left edge" $ do
      let gw = initGridWorld 10 8
          (gw', _) = gridCall "enemy" "patrol" [] gw
      enemyPos (gwEnemy gw') `shouldBe` Position 1 0
      enemyPatrolDir (gwEnemy gw') `shouldBe` DRight

    it "reverses direction upon reaching the right edge" $ do
      let gw = initGridWorld 10 8
          atRightEdge = gw { gwEnemy = (gwEnemy gw) { enemyPos = Position 9 0, enemyPatrolDir = DRight } }
          (gw', _) = gridCall "enemy" "patrol" [] atRightEdge
      enemyPatrolDir (gwEnemy gw') `shouldBe` DLeft
      enemyPos (gwEnemy gw') `shouldBe` Position 8 0

    it "reverses direction upon reaching the left edge" $ do
      let gw = initGridWorld 10 8
          atLeftEdge = gw { gwEnemy = (gwEnemy gw) { enemyPos = Position 0 0, enemyPatrolDir = DLeft } }
          (gw', _) = gridCall "enemy" "patrol" [] atLeftEdge
      enemyPatrolDir (gwEnemy gw') `shouldBe` DRight
      enemyPos (gwEnemy gw') `shouldBe` Position 1 0

  describe "gridCall \"enemy\" \"attack\"" $ do
    it "produces no world-state change" $ do
      let gw = initGridWorld 10 8
          (gw', result) = gridCall "enemy" "attack" [] gw
      gw' `shouldBe` gw
      result `shouldBe` VUnit

  describe "gridCall \"npc\" \"say\"" $ do
    it "records the spoken line" $ do
      let gw = initGridWorld 10 8
          (gw', result) = gridCall "npc" "say" [VStr "hello there"] gw
      npcLastLine (gwNpc gw') `shouldBe` Just "hello there"
      result `shouldBe` VUnit

    it "overwrites a previous line with the newest one" $ do
      let gw = initGridWorld 10 8
          (gw1, _) = gridCall "npc" "say" [VStr "first"] gw
          (gw2, _) = gridCall "npc" "say" [VStr "second"] gw1
      npcLastLine (gwNpc gw2) `shouldBe` Just "second"

    it "leaves the npc unchanged when called with the wrong argument shape" $ do
      let gw = initGridWorld 10 8
          (gw', result) = gridCall "npc" "say" [] gw
      npcLastLine (gwNpc gw') `shouldBe` Nothing
      result `shouldBe` VUnit

  describe "gridCall on unrecognized targets" $ do
    it "leaves the world unchanged for an unknown object" $ do
      let gw = initGridWorld 10 8
          (gw', result) = gridCall "ghost" "boo" [] gw
      gw' `shouldBe` gw
      result `shouldBe` VUnit

    it "leaves the world unchanged for an unknown method on a known object" $ do
      let gw = initGridWorld 10 8
          (gw', result) = gridCall "player" "fly" [] gw
      gw' `shouldBe` gw
      result `shouldBe` VUnit

  describe "gridLoad" $ do
    it "reads the player's current distance" $ do
      let gw = initGridWorld 10 8
          (gw', _) = gridCall "player" "jump" [] gw
      gridLoad "player" "distance" gw' `shouldBe` Just (VInt 1)

    it "reads the player's current level" $ do
      let gw = initGridWorld 10 8
      gridLoad "player" "level" gw `shouldBe` Just (VInt 1)

    it "returns Nothing for a field that isn't exposed" $ do
      let gw = initGridWorld 10 8
      gridLoad "player" "jumps" gw `shouldBe` Nothing

    it "returns Nothing for an unrecognized object" $ do
      let gw = initGridWorld 10 8
      gridLoad "enemy" "position" gw `shouldBe` Nothing