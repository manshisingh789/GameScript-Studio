module ApiCatalogSpec (spec) where

import Data.List (nub)
import Data.Maybe (isJust)
import Test.Hspec

import ApiCatalog
import qualified HostBindings as HB
import Simulation.GridWorld (GridWorld, gridLoad, initGridWorld)

world0 :: GridWorld
world0 = initGridWorld 10 8

names :: [ApiObject] -> [String]
names os = [ qualifiedName m | o <- os, m <- aoMembers o ]

spec :: Spec
spec = describe "ApiCatalog" $ do

  it "lists exactly what HostBindings declares (nothing missing, nothing invented)" $ do
    let declared = [ o ++ "." ++ m | (o, m, _, _) <- HB.hostObjectMembers ]
                   ++ [ n | HB.GlobalFunction n _ _ <- HB.hostGlobalFunctions ]
    names apiCatalog `shouldBe` declared

  it "groups members under player, enemy, npc and the global functions" $
    map aoName apiCatalog `shouldBe` ["player", "enemy", "npc", ""]

  it "every property can really be read from the simulation world" $
    [ qualifiedName m | m <- allMembers, amKind m == KindProperty
                      , not (isJust (gridLoad (amObject m) (amName m) world0)) ] `shouldBe` []

  it "live values come from the real world" $ do
    let prop n = head [ m | m <- allMembers, qualifiedName m == n ]
    liveValue world0 (prop "player.distance") `shouldBe` Just "0"
    liveValue world0 (prop "player.level")    `shouldBe` Just "1"
    liveValue world0 (prop "player.jump")     `shouldBe` Nothing

  it "every current member has a real description (no placeholder text)" $
    [ qualifiedName m | m <- allMembers, amDoc m == fallbackDoc ] `shouldBe` []

  it "signatures read like the language" $ do
    map signature (concatMap aoMembers apiCatalog) `shouldBe`
      [ "player.distance: int", "player.level: int", "player.jump() -> void"
      , "enemy.attack() -> void", "enemy.patrol() -> void"
      , "npc.say(text: string) -> void"
      , "print(text: string) -> void", "jump() -> void", "dash() -> void" ]

  it "click-to-insert text is valid LumenScript" $
    map insertText allMembers `shouldBe`
      [ "player.distance", "player.level", "player.jump()"
      , "enemy.attack()", "enemy.patrol()"
      , "npc.say(\"text\")"
      , "print(\"text\")", "jump()", "dash()" ]

  it "search matches object or member name, case-insensitively" $ do
    names (searchApi "say")    `shouldBe` ["npc.say"]
    names (searchApi "JUMP")   `shouldBe` ["player.jump", "jump"]
    names (searchApi "enemy")  `shouldBe` ["enemy.attack", "enemy.patrol"]
    names (searchApi "  ")     `shouldBe` names apiCatalog
    names (searchApi "nothing-like-this") `shouldBe` []

  it "completion after a dot lists that object's members" $ do
    map fst (completions "player.")   `shouldBe` ["distance", "level", "jump"]
    map fst (completions "player.ju") `shouldBe` ["jump"]
    map fst (completions "npc.s")     `shouldBe` ["say"]
    completions "nobody.x"            `shouldBe` []

  it "completion of a bare word offers objects and global functions" $ do
    map fst (completions "pl")            `shouldBe` ["player"]
    map fst (completions "let x = e")     `shouldBe` ["enemy"]
    map fst (completions "j")             `shouldBe` ["jump"]
    map fst (completions "print(pla")     `shouldBe` ["player"]
    completions ""                        `shouldBe` []

  it "completion details carry the signature" $
    completions "player.ju" `shouldBe` [("jump", "player.jump() -> void")]

  it "object names are unique" $
    let os = [ aoName o | o <- apiCatalog ] in os `shouldBe` nub os