module DemoSpec (spec) where

import Test.Hspec
import Token
import Lexer
import System.IO

spec :: Spec
spec = describe "Demo Scripts" $ do

  it "lexes platformer.lum correctly" $ do
    withFile "demos/platformer.lum" ReadMode $ \handle -> do
      contents <- hGetContents handle
      let tokens = lexer contents
      map tokenType tokens `shouldBe` [TIdent "player", TDot, TIdent "jumps", TAssign, TInt 2, TIdent "player", TDot, TIdent "can_dash", TAssign, TBool True, TKwOn, TKwKeyPress, TString "space", TColon, TKwIf, TIdent "player", TDot, TIdent "jumps", TGt, TInt 0, TColon, TIdent "player", TDot, TIdent "jump", TLParen, TRParen, TIdent "player", TDot, TIdent "jumps", TAssign, TIdent "player", TDot, TIdent "jumps", TMinus, TInt 1, TKwOn, TKwCollision, TString "player", TString "ground", TColon, TIdent "player", TDot, TIdent "jumps", TAssign, TInt 2, TIdent "player", TDot, TIdent "can_dash", TAssign, TBool True, TKwOn, TKwKeyPress, TString "shift", TColon, TKwIf, TIdent "player", TDot, TIdent "can_dash", TColon, TIdent "player", TDot, TIdent "dash", TLParen, TRParen, TIdent "player", TDot, TIdent "can_dash", TAssign, TBool False, TEOF]

  it "lexes enemy_ai.lum correctly" $ do
    withFile "demos/enemy_ai.lum" ReadMode $ \handle -> do
      contents <- hGetContents handle
      let tokens = lexer contents
      map tokenType tokens `shouldBe` [TIdent "enemy", TDot, TIdent "state", TAssign, TString "patrol", TKwOn, TKwUpdate, TColon, TKwIf, TIdent "enemy", TDot, TIdent "state", TEq, TString "patrol", TColon, TIdent "enemy", TDot, TIdent "patrol", TLParen, TRParen, TKwIf, TIdent "player", TDot, TIdent "distance_to", TLParen, TIdent "enemy", TRParen, TLt, TInt 10, TColon, TIdent "enemy", TDot, TIdent "state", TAssign, TString "chase", TKwElif, TIdent "enemy", TDot, TIdent "state", TEq, TString "chase", TColon, TIdent "enemy", TDot, TIdent "move_towards", TLParen, TIdent "player", TRParen, TKwIf, TIdent "player", TDot, TIdent "distance_to", TLParen, TIdent "enemy", TRParen, TLt, TInt 2, TColon, TIdent "enemy", TDot, TIdent "state", TAssign, TString "attack", TKwElif, TIdent "player", TDot, TIdent "distance_to", TLParen, TIdent "enemy", TRParen, TGt, TInt 15, TColon, TIdent "enemy", TDot, TIdent "state", TAssign, TString "patrol", TKwElif, TIdent "enemy", TDot, TIdent "state", TEq, TString "attack", TColon, TIdent "enemy", TDot, TIdent "attack", TLParen, TIdent "player", TRParen, TKwIf, TIdent "player", TDot, TIdent "distance_to", TLParen, TIdent "enemy", TRParen, TGt, TInt 2, TColon, TIdent "enemy", TDot, TIdent "state", TAssign, TString "chase", TEOF]

  it "lexes dialogue_tree.lum correctly" $ do
    withFile "demos/dialogue_tree.lum" ReadMode $ \handle -> do
      contents <- hGetContents handle
      let tokens = lexer contents
      map tokenType tokens `shouldBe` [TIdent "npc", TDot, TIdent "dialogue", TAssign, TString "start", TKwOn, TKwInteract, TString "npc", TColon, TKwIf, TIdent "npc", TDot, TIdent "dialogue", TEq, TString "start", TColon, TIdent "npc", TDot, TIdent "say", TLParen, TString "Hello, traveler! Are you a warrior or a mage?", TRParen, TIdent "player", TDot, TIdent "choice", TLParen, TString "I'm a warrior.", TComma, TString "warrior_path", TRParen, TIdent "player", TDot, TIdent "choice", TLParen, TString "I'm a mage.", TComma, TString "mage_path", TRParen, TKwElif, TIdent "npc", TDot, TIdent "dialogue", TEq, TString "warrior_path", TColon, TIdent "npc", TDot, TIdent "say", TLParen, TString "A warrior! Excellent. The world needs more heroes.", TRParen, TIdent "npc", TDot, TIdent "dialogue", TAssign, TString "end", TKwElif, TIdent "npc", TDot, TIdent "dialogue", TEq, TString "mage_path", TColon, TIdent "npc", TDot, TIdent "say", TLParen, TString "A mage! The arcane arts are a path of great power.", TRParen, TIdent "npc", TDot, TIdent "dialogue", TAssign, TString "end", TEOF]