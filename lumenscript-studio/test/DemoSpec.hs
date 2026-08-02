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
      map tokenType tokens `shouldBe` [TNewline, TIdent "player", TDot, TIdent "jumps", TAssign, TInt 2, TNewline, TIdent "player", TDot, TIdent "can_dash", TAssign, TBool True, TNewline, TNewline, TKwOn, TKwKeyPress, TString "space", TColon, TNewline, TKwIf, TIdent "player", TDot, TIdent "jumps", TGt, TInt 0, TColon, TNewline, TIdent "player", TDot, TIdent "jump", TLParen, TRParen, TNewline, TIdent "player", TDot, TIdent "jumps", TAssign, TIdent "player", TDot, TIdent "jumps", TMinus, TInt 1, TNewline, TNewline, TKwOn, TKwCollision, TString "player", TString "ground", TColon, TNewline, TIdent "player", TDot, TIdent "jumps", TAssign, TInt 2, TNewline, TIdent "player", TDot, TIdent "can_dash", TAssign, TBool True, TNewline, TNewline, TKwOn, TKwKeyPress, TString "shift", TColon, TNewline, TKwIf, TIdent "player", TDot, TIdent "can_dash", TColon, TNewline, TIdent "player", TDot, TIdent "dash", TLParen, TRParen, TNewline, TIdent "player", TDot, TIdent "can_dash", TAssign, TBool False, TEOF]

  it "lexes enemy_ai.lum correctly" $ do
    withFile "demos/enemy_ai.lum" ReadMode $ \handle -> do
      contents <- hGetContents handle
      let tokens = lexer contents
      map tokenType tokens `shouldBe` [TNewline, TIdent "enemy", TDot, TIdent "state", TAssign, TString "patrol", TNewline, TKwOn, TKwUpdate, TColon, TNewline, TKwIf, TIdent "enemy", TDot, TIdent "state", TEq, TString "patrol", TColon, TNewline, TIdent "enemy", TDot, TIdent "patrol", TLParen, TRParen, TNewline, TKwIf, TIdent "player", TDot, TIdent "distance_to", TLParen, TIdent "enemy", TRParen, TLt, TInt 10, TColon, TNewline, TIdent "enemy", TDot, TIdent "state", TAssign, TString "chase", TNewline, TKwElif, TIdent "enemy", TDot, TIdent "state", TEq, TString "chase", TColon, TNewline, TIdent "enemy", TDot, TIdent "move_towards", TLParen, TIdent "player", TRParen, TNewline, TKwIf, TIdent "player", TDot, TIdent "distance_to", TLParen, TIdent "enemy", TRParen, TLt, TInt 2, TColon, TNewline, TIdent "enemy", TDot, TIdent "state", TAssign, TString "attack", TNewline, TKwElif, TIdent "player", TDot, TIdent "distance_to", TLParen, TIdent "enemy", TRParen, TGt, TInt 15, TColon, TNewline, TIdent "enemy", TDot, TIdent "state", TAssign, TString "patrol", TNewline, TKwElif, TIdent "enemy", TDot, TIdent "state", TEq, TString "attack", TColon, TNewline, TIdent "enemy", TDot, TIdent "attack", TLParen, TIdent "player", TRParen, TNewline, TKwIf, TIdent "player", TDot, TIdent "distance_to", TLParen, TIdent "enemy", TRParen, TGt, TInt 2, TColon, TNewline, TIdent "enemy", TDot, TIdent "state", TAssign, TString "chase", TEOF]

  it "lexes dialogue_tree.lum correctly" $ do
    withFile "demos/dialogue_tree.lum" ReadMode $ \handle -> do
      contents <- hGetContents handle
      let tokens = lexer contents
      map tokenType tokens `shouldBe` [TNewline, TIdent "npc", TDot, TIdent "dialogue", TAssign, TString "start", TNewline, TKwOn, TKwInteract, TString "npc", TColon, TNewline, TKwIf, TIdent "npc", TDot, TIdent "dialogue", TEq, TString "start", TColon, TNewline, TIdent "npc", TDot, TIdent "say", TLParen, TString "Hello, traveler! Are you a warrior or a mage?", TRParen, TNewline, TIdent "player", TDot, TIdent "choice", TLParen, TString "I'm a warrior.", TComma, TString "warrior_path", TRParen, TNewline, TIdent "player", TDot, TIdent "choice", TLParen, TString "I'm a mage.", TComma, TString "mage_path", TRParen, TNewline, TKwElif, TIdent "npc", TDot, TIdent "dialogue", TEq, TString "warrior_path", TColon, TNewline, TIdent "npc", TDot, TIdent "say", TLParen, TString "A warrior! Excellent. The world needs more heroes.", TRParen, TNewline, TIdent "npc", TDot, TIdent "dialogue", TAssign, TString "end", TNewline, TKwElif, TIdent "npc", TDot, TIdent "dialogue", TEq, TString "mage_path", TColon, TNewline, TIdent "npc", TDot, TIdent "say", TLParen, TString "A mage! The arcane arts are a path of great power.", TRParen, TNewline, TIdent "npc", TDot, TIdent "dialogue", TAssign, TString "end", TEOF]