module DemoSpec (spec) where

import Test.Hspec
import Token
import Lexer

spec :: Spec
spec = describe "Grammar Coverage Test" $ do
  it "successfully tokenizes the full demo script" $ do
    let script = unlines
          [ "# Player attributes"
          , "player.jumps = 2"
          , "player.can_dash = true"
          , ""
          , "# Double jump"
          , "on key_press \"space\":"
          , "    if player.jumps > 0:"
          , "        player.jump()"
          , "        player.jumps = player.jumps - 1"
          , ""
          , "# Reset jumps on ground touch"
          , "on collision \"player\" \"ground\":"
          , "    player.jumps = 2"
          , "    player.can_dash = true"
          , ""
          , "# Dash move"
          , "on key_press \"shift\":"
          , "    if player.can_dash:"
          , "        player.dash()"
          , "        player.can_dash = false"
          , ""
          , "# Enemy state machine"
          , "enemy.state = \"patrol\""
          , ""
          , "on update:"
          , "    if enemy.state == \"patrol\":"
          , "        enemy.patrol()"
          , "        if player.distance_to(enemy) < 10:"
          , "            enemy.state = \"chase\""
          , "    elif enemy.state == \"chase\":"
          , "        enemy.move_towards(player)"
          , "        if player.distance_to(enemy) < 2:"
          , "            enemy.state = \"attack\""
          , "        elif player.distance_to(enemy) > 15:"
          , "            enemy.state = \"patrol\""
          , "    elif enemy.state == \"attack\":"
          , "        enemy.attack(player)"
          , "        if player.distance_to(enemy) > 2:"
          , "            enemy.state = \"chase\""
          , ""
          , "# Simple dialogue tree"
          , "npc.dialogue = \"start\""
          , ""
          , "on interact \"npc\":"
          , "    if npc.dialogue == \"start\":"
          , "        npc.say(\"Hello, traveler! Are you a warrior or a mage?\")"
          , "        player.choice(\"I'm a warrior.\", \"warrior_path\")"
          , "        player.choice(\"I'm a mage.\", \"mage_path\")"
          , "    elif npc.dialogue == \"warrior_path\":"
          , "        npc.say(\"A warrior! Excellent. The world needs more heroes.\")"
          , "        npc.dialogue = \"end\""
          , "    elif npc.dialogue == \"mage_path\":"
          , "        npc.say(\"A mage! The arcane arts are a path of great power.\")"
          , "        npc.dialogue = \"end\""
          ]
    let tokens = lexer script
    -- For now, we'll just check that the lexer doesn't produce any errors.
    -- A more thorough test would compare the entire token stream.
    let hasError = any isErrorToken tokens
    hasError `shouldBe` False

isErrorToken :: Token -> Bool
isErrorToken (Token (TError _) _) = True
isErrorToken _ = False