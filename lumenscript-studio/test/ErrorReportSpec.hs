module ErrorReportSpec (spec) where

import Data.List (isInfixOf, elemIndex)
import Test.Hspec

import ErrorReport
import Lexer (lexer)

outcome :: String -> Maybe String -> [String] -> Bool -> PipelineOutcome
outcome src perr serrs ok = PipelineOutcome src (lexer src) perr serrs ok

stagesOf :: PipelineReport -> [StageStatus]
stagesOf = map snd . prStages

-- column of the first '^' / of a given character in a rendered line
caretCol :: String -> Maybe Int
caretCol = elemIndex '^'

spec :: Spec
spec = describe "ErrorReport" $ do

  -- The exact parser error seen in the GUI: "Expected TColon but got TBraceL"
  it "parse error: stage indicator, position, message, why and hint" $ do
    let src = unlines [ "let score = 10", "on key_press SPACE: {", "  if score > 5 {", "    player.jump()", "  }", "}" ]
        perr = "ParseError \"Expected TColon but got TBraceL\" (Position {line = 3, column = 16})"
        r = buildReport (outcome src (Just perr) [] False)
    stagesOf r `shouldBe` [Passed, Failed, Skipped, Skipped]
    summaryLine r `shouldBe` "Lexing \10003  Parsing \10007  Semantic \8212  Bytecode \8212"
    case prDiagnostics r of
      [d] -> do
        dgStage d   `shouldBe` StParse
        dgLine d    `shouldBe` Just 3
        dgColumn d  `shouldBe` Just 16
        dgMessage d `shouldBe` "Expected `:` but found `{`"
        dgLength d  `shouldBe` 1
        (dgHint d `shouldSatisfy` ("if x > 1: {" `isInfixOf`))
        (dgWhy d `shouldSatisfy` ("needed `:`" `isInfixOf`))
      ds -> expectationFailure ("expected one diagnostic, got " ++ show (length ds))

  it "snippet: the caret sits exactly under the offending character" $ do
    let src = unlines [ "let score = 10", "on key_press SPACE: {", "  if score > 5 {", "    player.jump()", "  }", "}" ]
        perr = "ParseError \"Expected TColon but got TBraceL\" (Position {line = 3, column = 16})"
        d = firstDiag (buildReport (outcome src (Just perr) [] False))
        snip = renderSnippet src d
        errLine = head [ t | (ErrorLine, t) <- snip ]
        caret   = head [ t | (Caret, t) <- snip ]
    errLine `shouldBe` "> 3 |   if score > 5 {"
    caretCol caret `shouldBe` elemIndex '{' errLine
    length snip `shouldBe` 4            -- previous line, error line, caret, next line

  -- The exact semantic error seen in the GUI
  it "semantic error: UndefinedMember is located and explained" $ do
    let src = unlines [ "let score = 10", "on key_press SPACE: {", "  player.say(\"hi\")", "}" ]
        serr = "UndefinedMember {errBase = \"player\", errName = \"say\", errPos = Position {line = 3, column = 10}}"
        r = buildReport (outcome src Nothing [serr] False)
    stagesOf r `shouldBe` [Passed, Passed, Failed, Skipped]
    case prDiagnostics r of
      [d] -> do
        dgStage d   `shouldBe` StSemantic
        dgMessage d `shouldBe` "`player.say` does not exist"
        dgLine d    `shouldBe` Just 3
        dgColumn d  `shouldBe` Just 10
        dgLength d  `shouldBe` 3                       -- covers the word `say`
        (dgWhy d `shouldSatisfy` ("no member called `say`" `isInfixOf`))
        (dgHint d `shouldSatisfy` ("HostBindings.hs" `isInfixOf`))
        let snip = renderSnippet src d
            errLine = head [ t | (ErrorLine, t) <- snip ]
            caret   = head [ t | (Caret, t) <- snip ]
        caretCol caret `shouldBe` (elemIndex 's' (drop 10 errLine) `plus` 10)
      ds -> expectationFailure ("expected one diagnostic, got " ++ show (length ds))

  it "lexing error: parsing is reported as skipped (no cascade noise)" $ do
    let src = "let x = 5 @\n"
        r = buildReport (outcome src (Just "ParseError \"Expected something\" (Position {line = 1, column = 11})") [] False)
    stagesOf r `shouldBe` [Failed, Skipped, Skipped, Skipped]
    case prDiagnostics r of
      [d] -> do
        dgStage d   `shouldBe` StLex
        dgMessage d `shouldBe` "Unexpected `@`"
        dgLine d    `shouldBe` Just 1
        dgColumn d  `shouldBe` Just 11
      ds -> expectationFailure ("expected one diagnostic, got " ++ show (length ds))

  it "success: every stage passes and there is nothing to show" $ do
    let r = buildReport (outcome "let x = 5\n" Nothing [] True)
    stagesOf r `shouldBe` [Passed, Passed, Passed, Passed]
    prDiagnostics r `shouldBe` []
    summaryLine r `shouldBe` "Lexing \10003  Parsing \10003  Semantic \10003  Bytecode \10003"

  it "an error kind it has never seen still gets a position and a readable message" $ do
    let src = "let x = 1 + \"a\"\n"
        serr = "TypeMismatch {errExpected = \"Int\", errActual = \"String\", errPos = Position {line = 1, column = 13}}"
        d = firstDiag (buildReport (outcome src Nothing [serr] False))
    dgLine d   `shouldBe` Just 1
    dgColumn d `shouldBe` Just 13
    (dgMessage d `shouldSatisfy` ("type mismatch" `isInfixOf`))
    (dgMessage d `shouldSatisfy` ("errExpected = Int" `isInfixOf`))

  it "an error without any position does not crash and has no snippet" $ do
    let serr = "SomethingOdd"
        d = firstDiag (buildReport (outcome "let x = 1\n" Nothing [serr] False))
    dgLine d `shouldBe` Nothing
    renderSnippet "let x = 1\n" d `shouldBe` []

  it "several semantic errors are all reported, in order" $ do
    let src = unlines [ "let a = 1", "b = 2", "c = 3" ]
        e1 = "UndefinedVariable {errName = \"b\", errPos = Position {line = 2, column = 1}}"
        e2 = "UndefinedVariable {errName = \"c\", errPos = Position {line = 3, column = 1}}"
        ds = prDiagnostics (buildReport (outcome src Nothing [e1, e2] False))
    map dgLine ds `shouldBe` [Just 2, Just 3]
    map dgMessage ds `shouldBe` [ "`b` is not defined", "`c` is not defined" ]

  it "strings containing commas inside an error are read correctly" $ do
    let serr = "UndefinedMember {errBase = \"pl,ayer\", errName = \"x\", errPos = Position {line = 1, column = 1}}"
        d = firstDiag (buildReport (outcome "x\n" Nothing [serr] False))
    dgMessage d `shouldBe` "`pl,ayer.x` does not exist"

  it "token names are shown the way you would write them" $ do
    humanizeToken "TBraceL"      `shouldBe` "`{`"
    humanizeToken "TIdent \"x\"" `shouldBe` "name \"x\""
    humanizeToken "TEOF"         `shouldBe` "end of file"
  where
    firstDiag = head . prDiagnostics
    plus (Just a) b = Just (a + b)
    plus Nothing _  = Nothing