module SourceLinkSpec (spec) where

import qualified Data.IntSet as IS
import Test.Hspec
import SourceLink
import SourceLinkFixture

hl :: Selection -> Highlights
hl = resolve demoTable

set :: [Int] -> IS.IntSet
set = IS.fromList

spec :: Spec
spec = describe "SourceLink" $ do

  it "token 'a' lights its AST node and its LOAD_VAR" $ do
    let h = hl (SelToken 3)
    hlTokens h `shouldBe` set [3]
    hlAst h    `shouldBe` set [3]
    hlCode h   `shouldBe` set [0]

  it "cursor in the editor on 'b' behaves like clicking token b" $ do
    let off = offsetFromLineCol demoSource 1 13        -- the 'b'
    hl (SelSource (Span off off)) `shouldBe` hl (SelToken 5)

  it "selecting source range 'a + b' lights the whole expression" $ do
    let h = hl (SelSource (Span 8 13))
    hlTokens h `shouldBe` set [3, 4, 5]
    hlAst h    `shouldBe` set [2, 3, 4]
    hlCode h   `shouldBe` set [0, 1, 2]

  it "clicking AST BinOp + lights its tokens and its instructions" $ do
    let h = hl (SelAst 2)
    hlAst h    `shouldBe` set [2]
    hlTokens h `shouldBe` set [3, 4, 5]
    hlCode h   `shouldBe` set [0, 1, 2]

  it "clicking ADD points back at BinOp and the 3 tokens of 'a + b'" $ do
    let h = hl (SelCode 2)
    hlCode h   `shouldBe` set [2]
    hlAst h    `shouldBe` set [2]
    hlTokens h `shouldBe` set [3, 4, 5]
    hlSource h `shouldBe` [Span 8 13]

  it "clicking STORE x lights the whole 'let' statement" $ do
    let h = hl (SelCode 3)
    hlAst h    `shouldBe` set [1]
    hlTokens h `shouldBe` set [0 .. 6]

  it "the two x identifiers are told apart by position" $ do
    hlCode (hl (SelToken 1)) `shouldBe` set [3]   -- declaration -> STORE
    hlCode (hl (SelToken 9)) `shouldBe` set [4]   -- use -> LOAD_VAR

  it "synthetic HALT highlights only itself" $ do
    hl (SelCode 6) `shouldBe` emptyHighlights { hlCode = set [6] }

  it "a cursor touching the end of a word selects that word" $
    hl (SelSource (Span 3 3)) `shouldBe` hl (SelToken 0)

  it "a cursor over no token, and bad ids, highlight nothing" $ do
    hl (SelSource (Span 100 100)) `shouldBe` emptyHighlights
    hl (SelToken 99)              `shouldBe` emptyHighlights

  it "segments splits text into highlighted runs" $
    segments "abcdef" [Span 1 3, Span 2 4] `shouldBe`
      [(False, "a"), (True, "bcd"), (False, "ef")]