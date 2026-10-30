module LanguageManualSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec

import ApiCatalog (allMembers, qualifiedName, ApiMember(..))
import Documentation.LanguageManual

spec :: Spec
spec = describe "Engine API manual reference" $ do
  it "includes every member currently exposed by the API catalog" $ do
    let reference = sectionContent engineApiSection
    [qualifiedName member | member <- allMembers,
      not (qualifiedName member `isInfixOf` reference)] `shouldBe` []

  it "uses the catalog descriptions for every documented member" $ do
    let reference = sectionContent engineApiSection
    [qualifiedName member | member <- allMembers,
      amDoc member `notInfixOf` reference] `shouldBe` []

engineApiSection :: ManualSection
engineApiSection =
  case [ section | section <- manualSections, sectionId section == "engine-api" ] of
    [section] -> section
    _         -> error "engine-api section is missing or duplicated"

notInfixOf :: String -> String -> Bool
notInfixOf needle haystack = not (needle `isInfixOf` haystack)
