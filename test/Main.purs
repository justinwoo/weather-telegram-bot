module Test.Main where

import Prelude
import Data.String.Regex as Regex
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.String.Regex (Regex)
import Main (getRegex)
import Test.Unit (suite, test)
import Test.Unit.Assert (expectFailure, equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

testPattern :: forall e.
  Regex ->
  String ->
  Aff e Unit
testPattern pattern string =
  equal true $ Regex.test pattern string

main :: forall e.
  Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    | e
    )
    Unit
main = runTest do
  suite "getRegex" do
    test "does not crash the program" do
      let testPattern' = testPattern getRegex
      testPattern' "get"
    test "tests get 'get' but with case insensitivity" do
      let testPattern' = testPattern getRegex
      testPattern' "get"
      testPattern' "Get"
      testPattern' "GET"
      expectFailure "aget shouldn't work" $ testPattern' "aget"
      expectFailure "geta shouldn't work" $ testPattern' "geta"
