module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.Route as Route
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  TestUnit.suite "add" do
    TestUnit.test "1 + 1 = 2" do
      Assert.assert "1 + 1 = 2" (1 + 1 == 2)
  Route.tests
