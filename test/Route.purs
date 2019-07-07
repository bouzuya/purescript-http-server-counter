module Test.Route
  ( tests
  ) where

import Prelude

import Action as Action
import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.Request.NormalizedPath as NormalizedPath
import Route as Route
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Route" do
  TestUnit.suite "/counters" do
    let path = NormalizedPath.normalize "/counters"

    TestUnit.test "GET" do
      Assert.equal Action.CounterList (Route.route path Method.GET)

    TestUnit.test "POST" do
      Assert.equal Action.CounterCreate (Route.route path Method.POST)

    TestUnit.test "(other)" do
      Assert.equal
        (Action.MethodNotAllowed [Method.GET, Method.POST])
        (Route.route path Method.PATCH)

  TestUnit.suite "/counters/{id}" do
    let path = NormalizedPath.normalize "/counters/123"

    TestUnit.test "GET" do
      Assert.equal
        (Action.CounterGet "123")
        (Route.route path Method.GET)

    TestUnit.test "DELETE" do
      Assert.equal
        (Action.CounterDelete "123")
        (Route.route path Method.DELETE)

    TestUnit.test "PATCH" do
      Assert.equal
        (Action.CounterUpdate "123")
        (Route.route path Method.PATCH)

    TestUnit.test "(other)" do
      Assert.equal
        (Action.MethodNotAllowed [Method.GET, Method.DELETE, Method.PATCH])
        (Route.route path Method.POST)

  TestUnit.suite "/" do
    let path = NormalizedPath.normalize "/"

    TestUnit.test "GET" do
      Assert.equal Action.HealthCheck (Route.route path Method.GET)

    TestUnit.test "(other)" do
      Assert.equal
        (Action.MethodNotAllowed [Method.GET])
        (Route.route path Method.PATCH)

  TestUnit.suite "(other)" do
    let path = NormalizedPath.normalize "/foo"

    TestUnit.test "GET" do
      Assert.equal Action.NotFound (Route.route path Method.GET)
