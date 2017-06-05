port module Main exposing (..)

import TestSuite
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Test.Runner.Node.TestProgram
main = run emit TestSuite.all

port emit : ( String, Value ) -> Cmd msg
