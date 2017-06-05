module TestSuite exposing (all)

import Test exposing (..)
import Expect
import String

all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \() ->
                Expect.equal 10 (3 + 7)
        , test "This test should fail" <|
            \() ->
                Expect.fail "failed as expected!"
        ]
