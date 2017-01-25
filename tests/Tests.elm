module Tests exposing (..)

import Decode

import Test exposing (..)
import Expect

decoding : Test
decoding =
    describe "Decode RDAP history"
        [ test "Placeholder" <|
            \() ->
                Expect.fail "tests not implemented"
        ]

all : Test
all =
    describe "Main test suite"
        [ decoding
        ]
