module CheckerTest exposing (..)

import Checker exposing (..)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import L1 exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The L2 model checker given"
        [ test "an alias to a non-existant declaration reports an error." <|
            \_ ->
                let
                    alias =
                        TNamed "Missing" Unchecked |> DAlias

                    decls =
                        Dict.empty
                            |> Dict.insert "Test" alias

                    l2result =
                        check decls
                in
                Expect.err l2result
        ]
