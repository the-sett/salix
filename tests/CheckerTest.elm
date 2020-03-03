module CheckerTest exposing (..)

import Checker exposing (..)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import L1 exposing (..)
import List.Nonempty exposing (Nonempty)
import Random
import Shrink
import Test exposing (..)


suite : Test
suite =
    describe "The L2 model checker given"
        [ fuzz (declarableFuzzer typeFuzzer)
            "an alias to a non-existant declaration reports an error."
          <|
            \alias ->
                let
                    decls =
                        [ ( "Test", alias ) ]

                    l2result =
                        check decls
                in
                Expect.err l2result
        ]


type alias RecursiveFuzzerConfig a =
    { maxDepth : Int
    , baseWeight : Float
    , recurseWeight : Int -> Float
    , base : Fuzzer a
    , recurse : Fuzzer a -> Fuzzer a
    }


recursiveFuzzer : RecursiveFuzzerConfig a -> Fuzzer a
recursiveFuzzer { maxDepth, baseWeight, recurseWeight, base, recurse } =
    let
        helper depth =
            if depth > maxDepth then
                base

            else
                Fuzz.frequency
                    [ ( baseWeight, base )
                    , ( recurseWeight depth, recurse (helper (depth + 1)) )
                    ]
    in
    helper 1


basicFuzzer : Fuzzer Basic
basicFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant BBool
        , Fuzz.constant BInt
        , Fuzz.constant BReal
        , Fuzz.constant BString
        ]


declarableFuzzer : Fuzzer (Type () Unchecked) -> Fuzzer (Declarable () Unchecked)
declarableFuzzer tFuzzer =
    Fuzz.oneOf
        [ Fuzz.map (DAlias () L1.emptyProperties) tFuzzer
        , Fuzz.tuple3 ( Fuzz.string, tFuzzer, Fuzz.constant L1.emptyProperties )
            |> nonEmptyList
            |> Fuzz.map2 Tuple.pair Fuzz.string
            |> Fuzz.map List.Nonempty.fromElement
            |> Fuzz.map (DSum () L1.emptyProperties)

        --  DEnum (List String)
        --  DRestricted Restricted
        ]


containerFuzzer : Fuzzer (Type () Unchecked) -> Fuzzer (Container () Unchecked)
containerFuzzer tFuzzer =
    Fuzz.oneOf
        [ Fuzz.map CList tFuzzer
        , Fuzz.map CSet tFuzzer
        , Fuzz.map2 CDict tFuzzer tFuzzer
        , Fuzz.map COptional tFuzzer
        ]


restrictedFuzzer : Fuzzer Restricted
restrictedFuzzer =
    Fuzz.oneOf
        [ RInt { min = Nothing, max = Nothing, width = Nothing }
            |> Fuzz.constant
        , RString { minLength = Nothing, maxLength = Nothing, regex = Nothing }
            |> Fuzz.constant
        ]


leafTypeFuzzer : Fuzzer (Type () Unchecked)
leafTypeFuzzer =
    Fuzz.oneOf
        [ TNamed () L1.emptyProperties "Missing" Unchecked |> Fuzz.constant
        ]


recursiveTypeFuzzer : Fuzzer (Type () Unchecked) -> Fuzzer (Type () Unchecked)
recursiveTypeFuzzer tFuzzer =
    Fuzz.oneOf
        [ Fuzz.tuple3 ( Fuzz.string, tFuzzer, Fuzz.constant L1.emptyProperties )
            |> nonempty
            |> Fuzz.map (TProduct () L1.emptyProperties)
        , Fuzz.map (TContainer () L1.emptyProperties) (containerFuzzer tFuzzer)
        , Fuzz.map2 (TFunction () L1.emptyProperties) tFuzzer tFuzzer
        ]


typeFuzzer : Fuzzer (Type () Unchecked)
typeFuzzer =
    recursiveFuzzer
        { maxDepth = 2
        , baseWeight = 0.2
        , recurseWeight = \depth -> 1 / toFloat depth
        , base = leafTypeFuzzer
        , recurse = recursiveTypeFuzzer
        }


nonempty : Fuzzer a -> Fuzzer (Nonempty a)
nonempty fuzz =
    Fuzz.map2
        (\list singleton ->
            List.Nonempty.fromElement singleton
                |> List.Nonempty.replaceTail list
        )
        (Fuzz.list fuzz)
        fuzz


nonEmptyList fuzzer =
    Fuzz.map2 (::) fuzzer (Fuzz.list fuzzer)
