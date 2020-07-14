module Elm.Json.NamedRef exposing (..)

import Elm.CodeGen as CG exposing (Expression)
import ResultME exposing (ResultME)


type alias NamedRefGen =
    { decoderNamed : String -> ResultME String Expression
    , encoderNamed : String -> ResultME String Expression
    }
