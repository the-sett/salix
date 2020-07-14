module Elm.Json.NamedRef exposing (NamedRefError(..), NamedRefGen)

import Elm.CodeGen as CG exposing (Expression)
import L3
import ResultME exposing (ResultME)


type NamedRefError
    = L3Error L3.L3Error
    | NoCodingKindDefined String
    | NoDecoderAvailable String
    | NoEncoderAvailable String
    | NoCodecAvailable String


type alias NamedRefGen =
    String -> ResultME NamedRefError Expression
