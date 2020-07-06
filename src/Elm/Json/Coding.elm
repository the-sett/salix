module Elm.Json.Coding exposing (..)

{-| Elm.Json.Coding is a code generator for JSON encoders, decoders and miniBill/elm-codec
style codecs. The 'codec' property when defined on a declaration will signifiy which type
of json coding is required. The 'codec' property is also used to know how to generate
references to needed json coders where data models are nested.
-}

import Dict exposing (Dict)
import Errors exposing (Error, ErrorBuilder)
import L1 exposing (Basic(..), Container(..), Declarable(..), Field, PropSpec(..), Properties, Property(..), Restricted(..), Type(..))
import L3 exposing (DefaultProperties, L3, L3Error(..), ProcessorImpl, PropertiesAPI)
import ResultME exposing (ResultME)


type JsonCodecError
    = L3Error L3.L3Error


errorCatalogue =
    Dict.fromList
        []


errorBuilder : ErrorBuilder pos JsonCodecError
errorBuilder posFn err =
    case err of
        L3Error l3error ->
            L3.errorBuilder posFn l3error


check : L3 pos -> ResultME err (L3 pos)
check l3 =
    l3 |> Ok


processorImpl : ProcessorImpl pos JsonCodecError
processorImpl =
    { name = "Elm.Json.Codec"
    , defaults = defaultProperties
    , check = check
    , buildError = errorBuilder
    }


defaultProperties : DefaultProperties
defaultProperties =
    { top = L1.defineProperties [] []
    , alias = L1.defineProperties [] []
    , sum = L1.defineProperties [] []
    , enum = L1.defineProperties [] []
    , restricted = L1.defineProperties [] []
    , fields = L1.defineProperties [] []
    , unit = L1.defineProperties [] []
    , basic = L1.defineProperties [] []
    , named = L1.defineProperties [] []
    , product = L1.defineProperties [] []
    , emptyProduct = L1.defineProperties [] []
    , container = L1.defineProperties [] []
    , function = L1.defineProperties [] []
    }
