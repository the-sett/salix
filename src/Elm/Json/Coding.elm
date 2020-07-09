module Elm.Json.Coding exposing (..)

{-| Elm.Json.Coding is a code generator for JSON encoders, decoders and miniBill/elm-codec
style codecs. The 'codec' property when defined on a declaration will signifiy which type
of json coding is required. The 'codec' property is also used to know how to generate
references to needed json coders where data models are nested.
-}

import Dict exposing (Dict)
import Elm.FunDecl as FunDecl exposing (FunDecl, FunGen)
import Elm.Json.Decode as Decode
import Elm.Json.Encode as Encode
import Elm.Json.MinibillCodec as Codec
import Enum exposing (Enum)
import Errors exposing (Error, ErrorBuilder)
import L1 exposing (Basic(..), Container(..), Declarable(..), Field, PropSpec(..), Properties, Property(..), Restricted(..), Type(..))
import L2 exposing (RefChecked)
import L3 exposing (DefaultProperties, L3, L3Error(..), ProcessorImpl, PropertiesAPI)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import ResultME exposing (ResultME)


type JsonCodecError
    = L3Error L3.L3Error
    | NoCodingSpecified


errorCatalogue =
    Dict.fromList
        [ ( 701
          , { title = "No Coding Specified"
            , body = "The `jsonCodec` property was not defined so I don't know what kind of coding to generate."
            }
          )
        ]


errorBuilder : ErrorBuilder pos JsonCodecError
errorBuilder posFn err =
    case err of
        L3Error l3error ->
            L3.errorBuilder posFn l3error

        NoCodingSpecified ->
            Errors.lookupError errorCatalogue 701 Dict.empty []


check : L3 pos -> ResultME err (L3 pos)
check l3 =
    l3 |> Ok


processorImpl : ProcessorImpl pos JsonCodecError
processorImpl =
    { name = "Elm.Json.Coding"
    , defaults = defaultProperties
    , check = check
    , buildError = errorBuilder
    }


jsonCodingEnum : Enum String
jsonCodingEnum =
    Enum.define
        [ "Encoder"
        , "Decoder"
        , "MinibillCodec"
        ]
        identity


defaultProperties : DefaultProperties
defaultProperties =
    { top = L1.defineProperties [] []
    , alias =
        L1.defineProperties
            [ ( "jsonCoding", PSOptional (PSEnum jsonCodingEnum) )
            ]
            []
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


coding :
    PropertiesAPI pos
    -> String
    -> Declarable pos RefChecked
    -> ResultME JsonCodecError FunGen
coding propertiesApi name decl =
    NoCodingSpecified
        |> ResultME.error


partialCoding :
    PropertiesAPI pos
    -> String
    -> String
    -> Nonempty (Field pos RefChecked)
    -> ResultME JsonCodecError FunGen
partialCoding propertiesApi name codingKind fields =
    case codingKind of
        "Encoder" ->
            Encode.partialEncoder Encode.defaultEncoderOptions name fields
                |> Ok

        "Decoder" ->
            Decode.partialDecoder Decode.defaultDecoderOptions name fields
                |> Ok

        "MinibillCodec" ->
            NoCodingSpecified
                |> ResultME.error

        _ ->
            NoCodingSpecified
                |> ResultME.error
