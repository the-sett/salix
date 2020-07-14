module Elm.Json.Coding exposing
    ( processorImpl
    , JsonCodingError, errorBuilder
    , jsonCodingEnum
    , coding, partialCoding
    )

{-| Elm.Json.Coding is a code generator for JSON encoders, decoders and miniBill/elm-codec
style codecs. The 'codec' property when defined on a declaration will signifiy which type
of json coding is required. The 'codec' property is also used to know how to generate
references to needed json coders where data models are nested.


# The L3 processor implementation.

@docs processorImpl
@docs JsonCodingError, errorBuilder


# Properties

@docs jsonCodingEnum


# The code generation functions.

@docs coding, partialCoding

-}

import Dict exposing (Dict)
import Elm.CodeGen as CG exposing (Expression)
import Elm.FunDecl as FunDecl exposing (FunDecl, FunGen)
import Elm.Json.Decode as Decode
import Elm.Json.Encode as Encode
import Elm.Json.MinibillCodec as Codec
import Enum exposing (Enum)
import Errors exposing (Error, ErrorBuilder)
import L1 exposing (Basic(..), Container(..), Declarable(..), Field, PropSpec(..), Properties, Property(..), Restricted(..), Type(..))
import L2 exposing (L2, RefChecked)
import L3 exposing (DefaultProperties, L3, L3Error(..), ProcessorImpl, PropertiesAPI)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Naming
import ResultME exposing (ResultME)


type JsonCodingError
    = L3Error L3.L3Error
    | NoCodingSpecified String


errorCatalogue =
    Dict.fromList
        [ ( 701
          , { title = "No Coding Specified"
            , body = "The `jsonCodec` property was not defined on []{arg|key=name } so I don't know what kind of coding to generate."
            }
          )
        ]


errorBuilder : ErrorBuilder pos JsonCodingError
errorBuilder posFn err =
    case err of
        L3Error l3error ->
            L3.errorBuilder posFn l3error

        NoCodingSpecified name ->
            Errors.lookupError errorCatalogue
                701
                (Dict.fromList [ ( "name", name ) ])
                []


check : L3 pos -> ResultME err (L3 pos)
check l3 =
    l3 |> Ok


processorImpl : ProcessorImpl pos JsonCodingError
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
    -> ResultME JsonCodingError FunGen
coding propertiesApi name decl =
    (propertiesApi.declarable decl).getOptionalEnumProperty jsonCodingEnum "jsonCoding"
        |> ResultME.mapError L3Error
        |> ResultME.andThen
            (\maybeCodingKind ->
                -- let
                --     _ =
                --         Debug.log "coding" { name = name, codingKind = maybeCodingKind }
                -- in
                case maybeCodingKind of
                    Just "Encoder" ->
                        Encode.encoder Encode.defaultEncoderOptions name decl
                            |> Ok

                    Just "Decoder" ->
                        Decode.decoder Decode.defaultDecoderOptions name decl
                            |> Ok

                    Just "MinibillCodec" ->
                        Codec.codec name decl
                            |> Ok

                    _ ->
                        NoCodingSpecified name
                            |> ResultME.error
            )


partialCoding :
    PropertiesAPI pos
    -> String
    -> String
    -> Nonempty (Field pos RefChecked)
    -> ResultME JsonCodingError FunGen
partialCoding propertiesApi name codingKind fields =
    case codingKind of
        "Encoder" ->
            Encode.partialEncoder Encode.defaultEncoderOptions name fields
                |> Ok

        "Decoder" ->
            Decode.partialDecoder Decode.defaultDecoderOptions name fields
                |> Ok

        "MinibillCodec" ->
            NoCodingSpecified name
                |> ResultME.error

        _ ->
            NoCodingSpecified name
                |> ResultME.error


decoderNamed : PropertiesAPI pos -> L2 pos -> String -> ResultME String Expression
decoderNamed propertiesApi model named =
    -- Dict.get named model
    --     |> Maybe.map
    --         (\decl ->
    --             (propertiesApi.declarable decl).getOptionalEnumProperty jsonCodingEnum "jsonCoding"
    --                 |> ResultME.map
    --                     (Maybe.map
    --                         (\codingKind ->
    --                             case codingKind of
    --                                 "Decoder" ->
    --                                     CG.fun (Naming.safeCCL (named ++ "Decoder"))
    --
    --                                 "MinibillCodec" ->
    --                                     CG.apply
    --                                         [ CG.fqFun codecMod "decoder"
    --                                         , CG.val (Naming.safeCCL (named ++ "Codec"))
    --                                         ]
    --                                         |> CG.parens
    --                         )
    --                     )
    --         )
    --     |> Maybe.withDefault (Ok CG.unit)
    Ok CG.unit


encoderNamed : PropertiesAPI pos -> L2 pos -> String -> Expression
encoderNamed propertiesApi model named =
    -- case options.namedTypeEncoder of
    --     AssumeCodec ->
    --         CG.apply
    --             [ CG.fqFun codecMod "encoder"
    --             , CG.val (Naming.safeCCL (named ++ "Codec"))
    --             ]
    --             |> CG.parens
    --
    --     AssumeEncoder ->
    --         CG.fun (Naming.safeCCL (named ++ "Encoder"))
    CG.unit


codecMod : List String
codecMod =
    [ "Codec" ]
