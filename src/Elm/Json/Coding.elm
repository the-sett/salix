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
import Elm.Json.NamedRef as NamedRef exposing (NamedRefError, NamedRefGen)
import Enum exposing (Enum)
import Errors exposing (Error, ErrorBuilder)
import L1 exposing (Basic(..), Container(..), Declarable(..), Field, PropSpec(..), Properties, Property(..), Restricted(..), Type(..))
import L2 exposing (L2, RefChecked)
import L3 exposing (DefaultProperties, L3, L3Error(..), ProcessorImpl, PropertiesAPI)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Naming
import ResultME exposing (ResultME)


{-| The errors that can occurr whilst generating JSON codings.
-}
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


{-| The error building for `JsonCodingError`s.
-}
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


{-| The L3 processor implementation for JSON coding.
-}
processorImpl : ProcessorImpl pos JsonCodingError
processorImpl =
    { name = "Elm.Json.Coding"
    , defaults = defaultProperties
    , check = check
    , buildError = errorBuilder
    }


{-| An enumeration of the possible JSON codings that can be generated.

Set this on a `Declarable` to generate a coding for it.

-}
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


{-| Generates a JSON coding for a `Declarable`.
-}
coding :
    PropertiesAPI pos
    -> L2 pos
    -> String
    -> Declarable pos RefChecked
    -> ResultME JsonCodingError FunGen
coding propertiesApi model name decl =
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
                        Encode.encoder (encoderNamed propertiesApi model) name decl
                            |> Ok

                    Just "Decoder" ->
                        Decode.decoder (decoderNamed propertiesApi model) name decl
                            |> Ok

                    Just "MinibillCodec" ->
                        Codec.codec name decl
                            |> Ok

                    _ ->
                        NoCodingSpecified name
                            |> ResultME.error
            )


{-| Generates a JSON coding for a set of fields (which may be part of a product).
-}
partialCoding :
    PropertiesAPI pos
    -> L2 pos
    -> String
    -> String
    -> Nonempty (Field pos RefChecked)
    -> ResultME JsonCodingError FunGen
partialCoding propertiesApi model name codingKind fields =
    case codingKind of
        "Encoder" ->
            Encode.partialEncoder (encoderNamed propertiesApi model) name fields
                |> Ok

        "Decoder" ->
            Decode.partialDecoder (decoderNamed propertiesApi model) name fields
                |> Ok

        "MinibillCodec" ->
            NoCodingSpecified name
                |> ResultME.error

        _ ->
            NoCodingSpecified name
                |> ResultME.error



-- Referencing to coding functions for named declarations.


decoderNamed : PropertiesAPI pos -> L2 pos -> NamedRefGen
decoderNamed propertiesApi model named =
    buildNamedRefGen propertiesApi model codingKindToDecoder named


encoderNamed : PropertiesAPI pos -> L2 pos -> NamedRefGen
encoderNamed propertiesApi model named =
    buildNamedRefGen propertiesApi model codingKindToEncoder named


codecNamed : PropertiesAPI pos -> L2 pos -> NamedRefGen
codecNamed propertiesApi model named =
    buildNamedRefGen propertiesApi model codingKindToCodec named


buildNamedRefGen : PropertiesAPI pos -> L2 pos -> (String -> String -> ResultME NamedRefError Expression) -> NamedRefGen
buildNamedRefGen propertiesApi model codingExprFn named =
    Dict.get named model
        |> Maybe.map (getCodingKindProp propertiesApi)
        |> Maybe.withDefault (L3.DerefDeclMissing named |> NamedRef.L3Error |> ResultME.error)
        |> ResultME.map (Maybe.map (codingExprFn named) >> Maybe.withDefault (NamedRef.NoCodingKindDefined named |> ResultME.error))
        |> ResultME.flatten


codingKindToDecoder : String -> String -> ResultME NamedRefError Expression
codingKindToDecoder named codingKind =
    case codingKind of
        "Decoder" ->
            CG.fun (Naming.safeCCL (named ++ "Decoder"))
                |> Ok

        "MinibillCodec" ->
            CG.apply
                [ CG.fqFun codecMod "decoder"
                , CG.val (Naming.safeCCL (named ++ "Codec"))
                ]
                |> CG.parens
                |> Ok

        _ ->
            NamedRef.NoDecoderAvailable named |> ResultME.error


codingKindToEncoder : String -> String -> ResultME NamedRefError Expression
codingKindToEncoder named codingKind =
    case codingKind of
        "Encoder" ->
            CG.fun (Naming.safeCCL (named ++ "Encoder"))
                |> Ok

        "MinibillCodec" ->
            CG.apply
                [ CG.fqFun codecMod "encoder"
                , CG.val (Naming.safeCCL (named ++ "Codec"))
                ]
                |> CG.parens
                |> Ok

        _ ->
            NamedRef.NoEncoderAvailable named |> ResultME.error


codingKindToCodec : String -> String -> ResultME NamedRefError Expression
codingKindToCodec named codingKind =
    case codingKind of
        "MinibillCodec" ->
            CG.val (Naming.safeCCL (named ++ "Codec"))
                |> Ok

        _ ->
            NamedRef.NoCodecAvailable named |> ResultME.error


getCodingKindProp : PropertiesAPI pos -> Declarable pos RefChecked -> ResultME NamedRefError (Maybe String)
getCodingKindProp propertiesApi decl =
    (propertiesApi.declarable decl).getOptionalEnumProperty jsonCodingEnum "jsonCoding"
        |> ResultME.mapError NamedRef.L3Error


codecMod : List String
codecMod =
    [ "Codec" ]
