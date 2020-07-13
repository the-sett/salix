module Elm.Json.NamedRef exposing (..)

import Dict exposing (Dict)
import Elm.CodeGen as CG exposing (Expression)
import Enum exposing (Enum)
import L2 exposing (L2, RefChecked)
import L3 exposing (PropertiesAPI)
import Naming
import ResultME exposing (ResultME)


jsonCodingEnum : Enum String
jsonCodingEnum =
    Enum.define
        [ "Encoder"
        , "Decoder"
        , "MinibillCodec"
        ]
        identity


decoderNamed : PropertiesAPI pos -> L2 pos -> String -> ResultME String Expression
decoderNamed propertiesApi model named =
    -- case options.namedTypeDecoder of
    --     AssumeCodec ->
    --         CG.apply
    --             [ CG.fqFun codecMod "decoder"
    --             , CG.val (Naming.safeCCL (named ++ "Codec"))
    --             ]
    --             |> CG.parens
    --
    --     AssumeDecoder ->
    --         CG.fun (Naming.safeCCL (named ++ "Decoder"))
    Dict.get named model
        |> Maybe.map
            (\decl ->
                (propertiesApi.declarable decl).getOptionalEnumProperty jsonCodingEnum "jsonCoding"
                    |> ResultME.map
                        (Maybe.map
                            (\coding ->
                                case coding of
                                    "Decoder" ->
                                        CG.fun (Naming.safeCCL (named ++ "Decoder"))

                                    "MinibillCodec" ->
                                        CG.apply
                                            [ CG.fqFun codecMod "decoder"
                                            , CG.val (Naming.safeCCL (named ++ "Codec"))
                                            ]
                                            |> CG.parens
                            )
                        )
            )
        --|> Maybe.withDefault (ResultME.error "Name not found in model.")
        |> Maybe.withDefault (Ok CG.unit)


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
