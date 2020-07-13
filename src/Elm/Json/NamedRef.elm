module Elm.Json.NamedRef exposing (..)

import Dict exposing (Dict)
import Elm.CodeGen as CG exposing (Expression)
import L2 exposing (L2, RefChecked)
import L3 exposing (PropertiesAPI)
import ResultME exposing (ResultME)


decoderNamed : PropertiesAPI pos -> L2 pos -> String -> Expression
decoderNamed propertiesApi model named =
    case options.namedTypeDecoder of
        AssumeCodec ->
            CG.apply
                [ CG.fqFun codecMod "decoder"
                , CG.val (Naming.safeCCL (named ++ "Codec"))
                ]
                |> CG.parens

        AssumeDecoder ->
            CG.fun (Naming.safeCCL (named ++ "Decoder"))


encoderNamed : PropertiesAPI pos -> L2 pos -> String -> Expression
encoderNamed propertiesApi model named =
    case options.namedTypeEncoder of
        AssumeCodec ->
            CG.apply
                [ CG.fqFun codecMod "encoder"
                , CG.val (Naming.safeCCL (named ++ "Codec"))
                ]
                |> CG.parens

        AssumeEncoder ->
            CG.fun (Naming.safeCCL (named ++ "Encoder"))
