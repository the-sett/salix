module NamedRef exposing (..)

import L2 exposing (L2, RefChecked)
import L3 exposing (PropertiesAPI)


decoderNamed : PropertiesAPI -> L2 pos RefChecked -> String -> Expression
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


encoderNamed : PropertiesAPI -> L2 pos RefChecked -> String -> Expression
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
