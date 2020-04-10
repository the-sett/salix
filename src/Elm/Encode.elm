module Elm.Encode exposing (encoder, partialEncoder)

{-| Elm code generation for Encoders using `elm/json` from L2 models.

@docs encoder, partialEncoder

-}

import Elm.CodeGen as CG exposing (Comment, Declaration, DocComment, Expression, Import, LetDeclaration, Linkage, Pattern, TypeAnnotation)
import Elm.FunDecl as FunDecl exposing (FunDecl, FunGen)
import Elm.Helper as Util
import L1 exposing (Basic(..), Container(..), Declarable(..), Field, Restricted(..), Type(..))
import L2 exposing (RefChecked(..))
import List.Nonempty
import Maybe.Extra
import Naming
import Set exposing (Set)



--== Options for generating encoders


{-| When generating an encoder for something with a named type nested in it,
we assume that an encoder for that is also being generated, and simply call it.

It may have been generated as a codec, or as an encoder, and this allows this
assumption to be captured as an option.

-}
type AssumedEncoderForNamedType
    = AssumeCodec
    | AssumeEncoder


type alias EncoderOptions =
    { namedTypeEncoder : AssumedEncoderForNamedType
    }


defaultEncoderOptions : EncoderOptions
defaultEncoderOptions =
    { namedTypeEncoder = AssumeEncoder
    }



--== Encoders


{-| Generates a Encoder for a type declaration.
-}
encoder : EncoderOptions -> String -> Declarable pos RefChecked -> FunGen
encoder options name decl =
    case decl of
        DAlias _ _ l1Type ->
            typeAliasEncoder options name l1Type

        DSum _ _ constructors ->
            customTypeEncoder options name (List.Nonempty.toList constructors)

        DEnum _ _ labels ->
            enumEncoder name (List.Nonempty.toList labels)

        DRestricted _ _ res ->
            restrictedEncoder name res


partialEncoder : EncoderOptions -> String -> List (Field pos RefChecked) -> FunGen
partialEncoder options name fields =
    let
        encodeFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        sig =
            CG.typed "Encoder" [ CG.typed typeName [] ]

        impl =
            encoderNamedProduct options name fields

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Encoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        encodeFnName
        [ CG.varPattern "val" ]
        impl
    , CG.emptyLinkage
        |> CG.addImport encodeImport
        |> CG.addExposing (CG.funExpose encodeFnName)
    )


{-| Generates a Encoder for an L1 type alias.
-}
typeAliasEncoder : EncoderOptions -> String -> Type pos RefChecked -> ( FunDecl, Linkage )
typeAliasEncoder options name l1Type =
    let
        encodeFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        sig =
            CG.typed "Encoder" [ CG.typed typeName [] ]

        impl =
            encoderNamedType options name l1Type

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Encoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        encodeFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport encodeImport
        |> CG.addExposing (CG.funExpose encodeFnName)
    )


{-| Generates a Encoder for an L1 sum type.
-}
customTypeEncoder : EncoderOptions -> String -> List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> ( FunDecl, Linkage )
customTypeEncoder options name constructors =
    let
        encodeFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        sig =
            CG.typed "Encoder" [ CG.typed typeName [] ]

        impl =
            encoderCustomType options constructors

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Encoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        encodeFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport encodeImport
        |> CG.addExposing (CG.funExpose encodeFnName)
    )


enumEncoder : String -> List String -> ( FunDecl, Linkage )
enumEncoder name constructors =
    let
        encodeFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        enumName =
            Naming.safeCCL name

        sig =
            CG.typed "Encoder" [ CG.typed typeName [] ]

        impl =
            CG.apply
                [ CG.fqFun encodeMod "build"
                , CG.parens (CG.apply [ CG.fqFun enumMod "encoder", CG.val enumName ])
                , CG.parens (CG.apply [ CG.fqFun enumMod "decoder", CG.val enumName ])
                ]

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Encoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        encodeFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport encodeImport
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose encodeFnName)
    )


restrictedEncoder : String -> Restricted -> ( FunDecl, Linkage )
restrictedEncoder name _ =
    let
        encodeFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        enumName =
            Naming.safeCCL name

        sig =
            CG.typed "Encoder" [ CG.typed typeName [] ]

        impl =
            CG.apply
                [ CG.fqFun encodeMod "build"
                , CG.parens (CG.apply [ CG.fqFun refinedMod "encoder", CG.val enumName ])
                , CG.parens (CG.apply [ CG.fqFun refinedMod "decoder", CG.val enumName ])
                ]

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Encoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        encodeFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport encodeImport
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose encodeFnName)
    )


encoderCustomType : EncoderOptions -> List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> Expression
encoderCustomType options constructors =
    let
        encoderVariant name args =
            List.foldr
                (\( _, l1Type, _ ) accum -> encoderType options l1Type :: accum)
                [ Naming.safeCCU name |> CG.fun
                , Naming.safeCCU name |> CG.string
                , encodeFn ("variant" ++ String.fromInt (List.length args))
                ]
                args
                |> List.reverse
                |> CG.apply
    in
    List.foldr (\( name, consArgs ) accum -> encoderVariant name consArgs :: accum)
        [ CG.apply [ encodeFn "buildCustom" ] ]
        constructors
        |> CG.pipe
            (CG.apply
                [ encodeFn "custom"
                , encoderMatchFn constructors
                ]
            )


encoderMatchFn : List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> Expression
encoderMatchFn constructors =
    let
        consFnName name =
            "f" ++ Naming.safeCCL name

        args =
            List.foldr (\( name, _ ) accum -> (consFnName name |> CG.varPattern) :: accum)
                [ CG.varPattern "value" ]
                constructors

        consPattern ( name, consArgs ) =
            ( CG.namedPattern (Naming.safeCCU name)
                (List.map (\( argName, _, _ ) -> CG.varPattern argName) consArgs)
            , List.foldr (\( argName, _, _ ) accum -> CG.val argName :: accum)
                [ consFnName name |> CG.fun ]
                consArgs
                |> List.reverse
                |> CG.apply
            )

        matchFnBody =
            List.map consPattern constructors
                |> CG.caseExpr (CG.val "value")
    in
    CG.lambda args matchFnBody


{-| Generates a Encoder for an L1 type that has been named as an alias.
-}
encoderNamedType : EncoderOptions -> String -> Type pos RefChecked -> Expression
encoderNamedType options name l1Type =
    case l1Type of
        TUnit _ _ ->
            encoderUnit

        TBasic _ _ basic ->
            encoderType options l1Type

        TNamed _ _ named _ ->
            CG.string "encoderNamedType_TNamed"

        TProduct _ _ fields ->
            encoderNamedProduct options name (List.Nonempty.toList fields)

        TEmptyProduct _ _ ->
            encoderNamedProduct options name []

        TContainer _ _ container ->
            encoderType options l1Type

        TFunction _ _ arg res ->
            CG.unit


{-| Generates a Encoder for an L1 type.
-}
encoderType : EncoderOptions -> Type pos RefChecked -> Expression
encoderType options l1Type =
    case l1Type of
        TBasic _ _ basic ->
            encoderBasic basic

        TNamed _ _ named _ ->
            encoderNamed options named

        TProduct _ _ fields ->
            encoderProduct (List.Nonempty.toList fields)

        TContainer _ _ container ->
            encoderContainer options container

        _ ->
            CG.unit


{-| Generates a field encoder for a named field with an L1 type.
-}
encoderTypeField : EncoderOptions -> String -> Type pos RefChecked -> Expression
encoderTypeField options name l1Type =
    case l1Type of
        TUnit _ _ ->
            encoderUnit |> encoderField options name

        TBasic _ _ basic ->
            encoderBasic basic
                |> encoderField options name

        TNamed _ _ named _ ->
            encoderNamed options named
                |> encoderField options name

        TProduct _ _ fields ->
            encoderProduct (List.Nonempty.toList fields)
                |> encoderField options name

        TEmptyProduct _ _ ->
            encoderProduct []
                |> encoderField options name

        TContainer _ _ container ->
            encoderContainerField options name container

        TFunction _ _ arg res ->
            CG.unit


{-| Generates a encoder for unit.

Decodes `()`, and encodes to JSON `null`.

-}
encoderUnit =
    CG.apply
        [ encodeFn "constant"
        , CG.unit
        ]


{-| Generates a encoder for a basic L1 type.
-}
encoderBasic : Basic -> Expression
encoderBasic basic =
    case basic of
        BBool ->
            encodeFn "bool"

        BInt ->
            encodeFn "int"

        BReal ->
            encodeFn "float"

        BString ->
            encodeFn "string"


encoderNamed : EncoderOptions -> String -> Expression
encoderNamed options named =
    case options.namedTypeEncoder of
        AssumeCodec ->
            CG.apply
                [ CG.fqFun codecMod "encoder"
                , CG.val (Naming.safeCCL (named ++ "Codec"))
                ]

        AssumeEncoder ->
            CG.fun (Naming.safeCCL (named ++ "Encoder"))


encoderContainer : EncoderOptions -> Container pos RefChecked -> Expression
encoderContainer options container =
    case container of
        CList l1Type ->
            CG.apply [ encodeFn "list", encoderType options l1Type ]
                |> CG.parens

        CSet l1Type ->
            CG.apply [ encodeFn "set", encoderType options l1Type ]
                |> CG.parens

        CDict l1keyType l1valType ->
            encoderDict options l1keyType l1valType

        COptional l1Type ->
            CG.apply [ encodeFn "maybe", encoderType options l1Type ]
                |> CG.parens


encoderDict : EncoderOptions -> Type pos RefChecked -> Type pos RefChecked -> Expression
encoderDict options l1keyType l1valType =
    case l1keyType of
        TNamed _ _ name (RcRestricted basic) ->
            CG.apply
                [ encodeFn "build"
                , CG.apply
                    [ CG.fqFun refinedMod "dictEncoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ encodeFn "encoder", encoderType options l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                , CG.apply
                    [ CG.fqFun refinedMod "dictDecoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ encodeFn "decoder", encoderType options l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                ]

        TNamed _ _ name RcEnum ->
            CG.apply
                [ encodeFn "build"
                , CG.apply
                    [ CG.fqFun enumMod "dictEncoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ encodeFn "encoder", encoderType options l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                , CG.apply
                    [ CG.fqFun enumMod "dictDecoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ encodeFn "decoder", encoderType options l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                ]

        _ ->
            CG.apply [ encodeFn "dict", encoderType options l1valType ]
                |> CG.parens


{-| Generates a encoder for an L1 product type that has been named as an alias.
The alias name is also the constructor function for the type.
-}
encoderNamedProduct : EncoderOptions -> String -> List ( String, Type pos RefChecked, L1.Properties ) -> Expression
encoderNamedProduct options name fields =
    let
        typeName =
            Naming.safeCCU name

        impl =
            CG.apply
                [ encodeFn "object"
                , encoderFields options fields |> CG.list
                ]
    in
    impl


{-| Generates a encoder for an L1 product type that does not have a name.
Without a name there is no constructor function for the product, so it must be
built explicitly by its fields.
-}
encoderProduct : List ( String, Type pos RefChecked, L1.Properties ) -> Expression
encoderProduct fields =
    CG.string "encoderProduct"


{-| Generates a field encoder for an L1 container type. The 'optional' type is mapped
onto `Maybe` and makes use of `Encoder.optionalField`.
-}
encoderContainerField : EncoderOptions -> String -> Container pos RefChecked -> Expression
encoderContainerField options name container =
    case container of
        CList l1Type ->
            CG.apply [ encodeFn "list", encoderType options l1Type ]
                |> CG.parens
                |> encoderField options name

        CSet l1Type ->
            CG.apply [ encodeFn "set", encoderType options l1Type ]
                |> CG.parens
                |> encoderField options name

        CDict l1keyType l1valType ->
            encoderDict options l1keyType l1valType
                |> encoderField options name

        COptional l1Type ->
            encoderType options l1Type
                |> encoderOptionalField options name



--== Helper Functions


{-| Outputs encoders for a list of fields and terminates the list with `Encoder.buildObject`.
Helper function useful when building record encoders.
-}
encoderFields : EncoderOptions -> List ( String, Type pos RefChecked, L1.Properties ) -> List Expression
encoderFields options fields =
    List.foldr (\( fieldName, l1Type, _ ) accum -> encoderTypeField options fieldName l1Type :: accum)
        []
        fields


{-| Helper function for building field encoders.
-}
encoderField : EncoderOptions -> String -> Expression -> Expression
encoderField options name expr =
    CG.tuple
        [ CG.string name
        , CG.apply [ expr, CG.access (CG.val "val") (Naming.safeCCL name) ]
        ]


{-| Helper function for building optional field encoders.
-}
encoderOptionalField : EncoderOptions -> String -> Expression -> Expression
encoderOptionalField options name expr =
    CG.tuple
        [ CG.string name
        , CG.apply [ expr, CG.access (CG.val "val") (Naming.safeCCL name) ]
        ]


codecMod : List String
codecMod =
    [ "Codec" ]


encodeMod : List String
encodeMod =
    [ "Json", "Encode" ]


dictEnumMod : List String
dictEnumMod =
    [ "Dict", "Enum" ]


enumMod : List String
enumMod =
    [ "Enum" ]


dictRefinedMod : List String
dictRefinedMod =
    [ "Dict", "Refined" ]


refinedMod : List String
refinedMod =
    [ "Refined" ]


encodeFn : String -> Expression
encodeFn =
    CG.fqFun encodeMod


encodeImport : Import
encodeImport =
    CG.importStmt encodeMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Value" ])


enumImport : Import
enumImport =
    CG.importStmt enumMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Enum" ])
