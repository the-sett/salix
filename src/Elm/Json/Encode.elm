module Elm.Json.Encode exposing (encoder, partialEncoder)

{-| Elm code generation for Encoders using `elm/json` from L2 models.

@docs encoder, partialEncoder

-}

import Elm.CodeGen as CG exposing (Comment, Declaration, DocComment, Expression, Import, LetDeclaration, Linkage, Pattern, TypeAnnotation)
import Elm.FunDecl as FunDecl exposing (FunDecl, FunGen)
import Elm.Helper as Util
import Elm.Json.NamedRef as NamedRef exposing (NamedRefError, NamedRefGen)
import L1 exposing (Basic(..), Container(..), Declarable(..), Field, Restricted(..), Type(..))
import L2 exposing (RefChecked(..))
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Maybe.Extra
import Naming
import Set exposing (Set)



--== Encoders


{-| Generates an Encoder for a type declaration.
-}
encoder : NamedRefGen -> String -> Declarable pos RefChecked -> FunGen
encoder options name decl =
    case decl of
        DAlias _ _ l1Type ->
            typeAliasEncoder options name l1Type

        DSum _ _ constructors ->
            customTypeEncoder options name (Nonempty.toList constructors)

        DEnum _ _ labels ->
            enumEncoder name (Nonempty.toList labels)

        DRestricted _ _ res ->
            restrictedEncoder name res


{-| Generates an Encoder for a list of fields (which may be part of a record).
-}
partialEncoder : NamedRefGen -> String -> Nonempty (Field pos RefChecked) -> FunGen
partialEncoder options name fields =
    let
        encodeFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        sig =
            CG.funAnn (CG.typed typeName []) (CG.typed "Value" [])

        impl =
            encoderNamedProduct options name (Nonempty.toList fields)

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
        |> CG.addImport encodeOptionalImport
        |> CG.addExposing (CG.funExpose encodeFnName)
    )


{-| Generates a Encoder for an L1 type alias.
-}
typeAliasEncoder : NamedRefGen -> String -> Type pos RefChecked -> ( FunDecl, Linkage )
typeAliasEncoder options name l1Type =
    let
        encodeFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        sig =
            CG.funAnn (CG.typed typeName []) (CG.typed "Value" [])

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
customTypeEncoder : NamedRefGen -> String -> List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> ( FunDecl, Linkage )
customTypeEncoder options name constructors =
    let
        encodeFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        sig =
            CG.funAnn (CG.typed typeName []) (CG.typed "Value" [])

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
            CG.funAnn (CG.typed typeName []) (CG.typed "Value" [])

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
            CG.funAnn (CG.typed typeName []) (CG.typed "Value" [])

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


encoderCustomType : NamedRefGen -> List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> Expression
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
encoderNamedType : NamedRefGen -> String -> Type pos RefChecked -> Expression
encoderNamedType options name l1Type =
    case l1Type of
        TUnit _ _ ->
            encoderUnit

        TBasic _ _ basic ->
            encoderType options l1Type

        TNamed _ _ named _ ->
            CG.string "encoderNamedType_TNamed"

        TProduct _ _ fields ->
            encoderNamedProduct options name (Nonempty.toList fields)

        TEmptyProduct _ _ ->
            encoderNamedProduct options name []

        TContainer _ _ container ->
            encoderType options l1Type

        TFunction _ _ arg res ->
            CG.unit


{-| Generates a Encoder for an L1 type.
-}
encoderType : NamedRefGen -> Type pos RefChecked -> Expression
encoderType options l1Type =
    case l1Type of
        TBasic _ _ basic ->
            encoderBasic basic

        TNamed _ _ named _ ->
            encoderNamed options named

        TProduct _ _ fields ->
            encoderProduct (Nonempty.toList fields)

        TContainer _ _ container ->
            encoderContainer options container

        _ ->
            CG.unit


{-| Generates a field encoder for a named field with an L1 type.
-}
encoderTypeField : NamedRefGen -> String -> Type pos RefChecked -> Expression
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
            encoderProduct (Nonempty.toList fields)
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


encoderNamed : NamedRefGen -> String -> Expression
encoderNamed refgen named =
    refgen named
        |> Result.withDefault (CG.fun (Naming.safeCCL (named ++ "Encoder")))


encoderContainer : NamedRefGen -> Container pos RefChecked -> Expression
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


encoderDict : NamedRefGen -> Type pos RefChecked -> Type pos RefChecked -> Expression
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
encoderNamedProduct : NamedRefGen -> String -> List ( String, Type pos RefChecked, L1.Properties ) -> Expression
encoderNamedProduct options name fields =
    let
        typeName =
            Naming.safeCCU name

        impl =
            CG.pipe
                (encoderFields options fields |> CG.list)
                [ CG.fqFun encodeOptionalMod "objectMaySkip"
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
encoderContainerField : NamedRefGen -> String -> Container pos RefChecked -> Expression
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


{-| Outputs encoders for a list of fields and terminates the list with `Encoder.buildObject`.
Helper function useful when building record encoders.
-}
encoderFields : NamedRefGen -> List ( String, Type pos RefChecked, L1.Properties ) -> List Expression
encoderFields options fields =
    List.foldr (\( fieldName, l1Type, _ ) accum -> encoderTypeField options fieldName l1Type :: accum)
        []
        fields


{-| Helper function for building field encoders.
-}
encoderField : NamedRefGen -> String -> Expression -> Expression
encoderField options name expr =
    CG.applyBinOp
        (CG.tuple
            [ CG.string name
            , CG.access (CG.val "val") (Naming.safeCCL name)
            ]
        )
        CG.piper
        (CG.apply [ CG.fqFun encodeOptionalMod "field", expr ])


{-| Helper function for building optional field encoders.
-}
encoderOptionalField : NamedRefGen -> String -> Expression -> Expression
encoderOptionalField options name expr =
    CG.applyBinOp
        (CG.tuple
            [ CG.string name
            , CG.access (CG.val "val") (Naming.safeCCL name)
            ]
        )
        CG.piper
        (CG.apply [ CG.fqFun encodeOptionalMod "optionalField", expr ])



--== Helper Functions


codecMod : List String
codecMod =
    [ "Codec" ]


encodeMod : List String
encodeMod =
    [ "Json", "Encode" ]


encodeOptionalMod : List String
encodeOptionalMod =
    [ "EncodeOpt" ]


dictEnumMod : List String
dictEnumMod =
    [ "Dict", "Enum" ]


enumMod : List String
enumMod =
    [ "Enum" ]


maybeMod : List String
maybeMod =
    [ "Maybe" ]


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


encodeOptionalImport : Import
encodeOptionalImport =
    CG.importStmt [ "Json", "Encode", "Optional" ] (Just encodeOptionalMod) Nothing


enumImport : Import
enumImport =
    CG.importStmt enumMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Enum" ])
