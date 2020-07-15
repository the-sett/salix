module Elm.Json.Decode exposing (decoder, partialDecoder)

{-| Elm code generation for Decoders using `elm/json` from L2 models.

@docs decoder, partialDecoder

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



--== Decoders


{-| Generates a Decoder for a type declaration.
-}
decoder : NamedRefGen -> String -> Declarable pos RefChecked -> FunGen
decoder options name decl =
    case decl of
        DAlias _ _ l1Type ->
            typeAliasDecoder options name l1Type

        DSum _ _ constructors ->
            customTypeDecoder options name (Nonempty.toList constructors)

        DEnum _ _ labels ->
            enumDecoder name (Nonempty.toList labels)

        DRestricted _ _ res ->
            restrictedDecoder name res


{-| Generates a Decoder for a list of fields (which may be part of a record).
-}
partialDecoder : NamedRefGen -> String -> Nonempty (Field pos RefChecked) -> FunGen
partialDecoder options name fields =
    let
        decodeFnName =
            Naming.safeCCL (name ++ "Decoder")

        typeName =
            Naming.safeCCU name

        sig =
            CG.typed "Decoder" [ CG.typed typeName [] ]

        impl =
            decoderNamedProduct options name fields

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Decoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        decodeFnName
        [ CG.varPattern "val" ]
        impl
    , CG.emptyLinkage
        |> CG.addImport decodeImport
        |> CG.addImport jsonDecodePipelineImport
        |> CG.addExposing (CG.funExpose decodeFnName)
    )


{-| Generates a Decoder for an L1 type alias.
-}
typeAliasDecoder : NamedRefGen -> String -> Type pos RefChecked -> ( FunDecl, Linkage )
typeAliasDecoder options name l1Type =
    let
        decodeFnName =
            Naming.safeCCL (name ++ "Decoder")

        typeName =
            Naming.safeCCU name

        sig =
            CG.typed "Decoder" [ CG.typed typeName [] ]

        impl =
            decoderNamedType options name l1Type

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Decoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        decodeFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport decodeImport
        |> CG.addExposing (CG.funExpose decodeFnName)
    )


{-| Generates a Decoder for an L1 sum type.
-}
customTypeDecoder : NamedRefGen -> String -> List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> ( FunDecl, Linkage )
customTypeDecoder options name constructors =
    let
        decodeFnName =
            Naming.safeCCL (name ++ "Decoder")

        typeName =
            Naming.safeCCU name

        sig =
            CG.typed "Decoder" [ CG.typed typeName [] ]

        impl =
            decoderCustomType options constructors

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Decoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        decodeFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport decodeImport
        |> CG.addExposing (CG.funExpose decodeFnName)
    )


enumDecoder : String -> List String -> ( FunDecl, Linkage )
enumDecoder name constructors =
    let
        decodeFnName =
            Naming.safeCCL (name ++ "Decoder")

        typeName =
            Naming.safeCCU name

        enumName =
            Naming.safeCCL name

        sig =
            CG.typed "Decoder" [ CG.typed typeName [] ]

        impl =
            CG.apply
                [ CG.fqFun decodeMod "build"
                , CG.parens (CG.apply [ CG.fqFun enumMod "decoder", CG.val enumName ])
                , CG.parens (CG.apply [ CG.fqFun enumMod "decoder", CG.val enumName ])
                ]

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Decoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        decodeFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport decodeImport
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose decodeFnName)
    )


restrictedDecoder : String -> Restricted -> ( FunDecl, Linkage )
restrictedDecoder name _ =
    let
        decodeFnName =
            Naming.safeCCL (name ++ "Decoder")

        typeName =
            Naming.safeCCU name

        enumName =
            Naming.safeCCL name

        sig =
            CG.typed "Decoder" [ CG.typed typeName [] ]

        impl =
            CG.apply
                [ CG.fqFun decodeMod "build"
                , CG.parens (CG.apply [ CG.fqFun refinedMod "decoder", CG.val enumName ])
                , CG.parens (CG.apply [ CG.fqFun refinedMod "decoder", CG.val enumName ])
                ]

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Decoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        decodeFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport decodeImport
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose decodeFnName)
    )


decoderCustomType : NamedRefGen -> List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> Expression
decoderCustomType options constructors =
    let
        decoderVariant name args =
            List.foldr
                (\( _, l1Type, _ ) accum -> decoderType options l1Type :: accum)
                [ Naming.safeCCU name |> CG.fun
                , Naming.safeCCU name |> CG.string
                , decodeFn ("variant" ++ String.fromInt (List.length args))
                ]
                args
                |> List.reverse
                |> CG.apply
    in
    List.foldr (\( name, consArgs ) accum -> decoderVariant name consArgs :: accum)
        [ CG.apply [ decodeFn "buildCustom" ] ]
        constructors
        |> CG.pipe
            (CG.apply
                [ decodeFn "custom"
                , decoderMatchFn constructors
                ]
            )


decoderMatchFn : List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> Expression
decoderMatchFn constructors =
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


{-| Generates a Decoder for an L1 type that has been named as an alias.
-}
decoderNamedType : NamedRefGen -> String -> Type pos RefChecked -> Expression
decoderNamedType options name l1Type =
    case l1Type of
        TUnit _ _ ->
            decoderUnit

        TBasic _ _ basic ->
            decoderType options l1Type

        TNamed _ _ named _ ->
            CG.string "decoderNamedType_TNamed"

        TProduct _ _ fields ->
            decoderNamedProduct options name fields

        TEmptyProduct _ _ ->
            CG.unit

        TContainer _ _ container ->
            decoderType options l1Type

        TFunction _ _ arg res ->
            CG.unit


{-| Generates a Decoder for an L1 type.
-}
decoderType : NamedRefGen -> Type pos RefChecked -> Expression
decoderType options l1Type =
    case l1Type of
        TBasic _ _ basic ->
            decoderBasic basic

        TNamed _ _ named _ ->
            decoderNamed options named

        TProduct _ _ fields ->
            decoderProduct (Nonempty.toList fields)

        TContainer _ _ container ->
            decoderContainer options container

        _ ->
            CG.unit


{-| Generates a field decoder for a named field with an L1 type.
-}
decoderTypeField : NamedRefGen -> String -> Type pos RefChecked -> Expression
decoderTypeField options name l1Type =
    case l1Type of
        TUnit _ _ ->
            decoderUnit |> decoderField options name

        TBasic _ _ basic ->
            decoderBasic basic
                |> decoderField options name

        TNamed _ _ named _ ->
            decoderNamed options named
                |> decoderField options name

        TProduct _ _ fields ->
            decoderProduct (Nonempty.toList fields)
                |> decoderField options name

        TEmptyProduct _ _ ->
            decoderProduct []
                |> decoderField options name

        TContainer _ _ container ->
            decoderContainerField options name container

        TFunction _ _ arg res ->
            CG.unit


{-| Generates a decoder for unit.

Decodes `()`, and decodes to JSON `null`.

-}
decoderUnit =
    CG.apply
        [ decodeFn "constant"
        , CG.unit
        ]


{-| Generates a decoder for a basic L1 type.
-}
decoderBasic : Basic -> Expression
decoderBasic basic =
    case basic of
        BBool ->
            decodeFn "bool"

        BInt ->
            decodeFn "int"

        BReal ->
            decodeFn "float"

        BString ->
            decodeFn "string"


decoderNamed : NamedRefGen -> String -> Expression
decoderNamed options named =
    CG.fun (Naming.safeCCL (named ++ "Decoder"))


decoderContainer : NamedRefGen -> Container pos RefChecked -> Expression
decoderContainer options container =
    case container of
        CList l1Type ->
            CG.apply [ decodeFn "list", decoderType options l1Type ]
                |> CG.parens

        CSet l1Type ->
            CG.apply [ decodeFn "set", decoderType options l1Type ]
                |> CG.parens

        CDict l1keyType l1valType ->
            decoderDict options l1keyType l1valType

        COptional l1Type ->
            CG.apply [ decodeFn "maybe", decoderType options l1Type ]
                |> CG.parens


decoderDict : NamedRefGen -> Type pos RefChecked -> Type pos RefChecked -> Expression
decoderDict options l1keyType l1valType =
    case l1keyType of
        TNamed _ _ name (RcRestricted basic) ->
            CG.apply
                [ decodeFn "build"
                , CG.apply
                    [ CG.fqFun refinedMod "dictDecoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ decodeFn "decoder", decoderType options l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                , CG.apply
                    [ CG.fqFun refinedMod "dictDecoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ decodeFn "decoder", decoderType options l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                ]

        TNamed _ _ name RcEnum ->
            CG.apply
                [ decodeFn "build"
                , CG.apply
                    [ CG.fqFun enumMod "dictDecoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ decodeFn "decoder", decoderType options l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                , CG.apply
                    [ CG.fqFun enumMod "dictDecoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ decodeFn "decoder", decoderType options l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                ]

        _ ->
            CG.apply [ decodeFn "dict", decoderType options l1valType ]
                |> CG.parens


{-| Generates a decoder for an L1 product type that has been named as an alias.
The alias name is also the constructor function for the type.
-}
decoderNamedProduct : NamedRefGen -> String -> Nonempty ( String, Type pos RefChecked, L1.Properties ) -> Expression
decoderNamedProduct options name fields =
    let
        typeName =
            Naming.safeCCU name

        (Nonempty hdFieldDecoder remFieldDecoders) =
            decoderFields options fields

        impl =
            CG.pipe hdFieldDecoder remFieldDecoders
    in
    impl


{-| Generates a decoder for an L1 product type that does not have a name.
Without a name there is no constructor function for the product, so it must be
built explicitly by its fields.
-}
decoderProduct : List ( String, Type pos RefChecked, L1.Properties ) -> Expression
decoderProduct fields =
    CG.string "decoderProduct"


{-| Generates a field decoder for an L1 container type. The 'optional' type is mapped
onto `Maybe` and makes use of `Decoder.optionalField`.
-}
decoderContainerField : NamedRefGen -> String -> Container pos RefChecked -> Expression
decoderContainerField options name container =
    case container of
        CList l1Type ->
            CG.apply [ decodeFn "list", decoderType options l1Type ]
                |> CG.parens
                |> decoderField options name

        CSet l1Type ->
            CG.apply [ decodeFn "set", decoderType options l1Type ]
                |> CG.parens
                |> decoderField options name

        CDict l1keyType l1valType ->
            decoderDict options l1keyType l1valType
                |> decoderField options name

        COptional l1Type ->
            decoderType options l1Type
                |> decoderOptionalField options name


{-| Outputs decoders for a list of fields and terminates the list with `Decoder.buildObject`.
Helper function useful when building record decoders.
-}
decoderFields : NamedRefGen -> Nonempty ( String, Type pos RefChecked, L1.Properties ) -> Nonempty Expression
decoderFields options fields =
    Nonempty.map (\( fieldName, l1Type, _ ) -> decoderTypeField options fieldName l1Type)
        fields


{-| Helper function for building field decoders.
-}
decoderField : NamedRefGen -> String -> Expression -> Expression
decoderField options name expr =
    CG.apply [ CG.fqFun jsonDecodePipelineMod "required", CG.string name, expr ]


{-| Helper function for building optional field decoders.
-}
decoderOptionalField : NamedRefGen -> String -> Expression -> Expression
decoderOptionalField options name expr =
    let
        maybeDecoder =
            CG.apply [ CG.fqFun decodeMod "maybe", expr ] |> CG.parens
    in
    CG.apply [ CG.fqFun jsonDecodePipelineMod "optional", CG.string name, maybeDecoder, CG.val "Nothing" ]



--== Helper Functions


codecMod : List String
codecMod =
    [ "Codec" ]


decodeMod : List String
decodeMod =
    [ "Json", "Decode" ]


decodeOptionalMod : List String
decodeOptionalMod =
    [ "DecodeOpt" ]


jsonDecodePipelineMod : List String
jsonDecodePipelineMod =
    [ "Pipeline" ]


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


decodeFn : String -> Expression
decodeFn =
    CG.fqFun decodeMod


decodeImport : Import
decodeImport =
    CG.importStmt decodeMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Value" ])


jsonDecodePipelineImport : Import
jsonDecodePipelineImport =
    CG.importStmt [ "Json", "Decode", "Pipeline" ] (Just jsonDecodePipelineMod) Nothing


enumImport : Import
enumImport =
    CG.importStmt enumMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Enum" ])
