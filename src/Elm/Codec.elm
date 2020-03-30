module Elm.Codec exposing (codec, codecAsLetDecl)

{-| Elm code generation for Codecs using `minibill/elm-codec` from L2 models.

@docs codec, codecAsLetDecl

-}

import Elm.CodeGen as CG exposing (Comment, Declaration, DocComment, Expression, Import, LetDeclaration, Linkage, Pattern, TypeAnnotation)
import Elm.FunDecl as FunDecl exposing (FunDecl)
import L1 exposing (Basic(..), Container(..), Declarable(..), Restricted(..), Type(..))
import L2 exposing (RefChecked(..))
import List.Nonempty
import Maybe.Extra
import Naming
import Set exposing (Set)
import Templates.Helper as Util



--== Decoders and Encoders


{-| Generates a Codec for a type declaration.
-}
codec : String -> Declarable pos RefChecked -> ( Declaration, Linkage )
codec name decl =
    codecAsFunDecl name decl
        |> Tuple.mapFirst funDeclAsTopLevel


{-| Generates a Codec for a type declaration.
-}
codecAsLetDecl : String -> Declarable pos RefChecked -> ( LetDeclaration, Linkage )
codecAsLetDecl name decl =
    codecAsFunDecl name decl
        |> Tuple.mapFirst funDeclAsLetDecl


codecAsFunDecl : String -> Declarable pos RefChecked -> ( FunDecl, Linkage )
codecAsFunDecl name decl =
    case decl of
        DAlias _ _ l1Type ->
            typeAliasCodec name l1Type

        DSum _ _ constructors ->
            customTypeCodec name (List.Nonempty.toList constructors)

        DEnum _ _ labels ->
            enumCodec name (List.Nonempty.toList labels)

        DRestricted _ _ res ->
            restrictedCodec name res


{-| Generates a Codec for an L1 type alias.
-}
typeAliasCodec : String -> Type pos RefChecked -> ( FunDecl, Linkage )
typeAliasCodec name l1Type =
    let
        codecFnName =
            Naming.safeCCL (name ++ "Codec")

        typeName =
            Naming.safeCCU name

        sig =
            CG.typed "Codec" [ CG.typed typeName [] ]

        impl =
            codecNamedType name l1Type

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Codec for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
        |> CG.addExposing (CG.funExpose codecFnName)
    )


{-| Generates a Codec for an L1 sum type.
-}
customTypeCodec : String -> List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> ( FunDecl, Linkage )
customTypeCodec name constructors =
    let
        codecFnName =
            Naming.safeCCL (name ++ "Codec")

        typeName =
            Naming.safeCCU name

        sig =
            CG.typed "Codec" [ CG.typed typeName [] ]

        impl =
            codecCustomType constructors

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Codec for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
        |> CG.addExposing (CG.funExpose codecFnName)
    )


enumCodec : String -> List String -> ( FunDecl, Linkage )
enumCodec name constructors =
    let
        codecFnName =
            Naming.safeCCL (name ++ "Codec")

        typeName =
            Naming.safeCCU name

        enumName =
            Naming.safeCCL name

        sig =
            CG.typed "Codec" [ CG.typed typeName [] ]

        impl =
            CG.apply
                [ CG.fqFun codecMod "build"
                , CG.parens (CG.apply [ CG.fqFun enumMod "encoder", CG.val enumName ])
                , CG.parens (CG.apply [ CG.fqFun enumMod "decoder", CG.val enumName ])
                ]

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Codec for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose codecFnName)
    )


restrictedCodec : String -> Restricted -> ( FunDecl, Linkage )
restrictedCodec name _ =
    let
        codecFnName =
            Naming.safeCCL (name ++ "Codec")

        typeName =
            Naming.safeCCU name

        enumName =
            Naming.safeCCL name

        sig =
            CG.typed "Codec" [ CG.typed typeName [] ]

        impl =
            CG.apply
                [ CG.fqFun codecMod "build"
                , CG.parens (CG.apply [ CG.fqFun refinedMod "encoder", CG.val enumName ])
                , CG.parens (CG.apply [ CG.fqFun refinedMod "decoder", CG.val enumName ])
                ]

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Codec for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose codecFnName)
    )


codecCustomType : List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> Expression
codecCustomType constructors =
    let
        codecVariant name args =
            List.foldr
                (\( _, l1Type, _ ) accum -> codecType l1Type :: accum)
                [ Naming.safeCCU name |> CG.fun
                , Naming.safeCCU name |> CG.string
                , codecFn ("variant" ++ String.fromInt (List.length args))
                ]
                args
                |> List.reverse
                |> CG.apply
    in
    List.foldr (\( name, consArgs ) accum -> codecVariant name consArgs :: accum)
        [ CG.apply [ codecFn "buildCustom" ] ]
        constructors
        |> CG.pipe
            (CG.apply
                [ codecFn "custom"
                , codecMatchFn constructors
                ]
            )


codecMatchFn : List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> Expression
codecMatchFn constructors =
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


{-| Generates a Codec for an L1 type that has been named as an alias.
-}
codecNamedType : String -> Type pos RefChecked -> Expression
codecNamedType name l1Type =
    case l1Type of
        TUnit _ _ ->
            codecUnit

        TBasic _ _ basic ->
            codecType l1Type

        TNamed _ _ named _ ->
            CG.string "codecNamedType_TNamed"

        TProduct _ _ fields ->
            codecNamedProduct name (List.Nonempty.toList fields)

        TEmptyProduct _ _ ->
            codecNamedProduct name []

        TContainer _ _ container ->
            codecType l1Type

        TFunction _ _ arg res ->
            CG.unit


{-| Generates a Codec for an L1 type.
-}
codecType : Type pos RefChecked -> Expression
codecType l1Type =
    case l1Type of
        TBasic _ _ basic ->
            codecBasic basic

        TNamed _ _ named _ ->
            codecNamed named

        TProduct _ _ fields ->
            codecProduct (List.Nonempty.toList fields)

        TContainer _ _ container ->
            codecContainer container

        _ ->
            CG.unit


{-| Generates a field codec for a named field with an L1 type.
-}
codecTypeField : String -> Type pos RefChecked -> Expression
codecTypeField name l1Type =
    case l1Type of
        TUnit _ _ ->
            codecUnit |> codecField name

        TBasic _ _ basic ->
            codecBasic basic
                |> codecField name

        TNamed _ _ named _ ->
            codecNamed named
                |> codecField name

        TProduct _ _ fields ->
            codecProduct (List.Nonempty.toList fields)
                |> codecField name

        TEmptyProduct _ _ ->
            codecProduct []
                |> codecField name

        TContainer _ _ container ->
            codecContainerField name container

        TFunction _ _ arg res ->
            CG.unit


{-| Generates a codec for unit.

Decodes `()`, and encodes to JSON `null`.

-}
codecUnit =
    CG.apply
        [ codecFn "constant"
        , CG.unit
        ]


{-| Generates a codec for a basic L1 type.
-}
codecBasic : Basic -> Expression
codecBasic basic =
    case basic of
        BBool ->
            codecFn "bool"

        BInt ->
            codecFn "int"

        BReal ->
            codecFn "float"

        BString ->
            codecFn "string"


codecNamed named =
    CG.fun (Naming.safeCCL (named ++ "Codec"))


codecContainer : Container pos RefChecked -> Expression
codecContainer container =
    case container of
        CList l1Type ->
            CG.apply [ codecFn "list", codecType l1Type ]
                |> CG.parens

        CSet l1Type ->
            CG.apply [ codecFn "set", codecType l1Type ]
                |> CG.parens

        CDict l1keyType l1valType ->
            codecDict l1keyType l1valType

        COptional l1Type ->
            CG.apply [ codecFn "maybe", codecType l1Type ]
                |> CG.parens


codecDict : Type pos RefChecked -> Type pos RefChecked -> Expression
codecDict l1keyType l1valType =
    case l1keyType of
        TNamed _ _ name (RcRestricted basic) ->
            CG.apply
                [ codecFn "build"
                , CG.apply
                    [ CG.fqFun refinedMod "dictEncoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ codecFn "encoder", codecType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                , CG.apply
                    [ CG.fqFun refinedMod "dictDecoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ codecFn "decoder", codecType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                ]

        TNamed _ _ name RcEnum ->
            CG.apply
                [ codecFn "build"
                , CG.apply
                    [ CG.fqFun enumMod "dictEncoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ codecFn "encoder", codecType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                , CG.apply
                    [ CG.fqFun enumMod "dictDecoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ codecFn "decoder", codecType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                ]

        _ ->
            CG.apply [ codecFn "dict", codecType l1valType ]
                |> CG.parens


{-| Generates a codec for an L1 product type that has been named as an alias.
The alias name is also the constructor function for the type.
-}
codecNamedProduct : String -> List ( String, Type pos RefChecked, L1.Properties ) -> Expression
codecNamedProduct name fields =
    let
        typeName =
            Naming.safeCCU name

        impl =
            codecFields fields
                |> CG.pipe
                    (CG.apply
                        [ codecFn "object"
                        , CG.fun typeName
                        ]
                    )
    in
    impl


{-| Generates a codec for an L1 product type that does not have a name.
Without a name there is no constructor function for the product, so it must be
built explicitly by its fields.
-}
codecProduct : List ( String, Type pos RefChecked, L1.Properties ) -> Expression
codecProduct fields =
    CG.string "codecProduct"


{-| Generates a field codec for an L1 container type. The 'optional' type is mapped
onto `Maybe` and makes use of `Codec.optionalField`.
-}
codecContainerField : String -> Container pos RefChecked -> Expression
codecContainerField name container =
    case container of
        CList l1Type ->
            CG.apply [ codecFn "list", codecType l1Type ]
                |> CG.parens
                |> codecField name

        CSet l1Type ->
            CG.apply [ codecFn "set", codecType l1Type ]
                |> CG.parens
                |> codecField name

        CDict l1keyType l1valType ->
            codecDict l1keyType l1valType
                |> codecField name

        COptional l1Type ->
            codecType l1Type
                |> codecOptionalField name



--== Helper Functions


{-| Outputs codecs for a list of fields and terminates the list with `Codec.buildObject`.
Helper function useful when building record codecs.
-}
codecFields : List ( String, Type pos RefChecked, L1.Properties ) -> List Expression
codecFields fields =
    List.foldr (\( fieldName, l1Type, _ ) accum -> codecTypeField fieldName l1Type :: accum)
        [ CG.apply
            [ codecFn "buildObject"
            ]
        ]
        fields


{-| Helper function for building field codecs.
-}
codecField : String -> Expression -> Expression
codecField name expr =
    CG.apply
        [ codecFn "field"
        , CG.string name
        , CG.accessFun ("." ++ Naming.safeCCL name)
        , expr
        ]


{-| Helper function for building optional field codecs.
-}
codecOptionalField : String -> Expression -> Expression
codecOptionalField name expr =
    CG.apply
        [ codecFn "optionalField"
        , CG.string name
        , CG.accessFun ("." ++ Naming.safeCCL name)
        , expr
        ]


dummyFn : String -> ( Declaration, Linkage )
dummyFn name =
    ( CG.funDecl Nothing Nothing name [] CG.unit, CG.emptyLinkage )


codecMod : List String
codecMod =
    [ "Codec" ]


decodeMod : List String
decodeMod =
    [ "Json", "Decode" ]


encodeMod : List String
encodeMod =
    [ "Json", "Encode" ]


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


resultMod : List String
resultMod =
    [ "Result" ]


codecFn : String -> Expression
codecFn =
    CG.fqFun codecMod


codecImport : Import
codecImport =
    CG.importStmt codecMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Codec" ])


decodeImport : Import
decodeImport =
    CG.importStmt decodeMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Decoder" ])


encodeImport : Import
encodeImport =
    CG.importStmt encodeMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Value" ])


setImport : Import
setImport =
    CG.importStmt [ "Set" ] Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Set" ])


dictImport : Import
dictImport =
    CG.importStmt [ "Dict" ] Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Dict" ])


enumImport : Import
enumImport =
    CG.importStmt enumMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Enum" ])


dictEnumImport : Import
dictEnumImport =
    CG.importStmt dictEnumMod Nothing Nothing


refinedImport : Import
refinedImport =
    CG.importStmt refinedMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Refined" ])


dictRefinedImport : Import
dictRefinedImport =
    CG.importStmt dictRefinedMod Nothing Nothing


guardedImportExposing : List String -> Import
guardedImportExposing exposings =
    CG.importStmt refinedMod Nothing (Just <| CG.exposeExplicit (List.map CG.typeOrAliasExpose exposings))
