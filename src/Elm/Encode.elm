module Elm.Encode exposing (encoder, encoderAsLetDecl)

{-| Elm code generation for Encoders using `elm/json` from L2 models.

@docs encoder, encoderAsLetDecl

-}

import Elm.CodeGen as CG exposing (Comment, Declaration, DocComment, Expression, Import, LetDeclaration, Linkage, Pattern, TypeAnnotation)
import Elm.FunDecl as FunDecl exposing (FunDecl)
import Elm.Helper as Util
import L1 exposing (Basic(..), Container(..), Declarable(..), Restricted(..), Type(..))
import L2 exposing (RefChecked(..))
import List.Nonempty
import Maybe.Extra
import Naming
import Set exposing (Set)



--== Encoders


{-| Generates a Encoder for a type declaration.
-}
encoder : String -> Declarable pos RefChecked -> ( Declaration, Linkage )
encoder name decl =
    encoderAsFunDecl name decl
        |> Tuple.mapFirst FunDecl.funDeclAsTopLevel


{-| Generates a Encoder for a type declaration.
-}
encoderAsLetDecl : String -> Declarable pos RefChecked -> ( LetDeclaration, Linkage )
encoderAsLetDecl name decl =
    encoderAsFunDecl name decl
        |> Tuple.mapFirst FunDecl.funDeclAsLetDecl


encoderAsFunDecl : String -> Declarable pos RefChecked -> ( FunDecl, Linkage )
encoderAsFunDecl name decl =
    case decl of
        DAlias _ _ l1Type ->
            typeAliasEncoder name l1Type

        DSum _ _ constructors ->
            customTypeEncoder name (List.Nonempty.toList constructors)

        DEnum _ _ labels ->
            enumEncoder name (List.Nonempty.toList labels)

        DRestricted _ _ res ->
            restrictedEncoder name res


{-| Generates a Encoder for an L1 type alias.
-}
typeAliasEncoder : String -> Type pos RefChecked -> ( FunDecl, Linkage )
typeAliasEncoder name l1Type =
    let
        encoderFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        sig =
            CG.typed "Encoder" [ CG.typed typeName [] ]

        impl =
            encoderNamedType name l1Type

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Encoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        encoderFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport encoderImport
        |> CG.addExposing (CG.funExpose encoderFnName)
    )


{-| Generates a Encoder for an L1 sum type.
-}
customTypeEncoder : String -> List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> ( FunDecl, Linkage )
customTypeEncoder name constructors =
    let
        encoderFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        sig =
            CG.typed "Encoder" [ CG.typed typeName [] ]

        impl =
            encoderCustomType constructors

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Encoder for " ++ typeName ++ ".")
    in
    ( FunDecl
        (Just doc)
        (Just sig)
        encoderFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport encoderImport
        |> CG.addExposing (CG.funExpose encoderFnName)
    )


enumEncoder : String -> List String -> ( FunDecl, Linkage )
enumEncoder name constructors =
    let
        encoderFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        enumName =
            Naming.safeCCL name

        sig =
            CG.typed "Encoder" [ CG.typed typeName [] ]

        impl =
            CG.apply
                [ CG.fqFun encoderMod "build"
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
        encoderFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport encoderImport
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose encoderFnName)
    )


restrictedEncoder : String -> Restricted -> ( FunDecl, Linkage )
restrictedEncoder name _ =
    let
        encoderFnName =
            Naming.safeCCL (name ++ "Encoder")

        typeName =
            Naming.safeCCU name

        enumName =
            Naming.safeCCL name

        sig =
            CG.typed "Encoder" [ CG.typed typeName [] ]

        impl =
            CG.apply
                [ CG.fqFun encoderMod "build"
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
        encoderFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport encoderImport
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose encoderFnName)
    )


encoderCustomType : List ( String, List ( String, Type pos RefChecked, L1.Properties ) ) -> Expression
encoderCustomType constructors =
    let
        encoderVariant name args =
            List.foldr
                (\( _, l1Type, _ ) accum -> encoderType l1Type :: accum)
                [ Naming.safeCCU name |> CG.fun
                , Naming.safeCCU name |> CG.string
                , encoderFn ("variant" ++ String.fromInt (List.length args))
                ]
                args
                |> List.reverse
                |> CG.apply
    in
    List.foldr (\( name, consArgs ) accum -> encoderVariant name consArgs :: accum)
        [ CG.apply [ encoderFn "buildCustom" ] ]
        constructors
        |> CG.pipe
            (CG.apply
                [ encoderFn "custom"
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
encoderNamedType : String -> Type pos RefChecked -> Expression
encoderNamedType name l1Type =
    case l1Type of
        TUnit _ _ ->
            encoderUnit

        TBasic _ _ basic ->
            encoderType l1Type

        TNamed _ _ named _ ->
            CG.string "encoderNamedType_TNamed"

        TProduct _ _ fields ->
            encoderNamedProduct name (List.Nonempty.toList fields)

        TEmptyProduct _ _ ->
            encoderNamedProduct name []

        TContainer _ _ container ->
            encoderType l1Type

        TFunction _ _ arg res ->
            CG.unit


{-| Generates a Encoder for an L1 type.
-}
encoderType : Type pos RefChecked -> Expression
encoderType l1Type =
    case l1Type of
        TBasic _ _ basic ->
            encoderBasic basic

        TNamed _ _ named _ ->
            encoderNamed named

        TProduct _ _ fields ->
            encoderProduct (List.Nonempty.toList fields)

        TContainer _ _ container ->
            encoderContainer container

        _ ->
            CG.unit


{-| Generates a field encoder for a named field with an L1 type.
-}
encoderTypeField : String -> Type pos RefChecked -> Expression
encoderTypeField name l1Type =
    case l1Type of
        TUnit _ _ ->
            encoderUnit |> encoderField name

        TBasic _ _ basic ->
            encoderBasic basic
                |> encoderField name

        TNamed _ _ named _ ->
            encoderNamed named
                |> encoderField name

        TProduct _ _ fields ->
            encoderProduct (List.Nonempty.toList fields)
                |> encoderField name

        TEmptyProduct _ _ ->
            encoderProduct []
                |> encoderField name

        TContainer _ _ container ->
            encoderContainerField name container

        TFunction _ _ arg res ->
            CG.unit


{-| Generates a encoder for unit.

Decodes `()`, and encodes to JSON `null`.

-}
encoderUnit =
    CG.apply
        [ encoderFn "constant"
        , CG.unit
        ]


{-| Generates a encoder for a basic L1 type.
-}
encoderBasic : Basic -> Expression
encoderBasic basic =
    case basic of
        BBool ->
            encoderFn "bool"

        BInt ->
            encoderFn "int"

        BReal ->
            encoderFn "float"

        BString ->
            encoderFn "string"


encoderNamed named =
    CG.fun (Naming.safeCCL (named ++ "Encoder"))


encoderContainer : Container pos RefChecked -> Expression
encoderContainer container =
    case container of
        CList l1Type ->
            CG.apply [ encoderFn "list", encoderType l1Type ]
                |> CG.parens

        CSet l1Type ->
            CG.apply [ encoderFn "set", encoderType l1Type ]
                |> CG.parens

        CDict l1keyType l1valType ->
            encoderDict l1keyType l1valType

        COptional l1Type ->
            CG.apply [ encoderFn "maybe", encoderType l1Type ]
                |> CG.parens


encoderDict : Type pos RefChecked -> Type pos RefChecked -> Expression
encoderDict l1keyType l1valType =
    case l1keyType of
        TNamed _ _ name (RcRestricted basic) ->
            CG.apply
                [ encoderFn "build"
                , CG.apply
                    [ CG.fqFun refinedMod "dictEncoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ encoderFn "encoder", encoderType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                , CG.apply
                    [ CG.fqFun refinedMod "dictDecoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ encoderFn "decoder", encoderType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                ]

        TNamed _ _ name RcEnum ->
            CG.apply
                [ encoderFn "build"
                , CG.apply
                    [ CG.fqFun enumMod "dictEncoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ encoderFn "encoder", encoderType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                , CG.apply
                    [ CG.fqFun enumMod "dictDecoder"
                    , CG.val (Naming.safeCCL name)
                    , CG.apply [ encoderFn "decoder", encoderType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                ]

        _ ->
            CG.apply [ encoderFn "dict", encoderType l1valType ]
                |> CG.parens


{-| Generates a encoder for an L1 product type that has been named as an alias.
The alias name is also the constructor function for the type.
-}
encoderNamedProduct : String -> List ( String, Type pos RefChecked, L1.Properties ) -> Expression
encoderNamedProduct name fields =
    let
        typeName =
            Naming.safeCCU name

        impl =
            CG.apply
                [ encoderFn "object"
                , encoderFields fields |> CG.list
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
encoderContainerField : String -> Container pos RefChecked -> Expression
encoderContainerField name container =
    case container of
        CList l1Type ->
            CG.apply [ encoderFn "list", encoderType l1Type ]
                |> CG.parens
                |> encoderField name

        CSet l1Type ->
            CG.apply [ encoderFn "set", encoderType l1Type ]
                |> CG.parens
                |> encoderField name

        CDict l1keyType l1valType ->
            encoderDict l1keyType l1valType
                |> encoderField name

        COptional l1Type ->
            encoderType l1Type
                |> encoderOptionalField name



--== Helper Functions


{-| Outputs encoders for a list of fields and terminates the list with `Encoder.buildObject`.
Helper function useful when building record encoders.
-}
encoderFields : List ( String, Type pos RefChecked, L1.Properties ) -> List Expression
encoderFields fields =
    List.foldr (\( fieldName, l1Type, _ ) accum -> encoderTypeField fieldName l1Type :: accum)
        []
        fields


{-| Helper function for building field encoders.
-}
encoderField : String -> Expression -> Expression
encoderField name expr =
    CG.tuple
        [ CG.string name
        , CG.apply [ expr, CG.access (CG.val "val") (Naming.safeCCL name) ]
        ]


{-| Helper function for building optional field encoders.
-}
encoderOptionalField : String -> Expression -> Expression
encoderOptionalField name expr =
    CG.tuple
        [ CG.string name
        , CG.apply [ expr, CG.access (CG.val "val") (Naming.safeCCL name) ]
        ]


encoderMod : List String
encoderMod =
    [ "Encoder" ]


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


encoderFn : String -> Expression
encoderFn =
    CG.fqFun encoderMod


encoderImport : Import
encoderImport =
    CG.importStmt encoderMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Encoder" ])


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
