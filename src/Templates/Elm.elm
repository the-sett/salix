module Templates.Elm exposing
    ( typeDecl, codec, codecAsLetDecl
    , lowerType, lowerFun
    )

{-| Elm code generation from L2 models. This can be used when writing an L3 code
generator that output Elm code. It can be used to generate type declarations,
type annotations and JSON codecs for Elm data models.

Type declarations:

@docs typeDecl, codec, codecAsLetDecl

Lowerings of L2 into Elm type annotations:

@docs lowerType, lowerFun

-}

import Elm.CodeGen as CG exposing (Comment, Declaration, DocComment, Expression, Import, LetDeclaration, Linkage, Pattern, TypeAnnotation)
import L1 exposing (Basic(..), Container(..), Declarable(..), Restricted(..), Type(..))
import L2 exposing (RefChecked(..))
import List.Nonempty
import Maybe.Extra
import Naming
import Set exposing (Set)
import Templates.Helper as Util



--== Function Declarations


{-| Captures everything needed to make a function declaration either at top-level or inside
a let block. This allows the decision to build a function as a top-level or a let expression
to be deferred, which allows the same code to be used to generate both.
-}
type alias FunDecl =
    { doc : Maybe (Comment DocComment)
    , sig : Maybe TypeAnnotation
    , name : String
    , args : List Pattern
    , impl : Expression
    }


funDeclAsTopLevel : FunDecl -> Declaration
funDeclAsTopLevel funDecl =
    CG.funDecl funDecl.doc funDecl.sig funDecl.name funDecl.args funDecl.impl


funDeclAsLetDecl : FunDecl -> LetDeclaration
funDeclAsLetDecl funDecl =
    CG.letFunction funDecl.name funDecl.args funDecl.impl



--== Type Declarations


{-| Turns an L1 type declaration into Elm code.

A type can result in a list of declarations - enums in addition to declaring a
type can also declare the permitted enum values.

-}
typeDecl : String -> Comment DocComment -> Declarable pos RefChecked -> ( List Declaration, Linkage )
typeDecl name doc decl =
    case decl of
        DAlias _ _ l1Type ->
            typeAlias name (Just doc) l1Type
                |> Tuple.mapFirst List.singleton

        DSum _ _ constructors ->
            customType name (Just doc) (List.Nonempty.toList constructors)
                |> Tuple.mapFirst List.singleton

        DEnum _ _ labels ->
            enumCustomType name (Just doc) (List.Nonempty.toList labels)

        DRestricted _ _ res ->
            restrictedType name (Just doc) res


{-| Turns an L1 restricted type into Elm code.

This will result in a list of declarations - the type declaration in addition
to the functions needed to create or unbox the restricted type.

-}
restrictedType : String -> Maybe (Comment DocComment) -> Restricted -> ( List Declaration, Linkage )
restrictedType name maybeDoc restricted =
    case restricted of
        RInt res ->
            restrictedInt name maybeDoc res

        RString res ->
            restrictedString name maybeDoc res


restrictedInt :
    String
    -> Maybe (Comment DocComment)
    -> { min : Maybe Int, max : Maybe Int, width : Maybe Int }
    -> ( List Declaration, Linkage )
restrictedInt name maybeDoc res =
    let
        minGuard =
            Maybe.map
                (\minValue -> CG.apply [ CG.fqFun refinedMod "gte", CG.int minValue ])
                res.min

        maxGuard =
            Maybe.map
                (\maxValue -> CG.apply [ CG.fqFun refinedMod "lte", CG.int maxValue ])
                res.max

        guards =
            [ minGuard, maxGuard ] |> Maybe.Extra.values
    in
    case guards of
        [] ->
            -- If there are no guard clauses, it is just an int.
            ( [ CG.aliasDecl Nothing (Naming.safeCCU name) [] (lowerBasic BInt) ]
            , CG.emptyLinkage
                |> CG.addExposing (CG.typeOrAliasExpose (Naming.safeCCU name))
            )

        gd :: gds ->
            let
                boxedTypeDecl =
                    CG.customTypeDecl maybeDoc (Naming.safeCCU name) [] [ ( Naming.safeCCU name, [ CG.intAnn ] ) ]

                restrictedSig =
                    CG.typed "Refined"
                        [ CG.typed "Int" []
                        , CG.typed (Naming.safeCCU name) []
                        , CG.typed "IntError" []
                        ]

                typeWrapper =
                    CG.apply
                        [ CG.fqFun resultMod "map"
                        , CG.fun (Naming.safeCCU name)
                        ]

                guardFn =
                    CG.applyBinOp
                        (Util.mChainResult (CG.apply [ gd, CG.val "val" ])
                            (List.map CG.parens gds)
                        )
                        CG.piper
                        typeWrapper
                        |> CG.letFunction "guardFn" [ CG.varPattern "val" ]

                unboxFn =
                    CG.letFunction "unboxFn"
                        [ CG.namedPattern (Naming.safeCCU name) [ CG.varPattern "val" ] ]
                        (CG.val "val")

                restrictedImpl =
                    CG.apply
                        [ CG.fqFun refinedMod "define"
                        , CG.fun "guardFn"
                        , CG.fqVal decodeMod "int"
                        , CG.fqVal encodeMod "int"
                        , CG.fqFun refinedMod "intErrorToString"
                        , CG.fun "unboxFn"
                        ]
                        |> CG.letExpr [ guardFn, unboxFn ]

                restrictedDecl =
                    CG.valDecl maybeDoc (Just restrictedSig) (Naming.safeCCL name) restrictedImpl
            in
            ( [ boxedTypeDecl
              , restrictedDecl
              ]
            , CG.emptyLinkage
                |> CG.addImport (guardedImportExposing [ "Refined", "IntError" ])
                |> CG.addImport decodeImport
                |> CG.addImport encodeImport
                |> CG.addExposing (CG.funExpose (Naming.safeCCL name))
                |> CG.addExposing (CG.typeOrAliasExpose (Naming.safeCCU name))
            )


restrictedString :
    String
    -> Maybe (Comment DocComment)
    -> { minLength : Maybe Int, maxLength : Maybe Int, regex : Maybe String }
    -> ( List Declaration, Linkage )
restrictedString name maybeDoc res =
    let
        minLenGuard =
            Maybe.map
                (\minValue -> CG.apply [ CG.fqFun refinedMod "minLength", CG.int minValue ])
                res.minLength

        maxLenGuard =
            Maybe.map
                (\maxValue -> CG.apply [ CG.fqFun refinedMod "maxLength", CG.int maxValue ])
                res.maxLength

        patternGuard =
            Maybe.map
                (\regex -> CG.apply [ CG.fqFun refinedMod "regexMatch", CG.string regex ])
                res.regex

        guards =
            [ minLenGuard, maxLenGuard, patternGuard ] |> Maybe.Extra.values
    in
    case guards of
        [] ->
            -- If there are no guard clauses, it is just an string.
            ( [ CG.aliasDecl Nothing (Naming.safeCCU name) [] (lowerBasic BString) ]
            , CG.emptyLinkage
                |> CG.addExposing (CG.typeOrAliasExpose (Naming.safeCCU name))
            )

        gd :: gds ->
            let
                boxedTypeDecl =
                    CG.customTypeDecl maybeDoc (Naming.safeCCU name) [] [ ( Naming.safeCCU name, [ CG.stringAnn ] ) ]

                restrictedSig =
                    CG.typed "Refined"
                        [ CG.typed "String" []
                        , CG.typed (Naming.safeCCU name) []
                        , CG.typed "StringError" []
                        ]

                typeWrapper =
                    CG.apply
                        [ CG.fqFun resultMod "map"
                        , CG.fun (Naming.safeCCU name)
                        ]

                guardFn =
                    CG.applyBinOp
                        (Util.mChainResult (CG.apply [ gd, CG.val "val" ])
                            (List.map CG.parens gds)
                        )
                        CG.piper
                        typeWrapper
                        |> CG.letFunction "guardFn" [ CG.varPattern "val" ]

                unboxFn =
                    CG.letFunction "unboxFn"
                        [ CG.namedPattern (Naming.safeCCU name) [ CG.varPattern "val" ] ]
                        (CG.val "val")

                restrictedImpl =
                    CG.apply
                        [ CG.fqFun refinedMod "define"
                        , CG.fun "guardFn"
                        , CG.fqVal decodeMod "string"
                        , CG.fqVal encodeMod "string"
                        , CG.fqFun refinedMod "stringErrorToString"
                        , CG.fun "unboxFn"
                        ]
                        |> CG.letExpr [ guardFn, unboxFn ]

                restrictedDecl =
                    CG.valDecl maybeDoc (Just restrictedSig) (Naming.safeCCL name) restrictedImpl
            in
            ( [ boxedTypeDecl
              , restrictedDecl
              ]
            , CG.emptyLinkage
                |> CG.addImport (guardedImportExposing [ "Refined", "StringError" ])
                |> CG.addImport decodeImport
                |> CG.addImport encodeImport
                |> CG.addExposing (CG.funExpose (Naming.safeCCL name))
                |> CG.addExposing (CG.typeOrAliasExpose (Naming.safeCCU name))
            )


{-| Turns an L1 `Type` into a type alias in Elm code.
-}
typeAlias : String -> Maybe (Comment DocComment) -> Type pos RefChecked -> ( Declaration, Linkage )
typeAlias name maybeDoc l1Type =
    let
        ( loweredType, linkage ) =
            lowerType l1Type
    in
    ( CG.aliasDecl maybeDoc (Naming.safeCCU name) [] loweredType
    , linkage
        |> CG.addExposing (CG.typeOrAliasExpose (Naming.safeCCU name))
    )


{-| Turns an L1 sum type into a custom type in Elm code.
-}
customType :
    String
    -> Maybe (Comment DocComment)
    -> List ( String, List ( String, Type pos RefChecked, L1.Properties ) )
    -> ( Declaration, Linkage )
customType name maybeDoc constructors =
    let
        lowerArgs ( _, l1Type, _ ) =
            lowerType l1Type

        ( mappedConstructors, linkages ) =
            List.map
                (\( consName, consArgs ) ->
                    let
                        ( loweredArgs, linkage ) =
                            List.map lowerArgs consArgs
                                |> List.unzip
                                |> Tuple.mapSecond CG.combineLinkage
                    in
                    ( ( Naming.safeCCU consName, loweredArgs ), linkage )
                )
                constructors
                |> List.unzip
    in
    ( CG.customTypeDecl maybeDoc (Naming.safeCCU name) [] mappedConstructors
    , CG.combineLinkage linkages
        |> CG.addExposing (CG.openTypeExpose (Naming.safeCCU name))
    )


{-| Turns an L1 enum type into a custom type in Elm code.

This produces 2 declarations, one for the guarded type, and one for the enum
declaring its allowed values.

-}
enumCustomType : String -> Maybe (Comment DocComment) -> List String -> ( List Declaration, Linkage )
enumCustomType name maybeDoc labels =
    let
        constructors =
            List.map
                (\label -> ( Naming.safeCCU name ++ Naming.safeCCU label, [] ))
                labels

        enumValues =
            CG.apply
                [ CG.fqFun enumMod "define"
                , List.map
                    (\label ->
                        CG.fun (Naming.safeCCU name ++ Naming.safeCCU label)
                    )
                    labels
                    |> CG.list
                , CG.lambda [ CG.varPattern "val" ]
                    (CG.caseExpr (CG.val "val")
                        (List.map
                            (\label ->
                                ( CG.namedPattern (Naming.safeCCU name ++ Naming.safeCCU label) []
                                , CG.string label
                                )
                            )
                            labels
                        )
                    )
                ]

        enumSig =
            CG.typed "Enum" [ CG.typed (Naming.safeCCU name) [] ]
    in
    ( [ CG.customTypeDecl maybeDoc (Naming.safeCCU name) [] constructors
      , CG.valDecl maybeDoc (Just enumSig) (Naming.safeCCL name) enumValues
      ]
    , CG.emptyLinkage
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose (Naming.safeCCL name))
        |> CG.addExposing (CG.openTypeExpose (Naming.safeCCU name))
    )


{-| Turns an L1 enum type into a refined type in Elm code.

This produces 2 declarations, one for the refined type, and one for the enum
declaring its allowed values.

-}
enumRefinedType : String -> List String -> ( List Declaration, Linkage )
enumRefinedType name labels =
    let
        guardedConstructor =
            [ ( Naming.safeCCU name, [ CG.stringAnn ] ) ]

        enumValues =
            CG.apply
                [ CG.fqFun enumMod "define"
                , List.map
                    (\label ->
                        CG.apply
                            [ CG.fun (Naming.safeCCU name)
                            , label |> CG.string
                            ]
                    )
                    labels
                    |> CG.list
                , CG.lambda [ CG.namedPattern (Naming.safeCCU name) [ CG.varPattern "val" ] ]
                    (CG.val "val")
                ]

        enumSig =
            CG.typed "Enum" [ CG.typed (Naming.safeCCU name) [] ]
    in
    ( [ CG.customTypeDecl Nothing (Naming.safeCCU name) [] guardedConstructor
      , CG.valDecl Nothing (Just enumSig) (Naming.safeCCL name) enumValues
      ]
    , CG.emptyLinkage
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose (Naming.safeCCL name))
    )


{-| Lowers an L1 type into an Elm type annotation.
-}
lowerType : Type pos RefChecked -> ( TypeAnnotation, Linkage )
lowerType l1Type =
    case l1Type of
        TUnit _ _ ->
            ( CG.unitAnn, CG.emptyLinkage )

        TBasic _ _ basic ->
            ( lowerBasic basic
            , CG.emptyLinkage
            )

        TNamed _ _ name _ ->
            ( CG.typed (Naming.safeCCU name) []
            , CG.emptyLinkage
            )

        TProduct _ _ fields ->
            lowerProduct (List.Nonempty.toList fields)

        TEmptyProduct _ _ ->
            lowerProduct []

        TContainer _ _ container ->
            lowerContainer container

        TFunction _ _ arg res ->
            ( CG.unitAnn
            , CG.emptyLinkage
            )


{-| Lowers an L1 basic type into an Elm type annotation.
-}
lowerBasic : Basic -> TypeAnnotation
lowerBasic basic =
    case basic of
        BBool ->
            CG.boolAnn

        BInt ->
            CG.intAnn

        BReal ->
            CG.floatAnn

        BString ->
            CG.stringAnn


{-| Lowers an L1 product type into an Elm type annotation.
-}
lowerProduct : List ( String, Type pos RefChecked, L1.Properties ) -> ( TypeAnnotation, Linkage )
lowerProduct fields =
    let
        ( mappedFields, linkages ) =
            List.map
                (\( name, l1Type, _ ) ->
                    let
                        ( loweredType, linkage ) =
                            lowerType l1Type
                    in
                    ( ( Naming.safeCCL name, loweredType ), linkage )
                )
                fields
                |> List.unzip
    in
    ( CG.recordAnn mappedFields
    , CG.combineLinkage linkages
    )


{-| Lowers an L1 container type into an Elm type annotation.
-}
lowerContainer : Container pos RefChecked -> ( TypeAnnotation, Linkage )
lowerContainer container =
    case container of
        CList l1Type ->
            lowerType l1Type
                |> Tuple.mapFirst CG.listAnn

        CSet l1Type ->
            lowerType l1Type
                |> Tuple.mapFirst CG.setAnn
                |> Tuple.mapSecond (CG.addImport setImport)

        CDict l1keyType l1valType ->
            lowerDict l1keyType l1valType

        COptional l1Type ->
            lowerType l1Type
                |> Tuple.mapFirst CG.maybeAnn


lowerDict : Type pos RefChecked -> Type pos RefChecked -> ( TypeAnnotation, Linkage )
lowerDict l1keyType l1valType =
    case l1keyType of
        TNamed _ _ name (RcRestricted basic) ->
            let
                ( keyAnn, keyLink ) =
                    lowerType l1keyType

                ( valAnn, valLink ) =
                    lowerType l1valType
            in
            ( CG.fqTyped dictRefinedMod "Dict" [ lowerBasic basic, keyAnn, valAnn ]
            , CG.combineLinkage [ keyLink, valLink ] |> CG.addImport dictRefinedImport
            )

        TNamed _ _ name RcEnum ->
            let
                ( keyAnn, keyLink ) =
                    lowerType l1keyType

                ( valAnn, valLink ) =
                    lowerType l1valType
            in
            ( CG.fqTyped dictEnumMod "Dict" [ keyAnn, valAnn ]
            , CG.combineLinkage [ keyLink, valLink ] |> CG.addImport dictEnumImport
            )

        _ ->
            let
                ( keyAnn, keyLink ) =
                    lowerType l1keyType

                ( valAnn, valLink ) =
                    lowerType l1valType
            in
            ( CG.dictAnn keyAnn valAnn
            , CG.combineLinkage [ keyLink, valLink ] |> CG.addImport dictImport
            )


{-| Lowers an L1 function type into an Elm type annotation
-}
lowerFun : Type pos RefChecked -> Type pos RefChecked -> ( TypeAnnotation, Linkage )
lowerFun fromType toType =
    let
        ( from, fromLinkage ) =
            lowerType fromType

        ( to, toLinkage ) =
            lowerType toType
    in
    ( CG.funAnn from to
    , CG.combineLinkage [ fromLinkage, toLinkage ]
    )



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
