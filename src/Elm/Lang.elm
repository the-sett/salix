module Elm.Lang exposing
    ( typeDecl
    , lowerType, lowerFun
    )

{-| Elm code generation from L2 models. This can be used when writing a code
generator that outputs Elm code. It can be used to generate type declarations
and type annotations for Elm data models.

Type declarations:

@docs typeDecl

Lowerings of L2 into Elm type annotations:

@docs lowerType, lowerFun

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
