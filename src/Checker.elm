module Checker exposing (check, errorToString)

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), L1, Restricted(..), Type(..), Unchecked)
import L2 exposing (L2, RefChecked(..))
import List.Nonempty exposing (Nonempty(..))
import Maybe.Extra
import Naming
import ResultME exposing (ResultME)
import Set exposing (Set)


type ModelCheckingError pos
    = UnresolvedRef pos String
    | MapKeyTypeNotAllowed pos
    | BadFieldName pos String
    | BadDeclarationName pos String
    | DeclaredMoreThanOnce pos String


errorToString : ModelCheckingError pos -> String
errorToString err =
    case err of
        UnresolvedRef _ hint ->
            hint ++ " reference did not resolve."

        MapKeyTypeNotAllowed _ ->
            "Map .key is not an enum, restricted, or basic."

        BadFieldName _ name ->
            name ++ " is not allowed as a field name."

        BadDeclarationName _ name ->
            name ++ " is not allows as a declaration name."

        DeclaredMoreThanOnce _ name ->
            name ++ " cannot be declared more than once."


{-| Runs checks on an L1 model and lowers it to L2 if all the checks pass.
-}
check : L1 pos -> ResultME (ModelCheckingError pos) (L2 pos)
check l1Decls =
    checkDuplicateDecls l1Decls
        |> ResultME.andThen checkDecls


{-| Checks for duplicate declarations in L1.
-}
checkDuplicateDecls : L1 pos -> ResultME (ModelCheckingError pos) (Dict String (Declarable pos Unchecked))
checkDuplicateDecls l1Decls =
    let
        ( uniq, dupls ) =
            uniqueHelp Tuple.first Set.empty l1Decls [] []

        declaredMoreThanOnce ( name, decl ) =
            DeclaredMoreThanOnce (L1.positionOfDeclarable decl) name
    in
    case dupls of
        [] ->
            Dict.fromList uniq |> Ok

        headDup :: tailDup ->
            Nonempty headDup tailDup
                |> List.Nonempty.map declaredMoreThanOnce
                |> Err


uniqueHelp : (a -> comparable) -> Set comparable -> List a -> List a -> List a -> ( List a, List a )
uniqueHelp f existing remaining uniqAccum duplAccum =
    case remaining of
        [] ->
            ( List.reverse uniqAccum, List.reverse duplAccum )

        first :: rest ->
            let
                computedFirst =
                    f first
            in
            if Set.member computedFirst existing then
                uniqueHelp f existing rest uniqAccum (first :: duplAccum)

            else
                uniqueHelp f (Set.insert computedFirst existing) rest (first :: uniqAccum) duplAccum


{-| Checks all the individual declarations for errors.
-}
checkDecls : Dict String (Declarable pos a) -> ResultME (ModelCheckingError pos) (L2 pos)
checkDecls decls =
    Dict.map
        (\key val ->
            ResultME.combine2
                always
                (checkDecl decls val)
                (checkName (L1.positionOfDeclarable val) key)
        )
        decls
        |> ResultME.combineDict


checkDecl :
    Dict String (Declarable pos a)
    -> Declarable pos a
    -> ResultME (ModelCheckingError pos) (Declarable pos RefChecked)
checkDecl decls decl =
    case decl of
        DAlias pos props l1type ->
            checkType decls l1type
                |> Result.map (DAlias pos props)

        DSum pos props constructors ->
            List.Nonempty.map
                (\( name, fields ) ->
                    ResultME.combine2
                        Tuple.pair
                        (checkName pos name)
                        (checkFields pos decls fields)
                )
                constructors
                |> ResultME.combineNonempty
                |> Result.map (DSum pos props)

        DEnum pos props labels ->
            DEnum pos props labels |> Ok

        DRestricted pos props res ->
            DRestricted pos props res |> Ok


checkType :
    Dict String (Declarable pos a)
    -> Type pos a
    -> ResultME (ModelCheckingError pos) (Type pos RefChecked)
checkType decls l1type =
    case l1type of
        TUnit pos props ->
            TUnit pos props |> Ok

        TBasic pos props basic ->
            TBasic pos props basic |> Ok

        TNamed pos props name _ ->
            case Dict.get name decls of
                Nothing ->
                    UnresolvedRef pos name
                        |> ResultME.error

                Just resolvedDecl ->
                    declToRefChecked resolvedDecl
                        |> TNamed pos props name
                        |> Ok

        TProduct pos props fields ->
            checkNonemptyFields pos decls fields
                |> Result.map Naming.sortNonemptyNamed
                |> Result.map (TProduct pos props)

        TEmptyProduct pos props ->
            TEmptyProduct pos props |> Ok

        TContainer pos props container ->
            checkContainer pos decls container
                |> Result.map (TContainer pos props)

        TFunction pos props arg res ->
            ResultME.combine2 (TFunction pos props) (checkType decls arg) (checkType decls res)


checkContainer :
    pos
    -> Dict String (Declarable pos a)
    -> Container pos a
    -> ResultME (ModelCheckingError pos) (Container pos RefChecked)
checkContainer pos decls container =
    case container of
        CList valType ->
            checkType decls valType
                |> Result.map CList

        CSet valType ->
            checkType decls valType
                |> Result.map CSet

        CDict keyType valType ->
            ResultME.combine2
                CDict
                (checkType decls keyType
                    |> ResultME.andThen (checkDictKey (L1.positionOfType keyType))
                )
                (checkType decls valType)

        COptional valType ->
            checkType decls valType
                |> Result.map COptional


{-| Checks that a Dict key is either a basic type, or a ref to an enum or
refined type.

This check must be performed on a type that has already been ref-checked,
which makes it simple to know what it refers to in the case where it is a ref.

-}
checkDictKey : pos -> Type pos RefChecked -> ResultME (ModelCheckingError pos) (Type pos RefChecked)
checkDictKey pos l2type =
    case l2type of
        TBasic _ _ _ ->
            l2type |> Ok

        TNamed _ _ _ RcTBasic ->
            l2type |> Ok

        TNamed _ _ _ RcEnum ->
            l2type |> Ok

        TNamed _ _ _ (RcRestricted _) ->
            l2type |> Ok

        _ ->
            MapKeyTypeNotAllowed pos |> ResultME.error


checkNonemptyFields :
    pos
    -> Dict String (Declarable pos a)
    -> Nonempty ( String, Type pos a, L1.Properties )
    -> ResultME (ModelCheckingError pos) (Nonempty ( String, Type pos RefChecked, L1.Properties ))
checkNonemptyFields pos decls fields =
    fields
        |> List.Nonempty.map
            (\( name, fieldType, props ) ->
                ResultME.combine2
                    (\checkedName checkedFields -> ( checkedName, checkedFields, props ))
                    (checkName pos name)
                    (checkType decls fieldType)
            )
        |> ResultME.combineNonempty


checkFields :
    pos
    -> Dict String (Declarable pos a)
    -> List ( String, Type pos a, L1.Properties )
    -> ResultME (ModelCheckingError pos) (List ( String, Type pos RefChecked, L1.Properties ))
checkFields pos decls fields =
    fields
        |> List.map
            (\( name, fieldType, props ) ->
                ResultME.combine2
                    (\checkedName checkedFields -> ( checkedName, checkedFields, props ))
                    (checkName pos name)
                    (checkType decls fieldType)
            )
        |> ResultME.combineList


checkName : pos -> String -> ResultME (ModelCheckingError pos) String
checkName pos val =
    case Naming.checkName val of
        True ->
            Ok val

        False ->
            BadFieldName pos val |> ResultME.error


declToRefChecked : Declarable pos a -> RefChecked
declToRefChecked decl =
    case decl of
        DAlias _ _ l1type ->
            case l1type of
                TUnit _ _ ->
                    RcTUnit

                TBasic _ _ _ ->
                    RcTBasic

                TNamed _ _ _ _ ->
                    RcTNamed

                TProduct _ _ _ ->
                    RcTProduct

                TEmptyProduct _ _ ->
                    RcTEmptyProduct

                TContainer _ _ _ ->
                    RcTContainer

                TFunction _ _ _ _ ->
                    RcTFunction

        DSum _ constructors _ ->
            RcSum

        DEnum _ labels _ ->
            RcEnum

        DRestricted _ _ res ->
            case res of
                RInt _ ->
                    RcRestricted BInt

                RString _ ->
                    RcRestricted BString
