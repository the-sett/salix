module Checker exposing (check, errorToString)

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), L1, Restricted(..), Type(..))
import L2 exposing (L2, RefChecked(..))
import List.Nonempty exposing (Nonempty(..))
import Maybe.Extra
import MultiError exposing (ResultME)
import Naming


type ModelCheckingError
    = UnresolvedRef String
    | MapKeyTypeNotAllowed
    | BadFieldName String
    | BadDeclarationName String
    | DeclaredMoreThanOnce String


errorToString : ModelCheckingError -> String
errorToString err =
    case err of
        UnresolvedRef hint ->
            hint ++ " reference did not resolve."

        MapKeyTypeNotAllowed ->
            "Map .key is not an enum, restricted, or basic."

        BadFieldName name ->
            name ++ " is not allowed as a field name."

        BadDeclarationName name ->
            name ++ " is not allows as a declaration name."

        DeclaredMoreThanOnce name ->
            name ++ " cannot be declared more than once."


check : L1 -> ResultME ModelCheckingError L2
check l1Decls =
    let
        decls =
            Dict.fromList l1Decls
    in
    Dict.map
        (\key val ->
            MultiError.combine2
                always
                (checkDecl decls val)
                (checkName key)
        )
        decls
        |> MultiError.combineDict


checkDecl :
    Dict String (Declarable a)
    -> Declarable a
    -> ResultME ModelCheckingError (Declarable RefChecked)
checkDecl decls decl =
    case decl of
        DAlias l1type ->
            checkType decls l1type
                |> Result.map DAlias

        DSum constructors ->
            List.Nonempty.map
                (\( name, fields ) ->
                    MultiError.combine2
                        Tuple.pair
                        (checkName name)
                        (checkFields decls fields)
                )
                constructors
                |> MultiError.combineNonempty
                --|> Result.map Naming.sortNonemptyNamed
                |> Result.map DSum

        DEnum labels ->
            DEnum labels |> Ok

        DRestricted res ->
            DRestricted res |> Ok


checkType :
    Dict String (Declarable a)
    -> Type a
    -> ResultME ModelCheckingError (Type RefChecked)
checkType decls l1type =
    case l1type of
        TUnit ->
            TUnit |> Ok

        TBasic basic ->
            TBasic basic |> Ok

        TNamed name _ ->
            case Dict.get name decls of
                Nothing ->
                    UnresolvedRef name
                        |> MultiError.error

                Just resolvedDecl ->
                    declToRefChecked resolvedDecl
                        |> TNamed name
                        |> Ok

        TProduct fields ->
            checkNonemptyFields decls fields
                |> Result.map Naming.sortNonemptyNamed
                |> Result.map TProduct

        TEmptyProduct ->
            TEmptyProduct |> Ok

        TContainer container ->
            checkContainer decls container
                |> Result.map TContainer

        TFunction arg res ->
            MultiError.combine2 TFunction (checkType decls arg) (checkType decls res)


checkContainer :
    Dict String (Declarable a)
    -> Container a
    -> ResultME ModelCheckingError (Container RefChecked)
checkContainer decls container =
    case container of
        CList valType ->
            checkType decls valType
                |> Result.map CList

        CSet valType ->
            checkType decls valType
                |> Result.map CSet

        CDict keyType valType ->
            MultiError.combine2
                CDict
                (checkType decls keyType
                    |> MultiError.andThen checkDictKey
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
checkDictKey : Type RefChecked -> ResultME ModelCheckingError (Type RefChecked)
checkDictKey l2type =
    case l2type of
        TBasic _ ->
            l2type |> Ok

        TNamed _ RcTBasic ->
            l2type |> Ok

        TNamed _ RcEnum ->
            l2type |> Ok

        TNamed _ (RcRestricted _) ->
            l2type |> Ok

        _ ->
            MultiError.error MapKeyTypeNotAllowed


checkNonemptyFields :
    Dict String (Declarable a)
    -> Nonempty ( String, Type a )
    -> ResultME ModelCheckingError (Nonempty ( String, Type RefChecked ))
checkNonemptyFields decls fields =
    fields
        |> List.Nonempty.map
            (\( name, fieldType ) ->
                MultiError.combine2
                    Tuple.pair
                    (checkName name)
                    (checkType decls fieldType)
            )
        |> MultiError.combineNonempty


checkFields :
    Dict String (Declarable a)
    -> List ( String, Type a )
    -> ResultME ModelCheckingError (List ( String, Type RefChecked ))
checkFields decls fields =
    fields
        |> List.map
            (\( name, fieldType ) ->
                MultiError.combine2
                    Tuple.pair
                    (checkName name)
                    (checkType decls fieldType)
            )
        |> MultiError.combineList


checkName : String -> ResultME ModelCheckingError String
checkName val =
    case Naming.checkName val of
        True ->
            Ok val

        False ->
            BadFieldName val |> MultiError.error


declToRefChecked : Declarable a -> RefChecked
declToRefChecked decl =
    case decl of
        DAlias l1type ->
            case l1type of
                TUnit ->
                    RcTUnit

                TBasic _ ->
                    RcTBasic

                TNamed _ _ ->
                    RcTNamed

                TProduct _ ->
                    RcTProduct

                TEmptyProduct ->
                    RcTEmptyProduct

                TContainer _ ->
                    RcTContainer

                TFunction _ _ ->
                    RcTFunction

        DSum constructors ->
            RcSum

        DEnum labels ->
            RcEnum

        DRestricted res ->
            case res of
                RInt _ ->
                    RcRestricted BInt

                RString _ ->
                    RcRestricted BString
