module Trans exposing (check)

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), Declarations, L1, Restricted(..), Type(..))
import L2 exposing (L2, RefChecked(..))
import List.Nonempty exposing (Nonempty)
import Maybe.Extra


type ModelCheckingError
    = UnresolvedRef String
    | MapKeyTypeNotAllowed


errorToString : ModelCheckingError -> String
errorToString err =
    case err of
        UnresolvedRef hint ->
            hint ++ " reference did not resolve."

        MapKeyTypeNotAllowed ->
            "Map .key is not an enum, restricted, or basic."


check : L1 -> Result (Nonempty ModelCheckingError) L2
check decls =
    Dict.empty |> Ok


checkDecl : L1 -> Declarable a -> Result (Nonempty ModelCheckingError) (Declarable RefChecked)
checkDecl decls decl =
    case decl of
        DAlias l1type ->
            checkType decls l1type
                |> Result.map DAlias

        DSum constructors ->
            List.map
                (\( name, fields ) ->
                    checkFields decls fields
                        |> Result.map (\checkedFields -> ( name, checkedFields ))
                )
                constructors
                |> combineList
                |> Result.map DSum

        DEnum labels ->
            DEnum labels |> Ok

        DRestricted res ->
            DRestricted res |> Ok


checkType : L1 -> Type a -> Result (Nonempty ModelCheckingError) (Type RefChecked)
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
                        |> List.Nonempty.fromElement
                        |> Err

                Just resolvedDecl ->
                    declToRefChecked resolvedDecl
                        |> TNamed name
                        |> Ok

        TProduct fields ->
            checkFields decls fields
                |> Result.map TProduct

        TContainer container ->
            checkContainer decls container
                |> Result.map TContainer

        TFunction arg res ->
            combine2 TFunction (checkType decls arg) (checkType decls res)


checkContainer : L1 -> Container a -> Result (Nonempty ModelCheckingError) (Container RefChecked)
checkContainer decls container =
    case container of
        CList valType ->
            checkType decls valType
                |> Result.map CList

        CSet valType ->
            checkType decls valType
                |> Result.map CSet

        CDict keyType valType ->
            combine2 CDict (checkType decls keyType) (checkType decls valType)

        COptional valType ->
            checkType decls valType
                |> Result.map COptional


checkFields :
    L1
    -> List ( String, Type a )
    -> Result (Nonempty ModelCheckingError) (List ( String, Type RefChecked ))
checkFields decls fields =
    fields
        |> List.map
            (\( name, fieldType ) ->
                checkType decls fieldType |> Result.map (\checkedType -> ( name, checkedType ))
            )
        |> combineList


declToRefChecked : Declarable a -> RefChecked
declToRefChecked decl =
    case decl of
        DAlias l1type ->
            RcNone

        DSum constructors ->
            RcNone

        DEnum labels ->
            RcEnum

        DRestricted res ->
            case res of
                RInt _ ->
                    RcRestricted BInt

                RString _ ->
                    RcRestricted BString


combine2 : (a -> b -> c) -> Result (Nonempty err) a -> Result (Nonempty err) b -> Result (Nonempty err) c
combine2 fun first second =
    case ( first, second ) of
        ( Ok checkedArg, Ok checkedRes ) ->
            fun checkedArg checkedRes |> Ok

        ( Err error, Ok _ ) ->
            Err error

        ( Ok _, Err error ) ->
            Err error

        ( Err error1, Err error2 ) ->
            List.Nonempty.append error1 error2
                |> Err


combineList : List (Result (Nonempty err) a) -> Result (Nonempty err) (List a)
combineList results =
    List.foldl
        (\result accumRes ->
            case ( result, accumRes ) of
                ( Ok val, Ok accum ) ->
                    val :: accum |> Ok

                ( Err err, Ok _ ) ->
                    Err err

                ( Ok _, Err errAccum ) ->
                    Err errAccum

                ( Err err, Err errAccum ) ->
                    List.Nonempty.append err errAccum |> Err
        )
        (Ok [])
        results
