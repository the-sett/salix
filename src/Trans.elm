module Trans exposing (transform)

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), Declarations, L1, Restricted(..), Type(..))
import L2 exposing (L2, RefChecked(..))
import List.Nonempty exposing (Nonempty)
import Maybe.Extra


type ModelCheckingError
    = UnresolvedRef String
    | MapKeyTypeNotAllowed
    | Blah


errorToString : ModelCheckingError -> String
errorToString err =
    case err of
        UnresolvedRef hint ->
            hint ++ " reference did not resolve."

        MapKeyTypeNotAllowed ->
            "Map .key is not an enum, restricted, or basic."

        Blah ->
            "Todo"


dummyError =
    Blah
        |> List.Nonempty.fromElement
        |> Err


transform : L1 -> Result ModelCheckingError L2
transform _ =
    Dict.empty |> Ok


walk : Declarable a -> Result (Nonempty ModelCheckingError) (Declarable RefChecked)
walk decl =
    case decl of
        DAlias l1type ->
            walkType l1type
                |> Result.map DAlias

        DSum constructors ->
            List.map
                (\( name, fields ) ->
                    checkFields fields
                        |> Result.map (\checkedFields -> ( name, checkedFields ))
                )
                constructors
                |> mapResultErrList identity
                |> Result.map DSum

        DEnum labels ->
            DEnum labels |> Ok

        DRestricted res ->
            DRestricted res |> Ok


walkType : Type a -> Result (Nonempty ModelCheckingError) (Type RefChecked)
walkType l1type =
    case l1type of
        TUnit ->
            TUnit |> Ok

        TBasic basic ->
            TBasic basic |> Ok

        TNamed name _ ->
            dummyError

        TProduct fields ->
            checkFields fields
                |> Result.map TProduct

        TContainer container ->
            walkContainer container
                |> Result.map TContainer

        TFunction arg res ->
            map2ResultErr TFunction (walkType arg) (walkType res)


walkContainer : Container a -> Result (Nonempty ModelCheckingError) (Container RefChecked)
walkContainer container =
    case container of
        CList valType ->
            walkType valType
                |> Result.map CList

        CSet valType ->
            walkType valType
                |> Result.map CSet

        CDict keyType valType ->
            map2ResultErr CDict (walkType keyType) (walkType valType)

        COptional valType ->
            walkType valType
                |> Result.map COptional


checkFields :
    List ( String, Type a )
    -> Result (Nonempty ModelCheckingError) (List ( String, Type RefChecked ))
checkFields fields =
    fields
        |> List.map
            (\( name, fieldType ) ->
                walkType fieldType |> Result.map (\checkedType -> ( name, checkedType ))
            )
        |> mapResultErrList identity


map2ResultErr : (a -> b -> c) -> Result (Nonempty err) a -> Result (Nonempty err) b -> Result (Nonempty err) c
map2ResultErr fun first second =
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


mapResultErrList : (a -> b) -> List (Result (Nonempty err) a) -> Result (Nonempty err) (List b)
mapResultErrList fun results =
    List.foldl
        (\result accumRes ->
            case ( result, accumRes ) of
                ( Ok val, Ok accum ) ->
                    fun val :: accum |> Ok

                ( Err err, Ok _ ) ->
                    Err err

                ( Ok _, Err errAccum ) ->
                    Err errAccum

                ( Err err, Err errAccum ) ->
                    List.Nonempty.append err errAccum |> Err
        )
        (Ok [])
        results
