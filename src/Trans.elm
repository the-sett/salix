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


transform : L1 -> Result (Nonempty ModelCheckingError) L2
transform decls =
    Dict.empty |> Ok


refcheckDecl : L1 -> Declarable a -> Result (Nonempty ModelCheckingError) (Declarable RefChecked)
refcheckDecl decls decl =
    case decl of
        DAlias l1type ->
            refcheckType decls l1type
                |> Result.map DAlias

        DSum constructors ->
            List.map
                (\( name, fields ) ->
                    refcheckFields decls fields
                        |> Result.map (\checkedFields -> ( name, checkedFields ))
                )
                constructors
                |> mapResultErrList identity
                |> Result.map DSum

        DEnum labels ->
            DEnum labels |> Ok

        DRestricted res ->
            DRestricted res |> Ok


refcheckType : L1 -> Type a -> Result (Nonempty ModelCheckingError) (Type RefChecked)
refcheckType decls l1type =
    case l1type of
        TUnit ->
            TUnit |> Ok

        TBasic basic ->
            TBasic basic |> Ok

        TNamed name _ ->
            dummyError

        TProduct fields ->
            refcheckFields decls fields
                |> Result.map TProduct

        TContainer container ->
            refcheckContainer decls container
                |> Result.map TContainer

        TFunction arg res ->
            map2ResultErr TFunction (refcheckType decls arg) (refcheckType decls res)


refcheckContainer : L1 -> Container a -> Result (Nonempty ModelCheckingError) (Container RefChecked)
refcheckContainer decls container =
    case container of
        CList valType ->
            refcheckType decls valType
                |> Result.map CList

        CSet valType ->
            refcheckType decls valType
                |> Result.map CSet

        CDict keyType valType ->
            map2ResultErr CDict (refcheckType decls keyType) (refcheckType decls valType)

        COptional valType ->
            refcheckType decls valType
                |> Result.map COptional


refcheckFields :
    L1
    -> List ( String, Type a )
    -> Result (Nonempty ModelCheckingError) (List ( String, Type RefChecked ))
refcheckFields decls fields =
    fields
        |> List.map
            (\( name, fieldType ) ->
                refcheckType decls fieldType |> Result.map (\checkedType -> ( name, checkedType ))
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
