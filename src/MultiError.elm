module MultiError exposing (..)

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty(..))


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


combineNonempty : Nonempty (Result (Nonempty err) a) -> Result (Nonempty err) (Nonempty a)
combineNonempty (Nonempty head tail) =
    List.foldl
        (\result accumRes ->
            case ( result, accumRes ) of
                ( Ok val, Ok accum ) ->
                    List.Nonempty.cons val accum |> Ok

                ( Err err, Ok _ ) ->
                    Err err

                ( Ok _, Err errAccum ) ->
                    Err errAccum

                ( Err err, Err errAccum ) ->
                    List.Nonempty.append err errAccum |> Err
        )
        (Result.map List.Nonempty.fromElement head)
        tail


combineDict : Dict comparable (Result (Nonempty err) v) -> Result (Nonempty err) (Dict comparable v)
combineDict results =
    Dict.foldl
        (\key result accumRes ->
            case ( result, accumRes ) of
                ( Ok val, Ok accum ) ->
                    Dict.insert key val accum |> Ok

                ( Err err, Ok _ ) ->
                    Err err

                ( Ok _, Err errAccum ) ->
                    Err errAccum

                ( Err err, Err errAccum ) ->
                    List.Nonempty.append err errAccum |> Err
        )
        (Ok Dict.empty)
        results
