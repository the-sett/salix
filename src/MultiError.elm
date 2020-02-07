module MultiError exposing
    ( ResultME
    , combine2, combineList, combineDict, combineNonempty
    , map
    , mapError
    , andThen
    )

{-| MultiError is a variation on Result, where the `err` is a non-empty list of
errors. This is useful in situations where multiple errors can be detected in a
single pass, and it is preferable to report all errors detected, and not to fail
only on the first error.

Some examples; when parsing a form with multiple inputs and possibly multiple
errors to report to the user; when parsing some source code which may contain
multiple syntax errors.


# Type and Constructors

@docs ResultME


# Combining errors from multiple sources together

@docs combine2, combineList, combineDict, combineNonempty


# Mapping

@docs map
@docs mapError


# Chaining

@docs andThen

-}

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty(..))


{-| The result-possible-with-multiple-errors type.
-}
type alias ResultME err a =
    Result (Nonempty err) a


combine2 : (a -> b -> c) -> ResultME err a -> ResultME err b -> ResultME err c
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


combineList : List (ResultME err a) -> ResultME err (List a)
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


combineDict : Dict comparable (ResultME err v) -> ResultME err (Dict comparable v)
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


combineNonempty : Nonempty (ResultME err a) -> ResultME err (Nonempty a)
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


map : (a -> b) -> ResultME err a -> ResultME err b
map =
    Result.map


mapError : (x -> y) -> ResultME x a -> ResultME y a
mapError fun result =
    Result.mapError (List.Nonempty.map fun) result


andThen : (a -> ResultME err b) -> ResultME err a -> ResultME err b
andThen =
    Result.andThen
