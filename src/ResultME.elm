module ResultME exposing
    ( ResultME, error, errors, fromResult
    , combineApply, combine2, combine3, combine4, combine5
    , combineList, combineDict, combineNonempty
    , map
    , mapError
    , andThen, flatten
    )

{-| ResultME is a variation on Result, where the `err` is a non-empty list of
errors. This is useful in situations where multiple errors can be detected in a
single pass, and it is preferable to report all errors detected, and not to fail
only on the first error.

Some examples; when parsing a form with multiple inputs and possibly multiple
errors to report to the user; when parsing some source code which may contain
multiple syntax errors.


# Type and Constructors

@docs ResultME, error, errors, fromResult


# Combining errors from multiple sources together

@docs combineApply, combine2, combine3, combine4, combine5
@docs combineList, combineDict, combineNonempty


# Mapping

@docs map
@docs mapError


# Chaining

@docs andThen, flatten

-}

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty(..))


{-| The result-possible-with-multiple-errors type.
-}
type alias ResultME err a =
    Result (Nonempty err) a


error : err -> ResultME err a
error err =
    err
        |> List.Nonempty.fromElement
        |> Err


errors : err -> List err -> ResultME err a
errors err errs =
    List.Nonempty.Nonempty err errs |> Err


fromResult : Result err a -> ResultME err a
fromResult result =
    case result of
        Err err ->
            error err

        Ok val ->
            Ok val


combineApply : ResultME e a -> ResultME e (a -> b) -> ResultME e b
combineApply ra rfn =
    case ( ra, rfn ) of
        ( Ok a, Ok fun ) ->
            fun a |> Ok

        ( Err err, Ok _ ) ->
            Err err

        ( Ok _, Err err ) ->
            Err err

        ( Err err1, Err err2 ) ->
            List.Nonempty.append err1 err2
                |> Err


combine2 : (a -> b -> c) -> ResultME err a -> ResultME err b -> ResultME err c
combine2 fun first second =
    case ( first, second ) of
        ( Ok checkedArg, Ok checkedRes ) ->
            fun checkedArg checkedRes |> Ok

        ( Err err, Ok _ ) ->
            Err err

        ( Ok _, Err err ) ->
            Err err

        ( Err err1, Err err2 ) ->
            List.Nonempty.append err1 err2
                |> Err


combine3 :
    (a -> b -> c -> d)
    -> ResultME err a
    -> ResultME err b
    -> ResultME err c
    -> ResultME err d
combine3 fun first second third =
    case first of
        Ok checkedFirst ->
            combine2 (fun checkedFirst) second third

        Err errFirst ->
            combineApply third
                (combine2 (flip fun) second first)


combine4 :
    (a -> b -> c -> d -> e)
    -> ResultME err a
    -> ResultME err b
    -> ResultME err c
    -> ResultME err d
    -> ResultME err e
combine4 fun first second third fourth =
    case first of
        Ok checkedFirst ->
            combine3 (fun checkedFirst) second third fourth

        Err errFirst ->
            combineApply fourth
                (combine3 (flip fun) second first third)


combine5 :
    (a -> b -> c -> d -> e -> f)
    -> ResultME err a
    -> ResultME err b
    -> ResultME err c
    -> ResultME err d
    -> ResultME err e
    -> ResultME err f
combine5 fun first second third fourth fifth =
    case first of
        Ok checkedFirst ->
            combine4 (fun checkedFirst) second third fourth fifth

        Err errFirst ->
            combineApply fifth
                (combine4 (flip fun) second first third fourth)


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


flatten : ResultME err (ResultME err a) -> ResultME err a
flatten res =
    case res of
        Err err ->
            Err err

        Ok (Err err) ->
            Err err

        Ok (Ok val) ->
            Ok val


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b
