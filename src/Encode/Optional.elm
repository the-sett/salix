module Encode.Optional exposing
    ( Field, field, optionalField, nullableField, skippableField
    , objectMaySkip, objectMayNullify
    )

{-| Support for encoding optional fields in JSON.

If a record has `Maybe` fields in it, they could be encoded either as `null` or
left out altogether.

A good HTTP API will usually treat `null` or missing in the same way. But there are
unstable APIs out there that treat them differently. For example, a PUT request that
treats `null` as meaning set something to null, but missing to mean leave something
as it currently is.

This API lets you choose easily whether to use nulls or skip optional fields. It is
also designed to keep the encoder looking clean with ( "fieldName", value ) tuples
in a list.


# Make fields.

@docs Field, field, optionalField, nullableField, skippableField


# Make objects.

@docs objectMaySkip, objectMayNullify

-}

import Json.Encode as Encode exposing (Value)


{-| Fields of JSON object that can be optional.
-}
type Field
    = WithValue String Value
    | Optional String
    | Nullable String
    | Skippable


{-| Creates a field that must always have a value.
-}
field : ( String, Value ) -> Field
field ( name, val ) =
    WithValue name val


{-| Creates a field that may have a value. When no value is set, this field will
be encoded as `null` or skipped, depending on what behaviour is requested when
building the object.
-}
optionalField : ( String, Maybe a ) -> (a -> Value) -> Field
optionalField ( name, maybeVal ) encoder =
    case maybeVal of
        Just val ->
            WithValue name (encoder val)

        Nothing ->
            Optional name


{-| Creates a field that may have a value. When no value is set, this field will
always be encoded as `null`, ignoring any default behaviour requested when building
the object.
-}
nullableField : ( String, Maybe a ) -> (a -> Value) -> Field
nullableField ( name, maybeVal ) encoder =
    case maybeVal of
        Just val ->
            WithValue name (encoder val)

        Nothing ->
            Nullable name


{-| Creates a field that may have a value. When no value is set, this field will
always be skipped, ignoring any default behaviour requested when building
the object.
-}
skippableField : ( String, Maybe a ) -> (a -> Value) -> Field
skippableField ( name, maybeVal ) encoder =
    case maybeVal of
        Just val ->
            WithValue name (encoder val)

        Nothing ->
            Skippable


{-| Encodes a JSON object. Any `optionalField` that is `Nothing` is skipped in
the output.
-}
objectMaySkip : List Field -> Value
objectMaySkip fields =
    List.foldr
        (\fld accum ->
            case fld of
                WithValue name val ->
                    ( name, val ) :: accum

                Optional name ->
                    accum

                Nullable name ->
                    ( name, Encode.null ) :: accum

                Skippable ->
                    accum
        )
        []
        fields
        |> Encode.object


{-| Encodes a JSON object. Any `optionalField` that is `Nothing` is output as
`null`.
-}
objectMayNullify : List Field -> Value
objectMayNullify fields =
    List.foldr
        (\fld accum ->
            case fld of
                WithValue name val ->
                    ( name, val ) :: accum

                Optional name ->
                    ( name, Encode.null ) :: accum

                Nullable name ->
                    ( name, Encode.null ) :: accum

                Skippable ->
                    accum
        )
        []
        fields
        |> Encode.object
