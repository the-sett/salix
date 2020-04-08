module Query exposing (..)

{-| Functions for querying Salix models.


# Dereferencing named type aliases.

@docs deref

-}

import Dict exposing (Dict)
import L1 exposing (Declarable(..), Properties, Type(..))
import L2 exposing (L2, RefChecked)
import L3 exposing (L3, L3Error(..), PropertiesAPI)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import ResultME exposing (ResultME)



-- Maybe this should all just go in the L3 module?
--
-- Dereferencing named type aliases.
-- TODO: This should recurse on named types, as there may be a chain of aliases.


deref : String -> L3 pos -> ResultME L3Error (Declarable pos RefChecked)
deref name model =
    case Dict.get name model.declarations of
        Just val ->
            Ok val

        Nothing ->
            DerefDeclMissing name |> ResultME.error



-- type Declarable pos ref
--     = DAlias pos Properties (Type pos ref)
--     | DSum pos Properties (Nonempty ( String, List ( String, Type pos ref, Properties ) ))
--     | DEnum pos Properties (Nonempty String)
--     | DRestricted pos Properties Restricted
--


expectAlias : Declarable pos ref -> ResultME L3Error ( pos, Properties, Type pos ref )
expectAlias decl =
    case decl of
        DAlias pos props l1type ->
            Ok ( pos, props, l1type )

        _ ->
            NotExpectedKind "" "" |> ResultME.error



--
-- = TUnit pos Properties
-- | TBasic pos Properties Basic
-- | TNamed pos Properties String ref
-- | TProduct pos Properties (Nonempty ( String, Type pos ref, Properties ))
-- | TEmptyProduct pos Properties
-- | TContainer pos Properties (Container pos ref)
-- | TFunction pos Properties (Type pos ref) (Type pos ref)
--expectProduct : Type pos ref -> ResultME L3Error ( pos, Properties, Nonempty ( String, Type pos ref, Properties ) )
--
-- Filtering by properties.


type alias PropertyFilter pos a =
    PropertiesAPI pos -> a -> ResultME L3.L3Error Bool


andPropFilter : PropertyFilter pos a -> PropertyFilter pos a -> PropertyFilter pos a
andPropFilter filterA filterB =
    \propertiesAPI val ->
        filterA propertiesAPI val
            |> ResultME.andThen
                (\bool ->
                    if bool then
                        filterB propertiesAPI val

                    else
                        Ok False
                )


orPropFilter : PropertyFilter pos a -> PropertyFilter pos a -> PropertyFilter pos a
orPropFilter filterA filterB =
    \propertiesAPI val ->
        filterA propertiesAPI val
            |> ResultME.andThen
                (\bool ->
                    if bool then
                        Ok True

                    else
                        filterB propertiesAPI val
                )


notPropFilter : PropertyFilter pos a -> PropertyFilter pos a
notPropFilter filterA =
    \propertiesAPI val ->
        filterA propertiesAPI val
            |> ResultME.andThen
                (\bool ->
                    if bool then
                        Ok False

                    else
                        Ok True
                )


filterDictByProps :
    PropertiesAPI pos
    -> PropertyFilter pos a
    -> Dict String a
    -> ResultME L3.L3Error (Dict String a)
filterDictByProps propertiesApi filter dict =
    let
        ( filtered, errors ) =
            Dict.foldl
                (\name val ( accum, errAccum ) ->
                    case filter propertiesApi val of
                        Ok False ->
                            ( accum, errAccum )

                        Ok True ->
                            ( Dict.insert name val accum, errAccum )

                        Err err ->
                            ( accum, Nonempty.toList err ++ errAccum )
                )
                ( Dict.empty, [] )
                dict
    in
    case errors of
        [] ->
            Ok filtered

        e :: es ->
            Err (Nonempty e es)


filterListByProps :
    PropertiesAPI pos
    -> PropertyFilter pos a
    -> List a
    -> ResultME L3.L3Error (List a)
filterListByProps propertiesApi filter vals =
    let
        ( filtered, errors ) =
            List.foldl
                (\val ( accum, errAccum ) ->
                    case filter propertiesApi val of
                        Ok False ->
                            ( accum, errAccum )

                        Ok True ->
                            ( val :: accum, errAccum )

                        Err err ->
                            ( accum, Nonempty.toList err ++ errAccum )
                )
                ( [], [] )
                vals
    in
    case errors of
        [] ->
            Ok filtered

        e :: es ->
            Err (Nonempty e es)


filterNonemptyByProps :
    PropertiesAPI pos
    -> PropertyFilter pos a
    -> Nonempty a
    -> ResultME L3.L3Error (List a)
filterNonemptyByProps propertiesApi filter vals =
    filterListByProps propertiesApi filter (Nonempty.toList vals)
