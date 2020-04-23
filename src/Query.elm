module Query exposing (..)

{-| Functions for querying Salix models.


# Dereferencing named type aliases.

-- Maybe this should all just go in the L3 module?

@docs deref

-}

import Dict exposing (Dict)
import L1 exposing (Declarable(..), Field, Properties, Type(..))
import L2 exposing (L2, RefChecked)
import L3 exposing (L3, L3Error(..), PropertiesAPI)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import ResultME exposing (ResultME)
import Search exposing (Step)



-- Dereferencing named type aliases.


{-| Dereferences an alias. If an alias is to another alias, since it consists only of a named
type, this name will also be derferences recursively, until something that is not a named type
is encountered.
-}
deref : String -> L2 pos -> ResultME L3Error (Declarable pos RefChecked)
deref name model =
    case Dict.get name model of
        Just val ->
            case val of
                DAlias _ _ (TNamed _ _ nextName _) ->
                    deref nextName model

                _ ->
                    Ok val

        Nothing ->
            DerefDeclMissing name |> ResultME.error


{-| Finds the transitive closure starting from a sub-set of declarations from an L2.
Any members of the sub-set that are aliases, will pull their referred to declarations
into the closure, and this process will be continued recursively until no more members
are added to the closure.

This is useful if some declarations need to be generated from, including anything they
reference. For example, to code generate an encoder for a declaration that contains other
named types, the encoders for those other types also need to be generated. The transitive
closure gives the full set of declaration to generate encoders for to complete the code.

-- TODO: Make the buffer a Dict by name. That will automatically remove duplicates. DFS
over a Dict. Use ai-search? Find all goals as a list of errors or named declarations, combine
errors, tranform to a dict.

-}
transitiveClosure : L2 pos -> L2 pos -> ResultME L3Error (L2 pos)
transitiveClosure set model =
    let
        doOne name decl accum =
            case decl of
                DAlias _ _ (TNamed _ _ nextName _) ->
                    deref nextName model

                _ ->
                    Ok decl
    in
    -- Dict.foldl
    --     doOne
    --     Dict.empty
    --     set
    DerefDeclMissing "" |> ResultME.error


type alias State pos =
    Result L3Error ( String, Declarable pos L2.RefChecked )


step : L2 pos -> Step (State pos)
step model state =
    case state of
        Err _ ->
            []

        Ok decl ->
            case decl of
                ( name, DAlias _ _ (TNamed _ _ nextName _) ) ->
                    []

                _ ->
                    []



-- Partial projections as expectations.
--
-- TODO:
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
            NotExpectedKind (declarableConsName decl) "DAlias" |> ResultME.error



-- TODO:
-- = TUnit pos Properties
-- | TBasic pos Properties Basic
-- | TNamed pos Properties String ref
-- | TContainer pos Properties (Container pos ref)
-- | TFunction pos Properties (Type pos ref) (Type pos ref)


expectProduct : Type pos ref -> ResultME L3Error ( pos, Properties, Nonempty (Field pos ref) )
expectProduct l1type =
    case l1type of
        TProduct pos props fields ->
            Ok ( pos, props, fields )

        _ ->
            NotExpectedKind "TProduct" (typeConsName l1type) |> ResultME.error


expectProductOrEmpty : Type pos ref -> ResultME L3Error ( pos, Properties, List (Field pos ref) )
expectProductOrEmpty l1type =
    case l1type of
        TProduct pos props fields ->
            Ok ( pos, props, Nonempty.toList fields )

        TEmptyProduct pos props ->
            Ok ( pos, props, [] )

        _ ->
            NotExpectedKind "TProduct" (typeConsName l1type) |> ResultME.error



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



-- Helpers


declarableConsName : Declarable pos ref -> String
declarableConsName decl =
    case decl of
        DAlias _ _ _ ->
            "DAlias"

        DSum _ _ _ ->
            "DSum"

        DEnum _ _ _ ->
            "DEnum"

        DRestricted _ _ _ ->
            "DRestricted"


typeConsName : Type pos ref -> String
typeConsName l1type =
    case l1type of
        TUnit _ _ ->
            "TUnit"

        TBasic _ _ _ ->
            "TBasic"

        TNamed _ _ _ _ ->
            "TNamed"

        TProduct _ _ _ ->
            "TProduct"

        TEmptyProduct _ _ ->
            "TEmptyProduct"

        TContainer _ _ _ ->
            "TContainer"

        TFunction _ _ _ _ ->
            "TFunction"
