module Query exposing
    ( deref
    , transitiveClosure, transitiveClosureWithoutStartingSet
    , transitiveClosureOfType
    , expectAlias, expectProduct, expectProductOrEmpty
    , PropertyFilter, andPropFilter, notPropFilter, orPropFilter
    , filterDictByProps, filterListByProps, filterNonemptyByProps
    , propertiesApiWithoutDefaults
    )

{-| Functions for querying Salix models.


# Dereferencing named type aliases.

@docs deref


# Find dependency sets.

@docs transitiveClosure, transitiveClosureWithoutStartingSet
@docs transitiveClosureOfType


# Partial projections as expectations.

@docs expectAlias, expectProduct, expectProductOrEmpty


# Filtering by properties.

@docs PropertyFilter, andPropFilter, notPropFilter, orPropFilter
@docs filterDictByProps, filterListByProps, filterNonemptyByProps
@docs propertiesApiWithoutDefaults

-}

import Dict exposing (Dict)
import L1 exposing (Container(..), Declarable(..), Field, Properties, Type(..))
import L2 exposing (L2, RefChecked)
import L3 exposing (L3, L3Error(..), PropertiesAPI)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import ResultME exposing (ResultME)
import Search exposing (SearchResult(..), Step)
import Set exposing (Set)



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
Any type references in these declarations will pull their referred to declarations
into the closure, and this process will be continued recursively until no more members
are added to the closure.

This is useful if some declarations need to be generated from, including all their
dependencies. For example, to code generate an encoder for a declaration that references
other types by name, the encoders for those other types also need to be generated. The
transitive closure gives the full set of declaration to generate encoders for to complete
the code.

The first argument is the starting set, and the second argument is the complete model to
select a transitive closure from.

    transitiveClosure startingSet model

-}
transitiveClosure : L2 pos -> L2 pos -> ResultME L3Error (L2 pos)
transitiveClosure set model =
    transitiveClosureInner True set model


{-| Finds the transitive closure starting from a sub-set of declarations from an L2.
Any type references in these declarations will pull their referred to declarations
into the closure, and this process will be continued recursively until no more members
are added to the closure.

The starting set itself will not be automatically included in the results. Members of
the starting set may end up in the results, but only if they are dependencies of other
members of the starting set.

This is useful if some declarations need to be generated from, including all their
dependencies. For example, to code generate an encoder for a declaration that references
other types by name, the encoders for those other types also need to be generated. The
transitive closure gives the full set of declaration to generate encoders for to complete
the code.

The first argument is the starting set, and the second argument is the complete model to
select a transitive closure from.

-}
transitiveClosureWithoutStartingSet : L2 pos -> L2 pos -> ResultME L3Error (L2 pos)
transitiveClosureWithoutStartingSet set model =
    transitiveClosureInner False set model


{-| The transitive closure algorith, by depth first search.

TODO: Make the buffer a Set keyed by declaration name. That will remove duplicates from the
search but probably not all.
TODO: Add the allGoals function to the Search package.

-}
transitiveClosureInner : Bool -> L2 pos -> L2 pos -> ResultME L3Error (L2 pos)
transitiveClosureInner keepStartingSet set model =
    let
        crumbs =
            if keepStartingSet then
                Dict.keys set |> Set.fromList

            else
                Set.empty

        start =
            Dict.toList set
                |> List.map (\( name, decl ) -> { name = name, decl = decl, crumbs = crumbs })
                |> List.map Ok
                |> List.map (\val -> ( val, keepStartingSet ))
    in
    Search.depthFirst { cost = always 1.0, step = step model } start
        |> allGoals
        |> ResultME.combineList
        |> ResultME.map (List.map (\{ name, decl } -> ( name, decl )))
        |> ResultME.map Dict.fromList


{-| Starting from a type, computes the transitive closure from all outoing TNamed
references within that type. This yields all declarations that a type depends on.
-}
transitiveClosureOfType : Type pos L2.RefChecked -> L2 pos -> ResultME L3Error (L2 pos)
transitiveClosureOfType l2type model =
    let
        start =
            stepType Set.empty model l2type []
    in
    Search.depthFirst { cost = always 1.0, step = step model } start
        |> allGoals
        |> ResultME.combineList
        |> ResultME.map (List.map (\{ name, decl } -> ( name, decl )))
        |> ResultME.map Dict.fromList


allGoals : SearchResult (State pos) -> List (State pos)
allGoals result =
    let
        innerAllGoals innerResult accum =
            case innerResult of
                Complete ->
                    accum

                Goal state moreFn ->
                    -- let
                    --     _ =
                    --         Debug.log "\nFound goal: " state
                    -- in
                    innerAllGoals (moreFn ()) (state :: accum)

                Ongoing state moreFn ->
                    innerAllGoals (moreFn ()) accum
    in
    innerAllGoals result []


type alias State pos =
    ResultME L3Error
        { name : String
        , decl : Declarable pos L2.RefChecked
        , crumbs : Set String
        }


step : L2 pos -> Step (State pos)
step model state =
    case state of
        Err _ ->
            []

        Ok { name, decl, crumbs } ->
            stepDecl (Set.insert name crumbs) model decl []


stepDecl : Set String -> L2 pos -> Declarable pos L2.RefChecked -> List ( State pos, Bool ) -> List ( State pos, Bool )
stepDecl crumbs model decl accum =
    case decl of
        DAlias _ _ atype ->
            stepType crumbs model atype accum

        DSum _ _ constructors ->
            Nonempty.foldl
                (\( _, fields ) consAccum ->
                    List.foldl
                        (\( _, fType, _ ) fieldAccum -> stepType crumbs model fType fieldAccum)
                        consAccum
                        fields
                )
                accum
                constructors

        _ ->
            accum


stepType : Set String -> L2 pos -> Type pos L2.RefChecked -> List ( State pos, Bool ) -> List ( State pos, Bool )
stepType crumbs model l2type accum =
    case l2type of
        TNamed _ _ refName _ ->
            case Set.member refName crumbs of
                True ->
                    accum

                False ->
                    case Dict.get refName model of
                        Just val ->
                            ( Ok
                                { name = refName
                                , decl = val
                                , crumbs = crumbs
                                }
                            , True
                            )
                                :: accum

                        Nothing ->
                            ( DerefDeclMissing refName |> ResultME.error, True ) :: accum

        TProduct _ _ fields ->
            Nonempty.foldl
                (\( _, fType, _ ) fieldAccum -> stepType crumbs model fType fieldAccum)
                accum
                fields

        TContainer _ _ container ->
            stepContainer crumbs model container accum

        TFunction _ _ fromType toType ->
            stepType crumbs model toType (stepType crumbs model fromType accum)

        _ ->
            accum


stepContainer : Set String -> L2 pos -> Container pos L2.RefChecked -> List ( State pos, Bool ) -> List ( State pos, Bool )
stepContainer crumbs model container accum =
    case container of
        CList l2type ->
            stepType crumbs model l2type accum

        CSet l2type ->
            stepType crumbs model l2type accum

        CDict keyType valType ->
            stepType crumbs model valType (stepType crumbs model keyType accum)

        COptional l2type ->
            stepType crumbs model l2type accum



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
            NotExpectedKind (L1.declarableConsName decl) "DAlias" |> ResultME.error



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
            NotExpectedKind "TProduct" (L1.typeConsName l1type) |> ResultME.error


expectProductOrEmpty : Type pos ref -> ResultME L3Error ( pos, Properties, List (Field pos ref) )
expectProductOrEmpty l1type =
    case l1type of
        TProduct pos props fields ->
            Ok ( pos, props, Nonempty.toList fields )

        TEmptyProduct pos props ->
            Ok ( pos, props, [] )

        _ ->
            NotExpectedKind "TProduct" (L1.typeConsName l1type) |> ResultME.error



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


{-| This makes a properties API on top of an empty set of defaults and specs.

This is useful when you don't have an L3, but want to run queries over properties.
There may be intermediate properties that have been created during some processing
that you need to query over, for example.

-}
propertiesApiWithoutDefaults : L2 pos -> PropertiesAPI pos
propertiesApiWithoutDefaults l2 =
    L3.makePropertiesAPI L3.emptyDefaultProperties
        { properties = L1.emptyProperties
        , declarations = l2
        }
