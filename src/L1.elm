module L1 exposing
    ( Basic(..), Container(..), Field, Type(..), Restricted(..), Declarable(..), L1
    , PropSpec(..), Property(..), Properties, PropSpecs
    , defineProperties, emptyProperties
    , Unchecked(..)
    , positionOfDeclarable, positionOfType, propertiesOfDeclarable
    , updatePropertiesOfDeclarable
    , declarableConsName, typeConsName
    )

{-| Defines the level 1 language for data modelling.


# The L1 data modelling AST.

@docs Basic, Container, Field, Type, Restricted, Declarable, L1


# Properties that can be held against the L1 mode.

@docs PropSpec, Property, Properties, PropSpecs
@docs defineProperties, emptyProperties


# Ref checking status - L1 is unchecked.

@docs Unchecked


# Helper functions for extracting info from the L1 model.

@docs positionOfDeclarable, positionOfType, propertiesOfDeclarable
@docs updatePropertiesOfDeclarable


# Meta information on the model.

@docs declarableConsName, typeConsName

-}

import Dict exposing (Dict)
import Enum exposing (Enum)
import List.Nonempty exposing (Nonempty)


{-| The basic data types.
-}
type Basic
    = BBool
    | BInt
    | BReal
    | BString


{-| Containers for 0..n instances of some type.
-}
type Container pos ref
    = CList (Type pos ref)
    | CSet (Type pos ref)
    | CDict (Type pos ref) (Type pos ref)
    | COptional (Type pos ref)


{-| Re-usable field definition.
-}
type alias Field pos ref =
    ( String, Type pos ref, Properties )


{-| The possible type constructs.
-}
type Type pos ref
    = TUnit pos
    | TBasic pos Basic
    | TNamed pos String ref
    | TProduct pos (Nonempty (Field pos ref))
    | TEmptyProduct pos
    | TContainer pos (Container pos ref)
    | TFunction pos (Type pos ref) (Type pos ref)


{-| Restricted forms that are subsets of the basic data types.
-}
type Restricted
    = RInt
        { min : Maybe Int
        , max : Maybe Int
        , width : Maybe Int
        }
    | RString
        { minLength : Maybe Int
        , maxLength : Maybe Int
        , regex : Maybe String
        }


{-| Things that can be declared as named constructs.
-}
type Declarable pos ref
    = DAlias pos Properties (Type pos ref)
    | DSum pos Properties (Nonempty ( String, List (Field pos ref) ))
    | DEnum pos Properties (Nonempty String)
    | DRestricted pos Properties Restricted


{-| L1 is a list of unchecked declarables.
-}
type alias L1 pos =
    List ( String, Declarable pos Unchecked )



-- Additional model properties.


{-| Defines the kinds of additional property that can be placed in the model.
-}
type PropSpec
    = PSString
    | PSEnum (Enum String)
    | PSQName
    | PSBool
    | PSOptional PropSpec


{-| A set of additional property kinds that can or must be defined against
the model.
-}
type alias PropSpecs =
    Dict String PropSpec


{-| Allows additional properties from a variety of possible kinds to be placed
in the model.
-}
type Property
    = PString String
    | PEnum (Enum String) String
    | PQName (List String)
    | PBool Bool


{-| A set of additional properties on the model.
-}
type alias Properties =
    Dict String Property


{-| Creates an empty set of properties.
-}
emptyProperties : Properties
emptyProperties =
    Dict.empty


{-| Defines a set of property specifications with possible defaults. The first argument is a
list of property specs, and the second is a list of default values.

Values may appear in the second argument that are not in the first, in which case a specification
for them will be infered.

Values in the second argument that are also in the first should be of the same kind, but will be
overriden by the second argument if not.

-}
defineProperties : List ( String, PropSpec ) -> List ( String, Property ) -> ( PropSpecs, Properties )
defineProperties notSet set =
    let
        notSetPropSpecs =
            List.foldl
                (\( name, spec ) accum -> Dict.insert name spec accum)
                Dict.empty
                notSet

        ( fullPropSpecs, properties ) =
            List.foldl
                (\( name, property ) ( specsAccum, propsAccum ) ->
                    ( Dict.insert name (asPropSpec property) specsAccum
                    , Dict.insert name property propsAccum
                    )
                )
                ( notSetPropSpecs, Dict.empty )
                set

        asPropSpec property =
            case property of
                PString _ ->
                    PSString

                PEnum enum _ ->
                    PSEnum enum

                PQName _ ->
                    PSQName

                PBool _ ->
                    PSBool
    in
    ( fullPropSpecs, properties )



-- Model reference or property checking.


{-| Indicates that the model has not been checked.
-}
type Unchecked
    = Unchecked



-- Helper functions for extracting info.


{-| Gets the properties from a Declarable.
-}
propertiesOfDeclarable : Declarable pos ref -> Properties
propertiesOfDeclarable decl =
    case decl of
        DAlias _ props _ ->
            props

        DSum _ props _ ->
            props

        DEnum _ props _ ->
            props

        DRestricted _ props _ ->
            props


{-| Updates the properties from a Declarable.
-}
updatePropertiesOfDeclarable : (Properties -> Properties) -> Declarable pos ref -> Declarable pos ref
updatePropertiesOfDeclarable propsFn decl =
    case decl of
        DAlias pos props def ->
            DAlias pos (propsFn props) def

        DSum pos props constructors ->
            DSum pos (propsFn props) constructors

        DEnum pos props enums ->
            DEnum pos (propsFn props) enums

        DRestricted pos props def ->
            DRestricted pos (propsFn props) def


{-| Gets the position context from a Declarable.
-}
positionOfDeclarable : Declarable pos ref -> pos
positionOfDeclarable decl =
    case decl of
        DAlias pos _ _ ->
            pos

        DSum pos _ _ ->
            pos

        DEnum pos _ _ ->
            pos

        DRestricted pos _ _ ->
            pos


{-| Gets the position context from a Type.
-}
positionOfType : Type pos ref -> pos
positionOfType type_ =
    case type_ of
        TUnit pos ->
            pos

        TBasic pos _ ->
            pos

        TNamed pos _ _ ->
            pos

        TProduct pos _ ->
            pos

        TEmptyProduct pos ->
            pos

        TContainer pos _ ->
            pos

        TFunction pos _ _ ->
            pos


{-| Yields the constructor name for a `Declarable`.
-}
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


{-| Yields the constructor name for a `Type`.
-}
typeConsName : Type pos ref -> String
typeConsName l1type =
    case l1type of
        TUnit _ ->
            "TUnit"

        TBasic _ _ ->
            "TBasic"

        TNamed _ _ _ ->
            "TNamed"

        TProduct _ _ ->
            "TProduct"

        TEmptyProduct _ ->
            "TEmptyProduct"

        TContainer _ _ ->
            "TContainer"

        TFunction _ _ _ ->
            "TFunction"
