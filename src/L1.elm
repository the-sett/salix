module L1 exposing
    ( Basic(..), Container(..), Type(..), Restricted(..), Declarable(..), L1
    , PropSpec(..), Property(..), Properties, PropSpecs
    , defineProperties, emptyProperties
    , Unchecked(..)
    , positionOfDeclarable, positionOfType, propertiesOfDeclarable, propertiesOfType
    , updatePropertiesOfType
    )

{-| Defines the level 1 language for data modelling.


# The L1 data modelling AST.

@docs Basic, Container, Type, Restricted, Declarable, L1


# Properties that can be held against the L1 mode.

@docs PropSpec, Property, Properties, PropSpecs
@docs defineProperties, emptyProperties


# Ref checking status - L1 is unchecked.

@docs Unchecked


# Helper functions for extracting info from the L1 model.

@docs positionOfDeclarable, positionOfType, propertiesOfDeclarable, propertiesOfType
@docs updatePropertiesOfType

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


{-| The possible type constructs.
-}
type Type pos ref
    = TUnit pos Properties
    | TBasic pos Properties Basic
    | TNamed pos Properties String ref
    | TProduct pos Properties (Nonempty ( String, Type pos ref, Properties ))
    | TEmptyProduct pos Properties
    | TContainer pos Properties (Container pos ref)
    | TFunction pos Properties (Type pos ref) (Type pos ref)


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
    | DSum pos Properties (Nonempty ( String, List ( String, Type pos ref, Properties ) ))
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
    | POptional PropSpec (Maybe Property)


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

                POptional spec _ ->
                    PSOptional spec
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


{-| Gets the properties from a Type.
-}
propertiesOfType : Type pos ref -> Properties
propertiesOfType type_ =
    case type_ of
        TUnit _ props ->
            props

        TBasic _ props _ ->
            props

        TNamed _ props _ _ ->
            props

        TProduct _ props _ ->
            props

        TEmptyProduct _ props ->
            props

        TContainer _ props _ ->
            props

        TFunction _ props _ _ ->
            props


{-| Updates the properties from a Type.
-}
updatePropertiesOfType : (Properties -> Properties) -> Type pos ref -> Type pos ref
updatePropertiesOfType propsFn type_ =
    case type_ of
        TUnit pos props ->
            TUnit pos (propsFn props)

        TBasic pos props basic ->
            TBasic pos (propsFn props) basic

        TNamed pos props name ref ->
            TNamed pos (propsFn props) name ref

        TProduct pos props fields ->
            TProduct pos (propsFn props) fields

        TEmptyProduct pos props ->
            TEmptyProduct pos (propsFn props)

        TContainer pos props inner ->
            TContainer pos (propsFn props) inner

        TFunction pos props arg res ->
            TFunction pos (propsFn props) arg res


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
        TUnit pos _ ->
            pos

        TBasic pos _ _ ->
            pos

        TNamed pos _ _ _ ->
            pos

        TProduct pos _ _ ->
            pos

        TEmptyProduct pos _ ->
            pos

        TContainer pos _ _ ->
            pos

        TFunction pos _ _ _ ->
            pos
