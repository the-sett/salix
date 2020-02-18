module L1 exposing
    ( Basic(..)
    , Container(..)
    , Declarable(..)
    , L1
    , Properties
    , Property(..)
    , Restricted(..)
    , Type(..)
    , Unchecked(..)
    , positionOfDeclarable
    , positionOfType
    )

import Dict exposing (Dict)
import Enum exposing (Enum)
import List.Nonempty exposing (Nonempty)



-- TODO:
-- Need to be able to say what kind of property something is, without giving
-- a value for it.


type Basic
    = BBool
    | BInt
    | BReal
    | BString


type Container pos ref
    = CList (Type pos ref)
    | CSet (Type pos ref)
    | CDict (Type pos ref) (Type pos ref)
    | COptional (Type pos ref)


type Type pos ref
    = TUnit pos
    | TBasic pos Basic
    | TNamed pos String ref
    | TProduct pos (Nonempty ( String, Type pos ref )) -- Properties on fields.
    | TEmptyProduct pos
    | TContainer pos (Container pos ref)
    | TFunction pos (Type pos ref) (Type pos ref)


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


type Declarable pos ref
    = DAlias pos (Type pos ref)
    | DSum pos (Nonempty ( String, List ( String, Type pos ref ) )) -- Properties on fields.
    | DEnum pos (Nonempty String)
    | DRestricted pos Restricted



-- Additional model properties.


{-| Defines the type of additional property that can be placed in the model.
-}
type Property
    = PString String
    | PEnum (Enum String)
    | PQName (List String) String
    | PBool Bool
    | POptional (Maybe Property) -- Needs to say what kind it is..


{-| A set of additional properties on the model.
-}
type alias Properties =
    Dict String Property



-- Model property checking.


{-| Indicates that the model has not been reference checked.
-}
type Unchecked
    = Unchecked



-- Helper functions for extracting position info.


positionOfDeclarable : Declarable pos ref -> pos
positionOfDeclarable decl =
    case decl of
        DAlias pos _ ->
            pos

        DSum pos _ ->
            pos

        DEnum pos _ ->
            pos

        DRestricted pos _ ->
            pos


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


{-| The L1 model
-}
type alias L1 pos =
    List ( String, Declarable pos Unchecked )
