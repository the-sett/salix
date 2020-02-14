module L1 exposing
    ( Basic(..)
    , Container(..)
    , Declarable(..)
    , L1
    , Restricted(..)
    , Type(..)
    , Unchecked(..)
    )

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)



-- TODO:
-- Source code locations for error reporting.
-- Somewhere to hold L3 properties
-- Matching up of bi-directional references. When only one on each end it is obvious.
-- When more than one, target needs to be explicit. Is this an L1 concern?


type Basic
    = BBool
    | BInt
    | BReal
    | BString


type Container ref
    = CList (Type ref)
    | CSet (Type ref)
    | CDict (Type ref) (Type ref)
    | COptional (Type ref)


type Type ref
    = TUnit
    | TBasic Basic
    | TNamed String ref
    | TProduct (Nonempty ( String, Type ref ))
    | TEmptyProduct
    | TContainer (Container ref)
    | TFunction (Type ref) (Type ref)


type Restricted
    = RInt { min : Maybe Int, max : Maybe Int, width : Maybe Int }
    | RString { minLength : Maybe Int, maxLength : Maybe Int, regex : Maybe String }


type Declarable ref
    = DAlias (Type ref)
    | DSum (Nonempty ( String, List ( String, Type ref ) ))
    | DEnum (Nonempty String)
    | DRestricted Restricted


{-| Indicates that the model has not been reference checked.
-}
type Unchecked
    = Unchecked



-- The L1 model


type alias L1 =
    List ( String, Declarable Unchecked )
