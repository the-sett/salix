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


type Container a
    = CList (Type a)
    | CSet (Type a)
    | CDict (Type a) (Type a)
    | COptional (Type a)


type Type a
    = TUnit
    | TBasic Basic
    | TNamed String a
    | TProduct (Nonempty ( String, Type a ))
    | TEmptyProduct
    | TContainer (Container a)
    | TFunction (Type a) (Type a)


type Restricted
    = RInt { min : Maybe Int, max : Maybe Int, width : Maybe Int }
    | RString { minLength : Maybe Int, maxLength : Maybe Int, regex : Maybe String }


type Declarable a
    = DAlias (Type a)
    | DSum (Nonempty ( String, List ( String, Type a ) ))
    | DEnum (Nonempty String)
    | DRestricted Restricted


{-| Indicates that the model has not been reference checked.
-}
type Unchecked
    = Unchecked



-- The L1 model


type alias L1 =
    List ( String, Declarable Unchecked )
