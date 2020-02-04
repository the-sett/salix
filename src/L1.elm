module L1 exposing
    ( Basic(..)
    , Container(..)
    , Declarable(..)
    , Declarations
    , Restricted(..)
    , Type(..)
    , Unchecked(..)
    )

{-| L1 is a data modelling language.

The name L1 stands for 'level 1', as it is envisioned to be part of a sequence of
similar data modelling languages, 'level 2', 'level 3'. With L1 being completely
language agnostic, and higher levels bringing in more specific information targetting
code generation on particular languages or systems.

What you see here is a first pass at describing the L1 modelling language, and using
it in a practical context of generating AWS Service stubs for Elm.

-}

import Dict exposing (Dict)


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
    | TProduct (List ( String, Type a ))
    | TContainer (Container a)
    | TFunction (Type a) (Type a)



-- Do I want Restricted separate or merge into BInt and BString?


type Restricted
    = RInt { min : Maybe Int, max : Maybe Int, width : Maybe Int }
    | RString { minLength : Maybe Int, maxLength : Maybe Int, regex : Maybe String }


type Declarable a
    = DAlias (Type a)
    | DSum (List ( String, List ( String, Type a ) ))
    | DEnum (List String)
    | DRestricted Restricted



-- Indicates that the AST has not been reference checked.


type Unchecked
    = Unchecked



-- A set of declarations


type alias Declarations a =
    Dict String (Declarable a)


type alias L1 =
    Declarations Unchecked
