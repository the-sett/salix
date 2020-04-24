module L2 exposing
    ( L2
    , RefChecked(..)
    , Processor, ProcessorImpl, builder
    )

{-| Defines the level 2 language for data models that have been checked for
consitency, and have been processed for general data modelling concerns without
regard to specific code generators.


# The L2 data modelling language.

@docs L2


# Ref checking status - L2 is checked.

@docs RefChecked


# Standardized interface to an L2 processor.

@docs Processor, ProcessorImpl, builder

-}

import Dict exposing (Dict)
import Errors exposing (Error, ErrorBuilder)
import L1 exposing (Declarable, L1)
import ResultME exposing (ResultME)
import SourcePos exposing (SourceLines)


{-| Indicates that named types in the model have been reference checked to
enusure that they name something that actually exists in the L2 dictionary.

Some summary information of what they refer to is also cached during ref
checking. This can be very convenient during code generation to avoid having
to look up in the dictionary all the time, to find out what kind of thing a
named type is.

-}
type RefChecked
    = RcEnum
    | RcRestricted L1.Basic
    | RcSum
    | RcTUnit
    | RcTBasic L1.Basic
    | RcTNamed -- TODO: Should recursively explore this?
    | RcTProduct
    | RcTEmptyProduct
    | RcTContainer
    | RcTFunction


{-| L2 is a dictionary of named data model components, which has also been
reference checked to ensure that all references in the model have named
entries in the dictionary.
-}
type alias L2 pos =
    Dict String (Declarable pos RefChecked)


{-| API for an L2 model processor.
-}
type alias Processor pos =
    { name : String
    , check : L1 pos -> ResultME Error (L2 pos)
    }


{-| Builds an L2 Processor API from an implementation. A function to turn
source code positions into quoted lines of source code needs to be supplied.
-}
builder : (pos -> SourceLines) -> ProcessorImpl pos err -> Processor pos
builder posFn impl =
    { name = impl.name
    , check = impl.check >> ResultME.mapError (impl.buildError posFn)
    }


{-| SPI for an L2 model processor. Use the `builder` to turn one of these into
a `Processor`.
-}
type alias ProcessorImpl pos err =
    { name : String
    , check : L1 pos -> ResultME err (L2 pos)
    , buildError : ErrorBuilder pos err
    }
