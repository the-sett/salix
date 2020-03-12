module L2 exposing
    ( L2
    , RefChecked(..)
    , Processor, ProcessorImpl, builder
    )

{-| Defines the level 2 language for data models that have been checked for
consitency, and have been processed for general data modelling concerns without
regard to specific code generation.


# The L2 data modelling language.

@docs L2


# Ref checking status - L2 is checked.

@docs RefChecked


# Standardized interface to an L2 processor.

@docs Processor, ProcessorImpl, builder

-}

import Dict exposing (Dict)
import Errors exposing (Error, ErrorBuilder, SourceLines)
import L1 exposing (Declarable, L1)
import ResultME exposing (ResultME)


{-| Indicates that named type in the model have been reference checked to
enusure that they name something that actually exists.

Some summary information of what they refer to is also cached during ref
checking.

-}
type RefChecked
    = RcEnum
    | RcRestricted L1.Basic
    | RcSum
    | RcTUnit
    | RcTBasic
    | RcTNamed
    | RcTProduct
    | RcTEmptyProduct
    | RcTContainer
    | RcTFunction


{-| The L2 model
-}
type alias L2 pos =
    Dict String (Declarable pos RefChecked)


{-| API for an L2 model processor.
-}
type alias Processor pos =
    { name : String
    , check : L1 pos -> ResultME Error (L2 pos)
    }


builder : (pos -> SourceLines) -> ProcessorImpl pos err -> Processor pos
builder posFn impl =
    { name = impl.name
    , check = impl.check >> ResultME.mapError (impl.buildError posFn)
    }


{-| SPI for an L2 model processor.
-}
type alias ProcessorImpl pos err =
    { name : String
    , check : L1 pos -> ResultME err (L2 pos)
    , buildError : ErrorBuilder pos err
    }
