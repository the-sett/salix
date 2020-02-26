module L2 exposing
    ( L2
    , RefChecked(..)
    )

{-| Defines the level 2 language for data models that have been checked for
consitency, and have been processed for general data modelling concerns without
regard to specific code generation.


# The L2 data modelling language.

@docs L2


# Ref checking status - L2 is checked.

@docs RefChecked

-}

import Dict exposing (Dict)
import L1 exposing (Declarable)


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
