module L2 exposing (L2, RefChecked(..))

import Dict exposing (Dict)
import L1 exposing (Declarable)



-- TODO:
-- Matching up of bi-directional references. When only one on each end it is obvious.
-- When more than one, target needs to be explicit. Is this an L1 concern?


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
