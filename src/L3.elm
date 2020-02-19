module L3 exposing (DefaultProperties, Processor, PropChecked(..))

import Dict exposing (Dict)
import L1 exposing (Declarable, PropSpecs, Properties)
import L2 exposing (L2, RefChecked)
import ResultME exposing (ResultME)



-- TODO:
-- Stacking of properties of multiple processors
-- stack : Properties -> Properties -> ResultME err Properties


{-| Indicates that all L3 properties have been checked to be of the correct type
and legitimate for an L3 processor.
-}
type PropChecked
    = PropChecked


{-| Allows the default properties on parts of the model to be defined.
-}
type alias DefaultProperties =
    { top : ( PropSpecs, Properties )
    , alias : ( PropSpecs, Properties )
    , sum : ( PropSpecs, Properties )
    , enum : ( PropSpecs, Properties )
    , fields : ( PropSpecs, Properties )
    }


{-| The L3 model
-}
type alias L3 pos =
    { topProperties : Properties
    , declarations : Dict String (Declarable pos RefChecked)
    }


{-| API for an L3 model processor.
-- Rename to processor
-}
type alias Processor pos err =
    { name : String
    , defaults : DefaultProperties
    , check : L3 pos -> ResultME err (L3 pos)
    , errorToString : (pos -> String) -> pos -> err -> String
    }
