module L3 exposing (..)

import Dict exposing (Dict)
import L1 exposing (Properties)
import L2 exposing (L2)
import ResultME exposing (ResultME)


{-| API for an L3 model processor.
-}
type alias L3 pos err =
    { name : String
    , defaults : Properties
    , check : L2 pos -> ResultME err (L2 pos)
    , errorToString : (pos -> String) -> pos -> err -> String
    }



-- TODO:
-- Stacking of properties of multiple processors
-- stack : Properties -> Properties -> ResultME err Properties
--
-- Reporting of defaults throughout the entire model.
-- On declarations.
-- On fields.
