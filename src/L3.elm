module L3 exposing (..)

import Dict exposing (Dict)
import L1 exposing (Properties)
import L2 exposing (L2)
import ResultME exposing (ResultME)



--L3 = L1 but with checked properties


{-| API for an L3 model processor.
-- Rename to processor
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
