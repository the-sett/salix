module L3 exposing (..)

import Dict exposing (Dict)
import L2 exposing (L2)



--== Values of extra attributes that can be attached to a model
-- Enums?
-- Floats for max/min


type Property
    = PInt Int
    | PString String



-- Should use generic Value here?
-- What about restricted values such as enum?
-- type Json
--     = JString String
--     | JBool Bool
--     | JInt Int
--     | JFloat Float
--     | JNull
--     | JObj (Dict String Json)
--     | JArr (List Json)
--== A set of properties that can be attached to the model.


type alias Properties =
    Dict String Property



-- Alternatively
-- type alias Property = (String, Kind)
-- type alias Properties = Dict Property Value
--== Stacking


type alias Stack =
    Properties -> Properties -> Properties



--== Defaults


type alias Default =
    Properties



--== Declaring properties and types up-front
--== API for an L3 code generator


type alias L3 pos =
    { defaults : Properties
    , available : Properties
    , generate : L2 pos -> String
    }
