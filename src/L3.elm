module L3 exposing (..)

import Dict exposing (Dict)
import Enum exposing (Enum)
import L1 exposing (Declarable, PropSpec(..), PropSpecs, Properties, Property(..))
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
    { properties : Properties
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



--== Reading properties.


{-| Once properties have been checked, then reading properties as per the property
specification should always succeed, since those properties have been verified to be
present and of the correct kind.

If reading a property fails, it is a coding error and should be reported as a bug.
This error type enumerates the possible properties bugs.

-}
type PropCheckError
    = CheckedPropertyMissing
    | CheckedPropertyWrongKind


propCheckErrorToString : PropCheckError -> String
propCheckErrorToString err =
    case err of
        CheckedPropertyMissing ->
            "Checked property missing."

        CheckedPropertyWrongKind ->
            "Checked property wrong kind."


getProperty : PropSpec -> String -> Properties -> ResultME PropCheckError Property
getProperty spec name props =
    let
        maybeProp =
            Dict.get name props
    in
    case ( spec, maybeProp ) of
        ( PSString, Just (PString val) ) ->
            PString val |> Ok

        ( PSEnum _, Just (PEnum enum val) ) ->
            PEnum enum val |> Ok

        ( PSQName, Just (PQName path val) ) ->
            PQName path val |> Ok

        ( PSBool, Just (PBool val) ) ->
            PBool val |> Ok

        ( PSOptional propSpec, Just (POptional optSpec maybe) ) ->
            POptional spec maybe |> Ok

        ( _, Nothing ) ->
            CheckedPropertyMissing |> ResultME.error

        ( _, _ ) ->
            CheckedPropertyWrongKind |> ResultME.error


getStringProperty : String -> Properties -> ResultME PropCheckError String
getStringProperty name props =
    case getProperty PSString name props of
        Ok (PString val) ->
            Ok val

        _ ->
            CheckedPropertyWrongKind |> ResultME.error


getEnumProperty : Enum String -> String -> Properties -> ResultME PropCheckError String
getEnumProperty enum name props =
    case getProperty (PSEnum enum) name props of
        Ok (PEnum _ val) ->
            Ok val

        _ ->
            CheckedPropertyWrongKind |> ResultME.error


getQNameProperty : String -> Properties -> ResultME PropCheckError ( List String, String )
getQNameProperty name props =
    case getProperty PSQName name props of
        Ok (PQName path val) ->
            Ok ( path, val )

        _ ->
            CheckedPropertyWrongKind |> ResultME.error



-- getBoolProperty : String -> Properties -> Result PropCheckError Bool
