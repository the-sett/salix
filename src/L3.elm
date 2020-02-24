module L3 exposing
    ( DefaultProperties
    , L3
    , Processor
    , PropCheckError(..)
    , PropertiesAPI
    , PropertyGet
    , makePropertiesAPI
    , propCheckErrorToString
    )

import Dict exposing (Dict)
import Enum exposing (Enum)
import L1 exposing (Declarable(..), PropSpec(..), PropSpecs, Properties, Property(..))
import L2 exposing (L2, RefChecked)
import ResultME exposing (ResultME)



-- TODO:
-- Stacking of properties of multiple processors
-- stack : Properties -> Properties -> ResultME err Properties


{-| Allows the default properties on parts of the model to be defined.
-}
type alias DefaultProperties =
    { top : ( PropSpecs, Properties )
    , alias : ( PropSpecs, Properties )
    , sum : ( PropSpecs, Properties )
    , enum : ( PropSpecs, Properties )
    , restricted : ( PropSpecs, Properties )
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


makePropertiesAPI : DefaultProperties -> L3 pos -> PropertiesAPI pos
makePropertiesAPI defaultProperties l3 =
    { top = makePropertyGet (Tuple.second defaultProperties.top) l3.properties
    , declarable =
        \decl ->
            case decl of
                DAlias _ _ props ->
                    makePropertyGet (Tuple.second defaultProperties.alias) props

                DSum _ _ props ->
                    makePropertyGet (Tuple.second defaultProperties.sum) props

                DEnum _ _ props ->
                    makePropertyGet (Tuple.second defaultProperties.enum) props

                DRestricted _ _ props ->
                    makePropertyGet (Tuple.second defaultProperties.restricted) props
    , field = makePropertyGet (Tuple.second defaultProperties.fields)
    }


makePropertyGet : Properties -> Properties -> PropertyGet
makePropertyGet defaults props =
    { getStringProperty = getStringProperty defaults props
    , getEnumProperty = getEnumProperty defaults props
    , getQNameProperty = getQNameProperty defaults props
    , getBoolProperty = getBoolProperty defaults props
    , getOptionalStringProperty = getOptionalStringProperty defaults props
    , getOptionalEnumProperty = getOptionalEnumProperty defaults props
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


getWithDefault : Properties -> Properties -> String -> Maybe Property
getWithDefault defaults props name =
    case Dict.get name props of
        Nothing ->
            Dict.get name defaults

        justVal ->
            justVal


getProperty : Properties -> Properties -> PropSpec -> String -> ResultME PropCheckError Property
getProperty defaults props spec name =
    let
        maybeProp =
            getWithDefault defaults props name
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

        ( PSOptional _, Just (POptional optSpec maybe) ) ->
            POptional spec maybe |> Ok

        ( _, Nothing ) ->
            CheckedPropertyMissing |> ResultME.error

        ( _, _ ) ->
            CheckedPropertyWrongKind |> ResultME.error


getStringProperty : Properties -> Properties -> String -> ResultME PropCheckError String
getStringProperty defaults props name =
    case getProperty defaults props PSString name of
        Ok (PString val) ->
            Ok val

        _ ->
            CheckedPropertyWrongKind |> ResultME.error


getEnumProperty : Properties -> Properties -> Enum String -> String -> ResultME PropCheckError String
getEnumProperty defaults props enum name =
    case getProperty defaults props (PSEnum enum) name of
        Ok (PEnum _ val) ->
            Ok val

        _ ->
            CheckedPropertyWrongKind |> ResultME.error


getQNameProperty : Properties -> Properties -> String -> ResultME PropCheckError ( List String, String )
getQNameProperty defaults props name =
    case getProperty defaults props PSQName name of
        Ok (PQName path val) ->
            Ok ( path, val )

        _ ->
            CheckedPropertyWrongKind |> ResultME.error


getBoolProperty : Properties -> Properties -> String -> ResultME PropCheckError Bool
getBoolProperty defaults props name =
    case getProperty defaults props PSQName name of
        Ok (PBool val) ->
            Ok val

        _ ->
            CheckedPropertyWrongKind |> ResultME.error


getOptionalStringProperty : Properties -> Properties -> String -> ResultME PropCheckError (Maybe String)
getOptionalStringProperty defaults props name =
    case getProperty defaults props (PSOptional PSString) name of
        Ok (POptional PSString maybeProp) ->
            case maybeProp of
                Nothing ->
                    Ok Nothing

                Just (PString val) ->
                    Just val |> Ok

                _ ->
                    CheckedPropertyWrongKind |> ResultME.error

        _ ->
            CheckedPropertyWrongKind |> ResultME.error


getOptionalEnumProperty : Properties -> Properties -> Enum String -> String -> ResultME PropCheckError (Maybe String)
getOptionalEnumProperty defaults props enum name =
    case getProperty defaults props (PSEnum enum) name of
        Ok (POptional (PSEnum _) maybeProp) ->
            case maybeProp of
                Nothing ->
                    Ok Nothing

                Just (PEnum _ val) ->
                    Just val |> Ok

                _ ->
                    CheckedPropertyWrongKind |> ResultME.error

        _ ->
            CheckedPropertyWrongKind |> ResultME.error



---


type alias PropertyGet =
    { getStringProperty : String -> ResultME PropCheckError String
    , getEnumProperty : Enum String -> String -> ResultME PropCheckError String
    , getQNameProperty : String -> ResultME PropCheckError ( List String, String )
    , getBoolProperty : String -> ResultME PropCheckError Bool
    , getOptionalStringProperty : String -> ResultME PropCheckError (Maybe String)
    , getOptionalEnumProperty : Enum String -> String -> ResultME PropCheckError (Maybe String)
    }


type alias PropertiesAPI pos =
    { top : PropertyGet
    , declarable : Declarable pos RefChecked -> PropertyGet
    , field : Properties -> PropertyGet
    }
