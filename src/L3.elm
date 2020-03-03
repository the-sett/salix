module L3 exposing
    ( L3
    , DefaultProperties, PropertiesAPI, PropertyGet, makePropertiesAPI
    , Processor, PropCheckError(..), propCheckErrorToString
    )

{-| Defines the level 3 language for data models that have been annotated with
predicates indicating that the model has particular features needed by particular
code generators. A level 3 language ensures the specific requirements for futher
processing by a code generator are being met.


# The L3 data modelling language.

@docs L3


# Defaulting of properties accross the data model, and APIs to read properties.

@docs DefaultProperties, PropertiesAPI, PropertyGet, makePropertiesAPI


# Definition of an L3 processor which creates some output from an L3 model.

@docs Processor, PropCheckError, propCheckErrorToString

-}

import Dict exposing (Dict)
import Enum exposing (Enum)
import Errors exposing (Error)
import L1 exposing (Declarable(..), PropSpec(..), PropSpecs, Properties, Property(..), Type(..))
import L2 exposing (L2, RefChecked)
import ResultME exposing (ResultME)


{-| Allows the default properties on parts of the model to be defined.
-}
type alias DefaultProperties =
    { top : ( PropSpecs, Properties )

    -- Declarables
    , alias : ( PropSpecs, Properties )
    , sum : ( PropSpecs, Properties )
    , enum : ( PropSpecs, Properties )
    , restricted : ( PropSpecs, Properties )

    -- Fields
    , fields : ( PropSpecs, Properties )

    -- Types
    , unit : ( PropSpecs, Properties )
    , basic : ( PropSpecs, Properties )
    , named : ( PropSpecs, Properties )
    , product : ( PropSpecs, Properties )
    , emptyProduct : ( PropSpecs, Properties )
    , container : ( PropSpecs, Properties )
    , function : ( PropSpecs, Properties )
    }


{-| The L3 model
-}
type alias L3 pos =
    { properties : Properties
    , declarations : Dict String (Declarable pos RefChecked)
    }


{-| API for an L3 model processor.
-}
type alias Processor pos err =
    { name : String
    , defaults : DefaultProperties
    , check : L3 pos -> ResultME err (L3 pos)
    , errorToString : (pos -> String) -> err -> Error
    }


{-| An API for reading properties of various expected kinds.
-}
type alias PropertyGet =
    { getStringProperty : String -> ResultME PropCheckError String
    , getEnumProperty : Enum String -> String -> ResultME PropCheckError String
    , getQNameProperty : String -> ResultME PropCheckError (List String)
    , getBoolProperty : String -> ResultME PropCheckError Bool
    , getOptionalStringProperty : String -> ResultME PropCheckError (Maybe String)
    , getOptionalEnumProperty : Enum String -> String -> ResultME PropCheckError (Maybe String)
    }


{-| An API for supplying property readers for various parts of the data model.
-}
type alias PropertiesAPI pos =
    { top : PropertyGet
    , declarable : Declarable pos RefChecked -> PropertyGet
    , field : Properties -> PropertyGet
    , type_ : Type pos RefChecked -> PropertyGet
    }


{-| Creates a properties API from a set of defaulted property specs, and an
L3 model.
-}
makePropertiesAPI : DefaultProperties -> L3 pos -> PropertiesAPI pos
makePropertiesAPI defaultProperties l3 =
    { top = makePropertyGet (Tuple.second defaultProperties.top) l3.properties
    , declarable =
        \decl ->
            case decl of
                DAlias _ props _ ->
                    makePropertyGet (Tuple.second defaultProperties.alias) props

                DSum _ props _ ->
                    makePropertyGet (Tuple.second defaultProperties.sum) props

                DEnum _ props _ ->
                    makePropertyGet (Tuple.second defaultProperties.enum) props

                DRestricted _ props _ ->
                    makePropertyGet (Tuple.second defaultProperties.restricted) props
    , field = makePropertyGet (Tuple.second defaultProperties.fields)
    , type_ =
        \typedef ->
            case typedef of
                TUnit _ props ->
                    makePropertyGet (Tuple.second defaultProperties.unit) props

                TBasic _ props _ ->
                    makePropertyGet (Tuple.second defaultProperties.basic) props

                TNamed _ props _ _ ->
                    makePropertyGet (Tuple.second defaultProperties.named) props

                TProduct _ props _ ->
                    makePropertyGet (Tuple.second defaultProperties.product) props

                TEmptyProduct _ props ->
                    makePropertyGet (Tuple.second defaultProperties.emptyProduct) props

                TContainer _ props _ ->
                    makePropertyGet (Tuple.second defaultProperties.container) props

                TFunction _ props _ _ ->
                    makePropertyGet (Tuple.second defaultProperties.function) props
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
    = CheckedPropertyMissing String PropSpec
    | CheckedPropertyWrongKind String PropSpec


{-| Convert prop check errors to strings.
-}
propCheckErrorToString : PropCheckError -> String
propCheckErrorToString err =
    case err of
        CheckedPropertyMissing _ _ ->
            "Checked property missing."

        CheckedPropertyWrongKind _ _ ->
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

        ( PSQName, Just (PQName path) ) ->
            PQName path |> Ok

        ( PSBool, Just (PBool val) ) ->
            PBool val |> Ok

        ( PSOptional _, Just (POptional optSpec maybe) ) ->
            POptional optSpec maybe |> Ok

        ( _, Nothing ) ->
            CheckedPropertyMissing name spec |> ResultME.error

        ( _, _ ) ->
            CheckedPropertyWrongKind name spec |> ResultME.error


getStringProperty : Properties -> Properties -> String -> ResultME PropCheckError String
getStringProperty defaults props name =
    case getProperty defaults props PSString name of
        Ok (PString val) ->
            Ok val

        Ok _ ->
            CheckedPropertyWrongKind name PSString |> ResultME.error

        Err err ->
            Err err


getEnumProperty : Properties -> Properties -> Enum String -> String -> ResultME PropCheckError String
getEnumProperty defaults props enum name =
    case getProperty defaults props (PSEnum enum) name of
        Ok (PEnum _ val) ->
            Ok val

        Ok _ ->
            CheckedPropertyWrongKind name (PSEnum enum) |> ResultME.error

        Err err ->
            Err err


getQNameProperty : Properties -> Properties -> String -> ResultME PropCheckError (List String)
getQNameProperty defaults props name =
    case getProperty defaults props PSQName name of
        Ok (PQName path) ->
            Ok path

        Ok _ ->
            CheckedPropertyWrongKind name PSQName |> ResultME.error

        Err err ->
            Err err


getBoolProperty : Properties -> Properties -> String -> ResultME PropCheckError Bool
getBoolProperty defaults props name =
    case getProperty defaults props PSBool name of
        Ok (PBool val) ->
            Ok val

        Ok _ ->
            CheckedPropertyWrongKind name PSBool |> ResultME.error

        Err err ->
            Err err


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
                    CheckedPropertyWrongKind name (PSOptional PSString) |> ResultME.error

        Ok _ ->
            CheckedPropertyWrongKind name (PSOptional PSString) |> ResultME.error

        Err err ->
            Err err


getOptionalEnumProperty : Properties -> Properties -> Enum String -> String -> ResultME PropCheckError (Maybe String)
getOptionalEnumProperty defaults props enum name =
    case getProperty defaults props (PSOptional (PSEnum enum)) name of
        Ok (POptional (PSEnum _) maybeProp) ->
            case maybeProp of
                Nothing ->
                    Ok Nothing

                Just (PEnum _ val) ->
                    Just val |> Ok

                _ ->
                    CheckedPropertyWrongKind name (PSOptional (PSEnum enum))
                        |> ResultME.error

        Ok _ ->
            CheckedPropertyWrongKind name (PSOptional (PSEnum enum))
                |> ResultME.error

        Err err ->
            Err err
