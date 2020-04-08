module L3 exposing
    ( L3
    , Processor, ProcessorImpl, builder
    , L3Error(..), errorBuilder, errorCatalogue
    , DefaultProperties, PropertiesAPI, PropertyGet, makePropertiesAPI
    )

{-| Defines the level 3 language for data models that have been annotated with
properties indicating that the model has particular features needed by particular
code generators.

A level 3 language ensures the specific requirements for processing by a specific
code generator are being met.


# The L3 data modelling language.

@docs L3


# Standardized interface to an L3 processor.

@docs Processor, ProcessorImpl, builder


# Common L3 error catalogue for property and dereferncing errors.

@docs L3Error, errorBuilder, errorCatalogue


# Defaulting of properties across the data model, and APIs to read properties.

@docs DefaultProperties, PropertiesAPI, PropertyGet, makePropertiesAPI

-}

import Dict exposing (Dict)
import Enum exposing (Enum)
import Errors exposing (Error, ErrorBuilder, ErrorMessage)
import L1 exposing (Declarable(..), PropSpec(..), PropSpecs, Properties, Property(..), Type(..))
import L2 exposing (L2, RefChecked)
import ResultME exposing (ResultME)
import SourcePos exposing (SourceLines)


{-| The L3 model. This consists of an L2 model and a set of top-level properties
that a code generator needs to know.
-}
type alias L3 pos =
    { properties : Properties
    , declarations : Dict String (Declarable pos RefChecked)
    }



-- Standardized interface to an L3 processor.


{-| API for an L3 model processor.
-}
type alias Processor pos =
    { name : String
    , defaults : DefaultProperties
    , check : L3 pos -> ResultME Error (L3 pos)
    }


{-| Builds an L3 Processor API from an implementation. A function to turn
source code positions into quoted lines of source code needs to be supplied.
-}
builder : (pos -> SourceLines) -> ProcessorImpl pos err -> Processor pos
builder posFn impl =
    { name = impl.name
    , defaults = impl.defaults
    , check = impl.check >> ResultME.mapError (impl.buildError posFn)
    }


{-| SPI for an L3 model processor. Use the `builder` to turn one of these into
a `Processor`.
-}
type alias ProcessorImpl pos err =
    { name : String
    , defaults : DefaultProperties
    , check : L3 pos -> ResultME err (L3 pos)
    , buildError : ErrorBuilder pos err
    }



-- Common L3 error catalogue for property and dereferncing errors.


{-| The error catalogue for this property checking.

The error message here are quite generic, and you likely want to re-write
this error catalogue for specific modules in order to give better messages.

-}
errorCatalogue : Dict Int ErrorMessage
errorCatalogue =
    Dict.fromList
        [ ( 301
          , { title = "Required Property Missing"
            , body = "The required property []{arg|key=name } was not set."
            }
          )
        , ( 302
          , { title = "Property is the Wrong Kind"
            , body = "The required property []{arg|key=name } is the wrong kind."
            }
          )
        , ( 303
          , { title = "Name Type Alias Could Not Be Found"
            , body = "The type alias []{arg|key=name } could not be found."
            }
          )
        ]


{-| Common Errors that L3 processors may run into when fetching properties or
dereferencing declarations from the model. One the model has been reference and
property checked these error should not be possible, but we have to allow for the
error code branches anyway.

As these errors should generally not happen, they should be reported as bugs when
they do. This error type enumerates the possible dereferencing and property bugs.

-}
type L3Error
    = CheckedPropertyMissing String PropSpec
    | CheckedPropertyWrongKind String PropSpec
    | DerefDeclMissing String
    | NotExpectedKind String String


{-| Convert prop check errors to standard errors.
-}
errorBuilder : ErrorBuilder pos L3Error
errorBuilder posFn err =
    case err of
        CheckedPropertyMissing name propSpec ->
            Errors.lookupError errorCatalogue
                301
                (Dict.fromList [ ( "name", name ) ])
                []

        CheckedPropertyWrongKind name propSpec ->
            Errors.lookupError errorCatalogue
                302
                (Dict.fromList [ ( "name", name ) ])
                []

        DerefDeclMissing name ->
            Errors.lookupError errorCatalogue
                303
                (Dict.fromList [ ( "name", name ) ])
                []

        NotExpectedKind expected actual ->
            Errors.lookupError errorCatalogue
                304
                (Dict.fromList
                    [ ( "expected", expected )
                    , ( "actual", actual )
                    ]
                )
                []



-- Defaulting of properties across the data model, and APIs to read properties.


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


{-| An API for reading properties of various expected kinds.
-}
type alias PropertyGet =
    { getStringProperty : String -> ResultME L3Error String
    , getEnumProperty : Enum String -> String -> ResultME L3Error String
    , getQNameProperty : String -> ResultME L3Error (List String)
    , getBoolProperty : String -> ResultME L3Error Bool
    , getOptionalStringProperty : String -> ResultME L3Error (Maybe String)
    , getOptionalEnumProperty : Enum String -> String -> ResultME L3Error (Maybe String)
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


getWithDefault : Properties -> Properties -> String -> Maybe Property
getWithDefault defaults props name =
    case Dict.get name props of
        Nothing ->
            Dict.get name defaults

        justVal ->
            justVal


{-| Extracts a property from a set of properties. The property must be present
and must match the given specification - otherwise an error will be returned.
-}
getRequiredProperty : Properties -> Properties -> PropSpec -> String -> ResultME L3Error Property
getRequiredProperty defaults props spec name =
    let
        maybeProp =
            getWithDefault defaults props name

        checkSpec propSpec prop =
            case ( spec, prop ) of
                ( PSString, PString val ) ->
                    PString val |> Ok

                ( PSEnum _, PEnum enum val ) ->
                    PEnum enum val |> Ok

                ( PSQName, PQName path ) ->
                    PQName path |> Ok

                ( PSBool, PBool val ) ->
                    PBool val |> Ok

                ( PSOptional optSpec, val ) ->
                    checkSpec optSpec val

                ( unmatched, _ ) ->
                    CheckedPropertyWrongKind name unmatched |> ResultME.error
    in
    case maybeProp of
        Nothing ->
            CheckedPropertyMissing name spec |> ResultME.error

        Just prop ->
            checkSpec spec prop


{-| Extracts a property from a set of properties. The property if present
and must match the given specification - otherwise an error will be returned.

The result is a `Maybe` allowing for the property to be not set, in which case
`Nothing` is returned.

-}
getOptionalProperty : Properties -> Properties -> PropSpec -> String -> ResultME L3Error (Maybe Property)
getOptionalProperty defaults props spec name =
    let
        maybeProp =
            getWithDefault defaults props name

        checkSpec propSpec prop =
            case ( spec, prop ) of
                ( PSString, PString val ) ->
                    PString val |> Ok

                ( PSEnum _, PEnum enum val ) ->
                    PEnum enum val |> Ok

                ( PSQName, PQName path ) ->
                    PQName path |> Ok

                ( PSBool, PBool val ) ->
                    PBool val |> Ok

                ( PSOptional optSpec, val ) ->
                    checkSpec optSpec val

                ( unmatched, _ ) ->
                    CheckedPropertyWrongKind name unmatched |> ResultME.error
    in
    case maybeProp of
        Nothing ->
            Ok Nothing

        Just prop ->
            checkSpec spec prop
                |> Result.map Just


getStringProperty : Properties -> Properties -> String -> ResultME L3Error String
getStringProperty defaults props name =
    case getRequiredProperty defaults props PSString name of
        Ok (PString val) ->
            Ok val

        Ok _ ->
            CheckedPropertyWrongKind name PSString |> ResultME.error

        Err err ->
            Err err


getEnumProperty : Properties -> Properties -> Enum String -> String -> ResultME L3Error String
getEnumProperty defaults props enum name =
    case getRequiredProperty defaults props (PSEnum enum) name of
        Ok (PEnum _ val) ->
            Ok val

        Ok _ ->
            CheckedPropertyWrongKind name (PSEnum enum) |> ResultME.error

        Err err ->
            Err err


getQNameProperty : Properties -> Properties -> String -> ResultME L3Error (List String)
getQNameProperty defaults props name =
    case getRequiredProperty defaults props PSQName name of
        Ok (PQName path) ->
            Ok path

        Ok _ ->
            CheckedPropertyWrongKind name PSQName |> ResultME.error

        Err err ->
            Err err


getBoolProperty : Properties -> Properties -> String -> ResultME L3Error Bool
getBoolProperty defaults props name =
    case getRequiredProperty defaults props PSBool name of
        Ok (PBool val) ->
            Ok val

        Ok _ ->
            CheckedPropertyWrongKind name PSBool |> ResultME.error

        Err err ->
            Err err


getOptionalStringProperty : Properties -> Properties -> String -> ResultME L3Error (Maybe String)
getOptionalStringProperty defaults props name =
    case getOptionalProperty defaults props PSString name of
        Ok (Just (PString val)) ->
            Just val |> Ok

        Ok Nothing ->
            Ok Nothing

        Ok _ ->
            CheckedPropertyWrongKind name (PSOptional PSString) |> ResultME.error

        Err err ->
            Err err


getOptionalEnumProperty : Properties -> Properties -> Enum String -> String -> ResultME L3Error (Maybe String)
getOptionalEnumProperty defaults props enum name =
    case getOptionalProperty defaults props (PSEnum enum) name of
        Ok (Just (PEnum _ val)) ->
            Just val |> Ok

        Ok Nothing ->
            Ok Nothing

        Ok _ ->
            CheckedPropertyWrongKind name (PSOptional (PSEnum enum))
                |> ResultME.error

        Err err ->
            Err err
