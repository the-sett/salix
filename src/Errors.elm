module Errors exposing
    ( Error, ErrorMessage
    , ErrorBuilder, lookupError, lookupErrorNoArgs, emptySourceLines
    , document, asConsoleString, htmlRenderer
    )

{-| Errors defines a format for describing human readable error messages,
that can also quote some source, in order to identify the source of an error.


# Errors and Error Messages

@docs Error, ErrorMessage


# Instantiating Error Messages into Errors

@docs ErrorBuilder, lookupError, lookupErrorNoArgs, emptySourceLines


# Error formatting and printing

@docs document, asConsoleString, htmlRenderer

-}

import Console
import Css
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Mark
import Mark.Error
import SourcePos exposing (Region, RowCol, SourceLines)


{-| Common Error definition, for reporting errors to users.

This contains all the information needed to build a nice human readable error
message. The body is written in `mdgriffith/elm-markup` and looks like this:

    There was an error in your code.

    |> Source
        label = Look, here it is:
        pos = 0

    Please fix it. This is not a helpful error message.

-}
type alias Error =
    { code : Int
    , title : String
    , body : String
    , args : Dict String String
    , sources : List SourceLines
    }


emptySourceLines : SourceLines
emptySourceLines =
    { lines = Dict.empty
    , highlight = Nothing
    }


{-| Defines the content of an error message.

The body is written in `mdgriffith/elm-markup` and looks like this:

    There was an error in your code.

    |> Source
        label = Look, here it is:
        pos = 0

    Please fix it. This is not a helpful error message.

An `ErrorMessage` will be combined with some lines of source code that it can
quote, in order to produce an `Error`.

-}
type alias ErrorMessage =
    { title : String
    , body : String
    }



-- Support for error catalogues.


defaultErrorMessage : ErrorMessage
defaultErrorMessage =
    { title = "Unhandled Error"
    , body = """
This is a bug and should be reported to the development team. If you see
this error message, it is because the correct error message was not found.
It may be that adding a more helpful message for an error condition has been
overlooked.
    """
    }


defaultError : Error
defaultError =
    { code = -1
    , title = defaultErrorMessage.title
    , body = defaultErrorMessage.body
    , args = Dict.empty
    , sources = []
    }


{-| Defines the signature of a function for building Errors. This keeps the
positional information variable, but requires a function to turn source code
positions into quotes lines of source code.

Typically implementation of this will use the `lookupError` functions and an
error catalogue of error messages.

-}
type alias ErrorBuilder pos err =
    (pos -> SourceLines) -> err -> Error


{-| Looks up an ErrorMessage in an error catalogue and fills in its quoted
source code and parameters to produce an Error.
-}
lookupError :
    Dict Int ErrorMessage
    -> Int
    -> Dict String String
    -> List SourceLines
    -> Error
lookupError errorDict code args sourceLines =
    let
        makeError message =
            { code = code
            , title = message.title
            , body = message.body
            , args = args
            , sources = sourceLines
            }
    in
    Dict.get code errorDict
        |> Maybe.map makeError
        |> Maybe.withDefault defaultError


{-| Looks up an ErrorMessage in an error catalogue and fills in its quoted
source code to produce an Error.
-}
lookupErrorNoArgs :
    Dict Int ErrorMessage
    -> Int
    -> List SourceLines
    -> Error
lookupErrorNoArgs errorDict code sourceLines =
    lookupError errorDict code Dict.empty sourceLines



-- Structural formatting of error messages as elm-markup.


{-| elm-markup Document describing the format of an error.
-}
document : Renderer content -> Error -> Mark.Document (List content)
document renderer error =
    Mark.manyOf
        [ errorDocs renderer error
        , quoteSource renderer error
        ]
        |> Mark.document (\parts -> renderer.renderTitle error.title :: parts)


errorDocs : Renderer content -> Error -> Mark.Block content
errorDocs renderer err =
    Mark.textWith
        { view = renderer.styleText
        , replacements = Mark.commonReplacements
        , inlines =
            [ inlineArg renderer err
                |> Mark.annotation "arg"
                |> Mark.field "key" Mark.string
            ]
        }
        |> Mark.map renderer.textsToParagraph


inlineArg : Renderer content -> Error -> List ( Mark.Styles, String ) -> String -> content
inlineArg renderer error texts key =
    let
        arg =
            Dict.get key error.args
                |> Maybe.withDefault ""
    in
    List.map (\( styles, val ) -> renderer.styleText styles val)
        (texts ++ [ ( { bold = False, italic = True, strike = False }, arg ) ])
        |> renderer.textsInLine


quoteSource : Renderer content -> Error -> Mark.Block content
quoteSource renderer err =
    let
        source n =
            List.drop n err.sources
                |> List.head
                |> Maybe.withDefault { lines = Dict.empty, highlight = Nothing }

        quote label pos =
            renderer.annotatedSource label (source pos)
    in
    Mark.record "Source"
        quote
        |> Mark.field "label" (Mark.text renderer.styleText)
        |> Mark.field "pos" Mark.int
        |> Mark.toBlock



-- Rendering abstraction.


type alias Renderer content =
    { annotatedSource : List content -> SourceLines -> content
    , renderTitle : String -> content
    , styleText : Mark.Styles -> String -> content
    , textsToParagraph : List content -> content
    , textsInLine : List content -> content
    }



-- Html rendering of error messages.


{-| Renders an Error as HTML.
-}
htmlRenderer : Renderer (Html msg)
htmlRenderer =
    { annotatedSource = htmlAnnotatedSource
    , renderTitle = htmlRenderTitle
    , styleText = htmlStyleText
    , textsToParagraph = htmlTextsToParagraph
    , textsInLine = htmlTextsInLine
    }


htmlAnnotatedSource : List (Html msg) -> SourceLines -> Html msg
htmlAnnotatedSource label lines =
    Html.div []
        [ Html.div [] label
        , Html.pre [] (Dict.values lines.lines |> List.map Html.text)
        ]


htmlRenderTitle : String -> Html msg
htmlRenderTitle val =
    Html.styled Html.div
        [ Css.textTransform Css.uppercase ]
        []
        [ Html.h4 [] [ Html.text val ] ]


htmlStyleText : Mark.Styles -> String -> Html msg
htmlStyleText styles string =
    if styles.bold || styles.italic || styles.strike then
        Html.styled Html.span
            [ Css.fontStyle Css.italic ]
            [ Attr.classList
                [ ( "bold", styles.bold )
                , ( "italic", styles.italic )
                , ( "strike", styles.strike )
                ]
            ]
            [ Html.text string ]

    else
        Html.text string


htmlTextsToParagraph : List (Html msg) -> Html msg
htmlTextsToParagraph texts =
    Html.p [] texts


htmlTextsInLine : List (Html msg) -> Html msg
htmlTextsInLine texts =
    Html.span [] texts



-- Console rendering of error messages.


{-| Renders an Error as a String for printing to the console with ANSI colours.
-}
asConsoleString : Error -> String
asConsoleString error =
    let
        markupErrors : List Mark.Error.Error -> String
        markupErrors errors =
            List.map Mark.Error.toString errors
                |> String.join "\n"
    in
    case Mark.compile (document consoleRenderer error) error.body of
        Mark.Success success ->
            String.join "\n\n" success

        Mark.Almost { result, errors } ->
            markupErrors errors

        Mark.Failure errors ->
            markupErrors errors


consoleRenderer : Renderer String
consoleRenderer =
    { annotatedSource = consoleAnnotatedSource
    , renderTitle = consoleRenderTitle
    , styleText = consoleStyleText
    , textsToParagraph = consoleTextsToParagraph
    , textsInLine = consoleTextsInLine
    }


consoleAnnotatedSource : List String -> SourceLines -> String
consoleAnnotatedSource label lines =
    consoleTextsToParagraph label
        ++ "\n"
        ++ consoleTextsToParagraph (Dict.values lines.lines)


consoleRenderTitle : String -> String
consoleRenderTitle val =
    "-- " ++ String.toUpper val ++ " --"


consoleStyleText : Mark.Styles -> String -> String
consoleStyleText styles string =
    if styles.bold || styles.italic || styles.strike then
        string

    else
        string


consoleTextsToParagraph : List String -> String
consoleTextsToParagraph texts =
    List.foldr (++) "" texts


consoleTextsInLine : List String -> String
consoleTextsInLine texts =
    List.foldr (++) "" texts
