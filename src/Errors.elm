module Errors exposing (..)

{-| Errors defines a format for describing human readable error messages,
that can also quote some source, in order to identify the source of an error.

@docs Error

-}

import Console
import Css
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Mark
import Mark.Error
import SourcePos exposing (Region, RowCol)


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
    , sources : List SourceLines
    , body : String
    }


{-| Captures a set of source code lines which are indexed by their line numbers,

A highlight region may also be specified, and if set is used to indicate where
within the source lines an error is located. All lines within the highlight
region are expected to be in the line dict.

-}
type alias SourceLines =
    { lines : Dict Int String
    , highlight : Maybe Region
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
    , sources = []
    }


{-| Using this as an example. TODO: Remove before production release!.
-}
rudeExampleErrorMessage : ErrorMessage
rudeExampleErrorMessage =
    { title = "Unhandled Error"
    , body = """
You got it wrong, maybe you aren't as clever as you think?

|> Source
    label = The first time you screwed up:
    pos = 0

|> Source
    label = Then here you did it again:
    pos = 1

Fix this by reading the manual. Idiot.
      """
    }


type alias ErrorBuilder pos err =
    (pos -> SourceLines) -> err -> Error


lookupError : Dict Int ErrorMessage -> Int -> List SourceLines -> Error
lookupError errorDict code sourceLines =
    let
        makeError message =
            { code = code
            , title = message.title
            , body = message.body
            , sources = sourceLines
            }
    in
    Dict.get code errorDict
        |> Maybe.map makeError
        |> Maybe.withDefault defaultError



-- Structural formatting of error messages as elm-markup.


document : Renderer content -> Error -> Mark.Document (List content)
document renderer error =
    let
        err =
            error
    in
    Mark.manyOf
        [ errorDocs renderer error
        , quoteSource renderer error
        ]
        |> Mark.document (\parts -> renderer.renderTitle err.title :: parts)


errorDocs : Renderer content -> Error -> Mark.Block content
errorDocs renderer err =
    Mark.text renderer.styleText
        |> Mark.map renderer.textsToParagraph


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
    }



-- Html rendering of error messages.


htmlRenderer : Renderer (Html msg)
htmlRenderer =
    { annotatedSource = htmlAnnotatedSource
    , renderTitle = htmlRenderTitle
    , styleText = htmlStyleText
    , textsToParagraph = htmlTextsToParagraph
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
        Html.span
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



-- Console rendering of error messages.


asConsoleString : Error -> String
asConsoleString _ =
    let
        error =
            { code = -1
            , title = rudeExampleErrorMessage.title
            , body = rudeExampleErrorMessage.body
            , sources =
                [ { lines = Dict.fromList [ ( 0, "Source code position 0" ) ]
                  , highlight =
                        Just
                            { start = { row = 0, col = 0 }
                            , end = { row = 0, col = 3 }
                            }
                  }
                , { lines = Dict.fromList [ ( 0, "Source code position 1" ) ]
                  , highlight =
                        Just
                            { start = { row = 0, col = 0 }
                            , end = { row = 0, col = 3 }
                            }
                  }
                ]
            }

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
