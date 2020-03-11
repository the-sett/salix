module Errors exposing (..)

import Console
import Css
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Mark
import Mark.Error



-- Common Error definition, for reporting errors to users.


type Error
    = Error
        { code : Int
        , title : String
        , body : List String
        }



-- Support for error catalogues.


defaultError : Error
defaultError =
    Error
        { code = -1
        , title = "Unhandled Error"
        , body = []
        }


lookupError : Dict Int Error -> Int -> Error
lookupError errorDict code =
    Dict.get code errorDict
        |> Maybe.withDefault defaultError


type alias ErrorBuilder pos err =
    (pos -> String) -> err -> Error



-- Structural formatting of error messages as elm-markup.


document : Renderer content -> Error -> Mark.Document (List content)
document renderer error =
    let
        (Error err) =
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
quoteSource renderer (Error err) =
    Mark.record "Source"
        renderer.annotatedSource
        |> Mark.field "label" (Mark.text renderer.styleText)
        |> Mark.field "pos" Mark.int
        |> Mark.field "source" Mark.string
        |> Mark.toBlock



-- Rendering abstraction.


type alias Renderer content =
    { annotatedSource : List content -> Int -> String -> content
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


htmlAnnotatedSource : List (Html msg) -> Int -> String -> Html msg
htmlAnnotatedSource label pos source =
    Html.div []
        [ Html.div [] label
        , Html.pre []
            [ Html.text "source code for location "
            , Html.text (String.fromInt pos)
            ]
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


example =
    """
You got it wrong, maybe you aren't as clever as you think?

|> Source
    label = The first time you screwed up:
    pos = 0
    source = "source code location 0"

|> Source
    label = Then here you did it again:
    pos = 0
    source = source code location 1

Fix this by reading the manual. Idiot.
"""


asConsoleString : Error -> String
asConsoleString error =
    let
        markupErrors : List Mark.Error.Error -> String
        markupErrors errors =
            List.map Mark.Error.toString errors
                |> String.join "\n"
    in
    case Mark.compile (document consoleRenderer error) example of
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


consoleAnnotatedSource : List String -> Int -> String -> String
consoleAnnotatedSource label pos source =
    consoleTextsToParagraph label ++ "\n" ++ source


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
