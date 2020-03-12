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

    -- TODO: Needs to allow position info here to be able to highlight the error
    -- in the correct place.
    -- This should be optional, for example if the source is a JSON path, rather
    -- than a set of line/row locations.
    -- Each item in the list should be an instance of quoted source code.
    -- Within each item:
    -- How about Dict Int String? for the lines of code.
    -- Then the pos spec is (row1, column1) - (row2, column2), this should also
    -- be kept in the item as it is needed for highlighting. This is also
    -- optional.
    , sources : List String
    , body : String
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


defaultError : Error
defaultError =
    { code = -1
    , title = "Unhandled Error"
    , body = """
This is a bug and should be reported to the development team. If you see
this error message, it is because the correct error message was not found.
It may be that adding a more helpful message for an error condition has been
overlooked.
    """
    , sources = []
    }


{-| Using this as an example. TODO: Remove before production release!.
-}
rudeExampleError : Error
rudeExampleError =
    { code = -1
    , title = "Unhandled Error"
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
    , sources = [ "source code location 0", "source code location 1" ]
    }


lookupError : Dict Int Error -> Int -> Error
lookupError errorDict code =
    Dict.get code errorDict
        |> Maybe.withDefault defaultError



-- TODO: Need the source, a pos that extracts from that source and produces
-- elements of the `sources` field in `Error`. Which is to say that the pos
-- info may be present to describe which parts of the lines of the quoted
-- source need to be highlighted.
-- type alias ErrorBuilder pos err =
--     (pos -> String) -> err -> Error
--
--
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
                |> Maybe.withDefault ""

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
    { annotatedSource : List content -> String -> content
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


htmlAnnotatedSource : List (Html msg) -> String -> Html msg
htmlAnnotatedSource label source =
    Html.div []
        [ Html.div [] label
        , Html.pre [] [ Html.text source ]
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
            rudeExampleError

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


consoleAnnotatedSource : List String -> String -> String
consoleAnnotatedSource label source =
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
