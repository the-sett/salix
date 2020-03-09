module Errors exposing (..)

import Dict exposing (Dict)



-- import Markdown.Block exposing (Block)
-- import Markdown.Html
-- import Markdown.Parser exposing (ListItem(..), Renderer)
-- TODO:
-- When converting errors, they can have multiple positions in them,
-- so quoted code blocks must be a list, to allow for more than one.
-- type Markdown
--     = List Block


type Error
    = Error
        { code : Int
        , title : String
        , body : List String
        }


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


asConsoleString : Error -> String
asConsoleString (Error { code, title, body }) =
    -- let
    --     bodyResult =
    --         Markdown.Parser.render consoleRenderer body
    --             |> Result.map (List.foldl (++) "")
    --             |> Result.withDefault ""
    -- in
    -- String.fromInt code
    --     ++ " : "
    --     ++ title
    --     ++ "\n\n"
    --     ++ bodyResult
    String.fromInt code
        ++ " : "
        ++ title



-- consoleRenderer : Renderer String
-- consoleRenderer =
--     { heading = \{ level, children, rawText } -> List.foldl (++) "" children
--     , raw = List.foldl (++) ""
--     , blockQuote = List.foldl (++) ""
--     , html = Markdown.Html.oneOf []
--     , plain = \content -> content
--     , code = \content -> "" ++ content
--     , bold = \content -> "" ++ content
--     , italic = \content -> "" ++ content
--     , link = \link content -> link.destination |> Ok
--     , image = \image content -> content |> Ok
--     , unorderedList =
--         \items ->
--             items
--                 |> List.map (\(ListItem _ itemBlocks) -> List.foldl (++) "" itemBlocks)
--                 |> List.foldl (++) ""
--     , orderedList =
--         \startingIndex items ->
--             items
--                 |> List.map (List.foldl (++) "")
--                 |> List.foldl (++) ""
--     , codeBlock = \{ body, language } -> "" ++ body
--     , thematicBreak = ""
--     }
