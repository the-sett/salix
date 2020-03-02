module Errors exposing (..)

--import Markdown.Block exposing (Block)
-- TODO:
-- When converting errors, they can have multiple positions in them,
-- so quoted code blocks must be a list, to allow for more than one.
-- type Markdown
--     = List Block


type Error
    = Error String



-- { code : Int
-- , title : String
-- , body : Markdown
-- }


asConsoleString : Error -> String
asConsoleString (Error val) =
    val
