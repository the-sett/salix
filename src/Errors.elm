module Errors exposing (..)

import Markdown.Block exposing (Block)


type Markdown
    = List Block


type Error
    = Error
        { code : Int
        , title : String
        , body : Markdown
        }
