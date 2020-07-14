module Elm.Json.NamedRef exposing (..)

import Elm.CodeGen as CG exposing (Expression)
import ResultME exposing (ResultME)


type alias NamedRefGen =
    String -> ResultME String Expression
