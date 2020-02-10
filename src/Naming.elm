module Naming exposing (..)

import List.Nonempty exposing (Nonempty)
import Regex
import Set
import String.Case as Case


nameRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\w+[\\d\\w]*$"


checkName : String -> Bool
checkName val =
    Regex.contains nameRegex val


{-| Checks if a name matches an Elm keyword, and proposes a different name to
use instead, which is the original with an underscore appended.

    cleanupName "type" == "type_"

-}
safeName : String -> String
safeName val =
    let
        keywords =
            Set.fromList
                [ "type"
                , "alias"
                , "let"
                , "in"
                , "if"
                , "then"
                , "else"
                , "import"
                , "exposing"
                , "module"
                , "as"
                , "case"
                , "of"
                , "port"
                , "Int"
                , "Bool"
                , "Float"
                , "String"
                , "Char"
                , "Order"
                , "Never"
                ]
    in
    if Set.member val keywords then
        val ++ "_"

    else
        val


safeCCL : String -> String
safeCCL =
    Case.toCamelCaseLower >> safeName


safeCCU : String -> String
safeCCU =
    Case.toCamelCaseUpper >> safeName


sortNamed : List ( String, a ) -> List ( String, a )
sortNamed =
    List.sortBy Tuple.first


sortNonemptyNamed : Nonempty ( String, a ) -> Nonempty ( String, a )
sortNonemptyNamed =
    List.Nonempty.sortBy Tuple.first
