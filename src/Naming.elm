module Naming exposing
    ( checkName
    , safeName, safeCCU, safeCCL
    , sortNamed, sortNonemptyNamed
    )

{-| Helper functions for working with names in source code.


# Check names are valid.

@docs checkName


# Build valid names.

@docs safeName, safeCCU, safeCCL


# Sort things by name.

@docs sortNamed, sortNonemptyNamed

-}

import List.Nonempty exposing (Nonempty)
import Regex
import Set
import String.Case as Case


nameRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^\\w+[\\d\\w]*$"


{-| Checks that a name starts with an alpha character, and is followed only
by alpha and numeric characters.
-}
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


{-| Checks if a name matches an Elm keyword, and proposes a different name to
use instead, which is the original with an underscore appended.

    cleanupName "type" == "type_"

The name is put into camel case starting with a lower case letter.

-}
safeCCL : String -> String
safeCCL =
    Case.toCamelCaseLower >> safeName


{-| Checks if a name matches an Elm keyword, and proposes a different name to
use instead, which is the original with an underscore appended.

    cleanupName "type" == "type_"

The name is put into camel case starting with an upper case letter.

-}
safeCCU : String -> String
safeCCU =
    Case.toCamelCaseUpper >> safeName


{-| Sorts a list of named things.
-}
sortNamed : List ( String, a ) -> List ( String, a )
sortNamed =
    List.sortBy Tuple.first


{-| Sorts an non-empty list of named things.
-}
sortNonemptyNamed : Nonempty ( String, a, b ) -> Nonempty ( String, a, b )
sortNonemptyNamed =
    List.Nonempty.sortBy (\( name, _, _ ) -> name)
