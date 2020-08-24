module Salix.Pretty exposing (..)

{-| Pretty printer for L1.
-}

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), Field, L1, Restricted(..), Type(..))
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Maybe.Extra
import Pretty exposing (Doc, append, join, line, space, string)
import Set exposing (Set)


{-| Pretty prints an L1 model.
-}
prepareLayout : List ( String, Declarable pos check ) -> Doc
prepareLayout model =
    List.map
        (\decl ->
            prettyDeclarable decl
                |> append line
        )
        model
        |> doubleLines


prettyBasic : Basic -> Doc
prettyBasic basic =
    case basic of
        BBool ->
            string "Bool"

        BInt ->
            string "Int"

        BReal ->
            string "Real"

        BString ->
            string "String"


prettyContainer : Container pos ref -> Doc
prettyContainer container =
    case container of
        CList typ ->
            [ string "List"
            , prettyType typ
            ]
                |> Pretty.words

        CSet typ ->
            [ string "Set"
            , prettyType typ
            ]
                |> Pretty.words

        CDict keyType valType ->
            [ string "Dict"
            , prettyType keyType
            , prettyType valType
            ]
                |> Pretty.words

        COptional typ ->
            [ string "Optional"
            , prettyType typ
            ]
                |> Pretty.words


prettyRestricted : Restricted -> Doc
prettyRestricted res =
    case res of
        RInt _ ->
            string "RInt"

        RString _ ->
            string "RString"


prettyType : Type pos ref -> Doc
prettyType typ =
    case typ of
        TUnit pos props ->
            string "()"

        TBasic pos props basic ->
            prettyBasic basic

        TNamed pos props name ref ->
            string name

        TProduct pos props fields ->
            prettyFields fields

        TEmptyProduct pos props ->
            string "{}"

        TContainer pos props container ->
            prettyContainer container

        TFunction pos props fromType toType ->
            [ prettyType fromType
            , string "->"
            , prettyType toType
            ]
                |> Pretty.words


prettyFields : Nonempty (Field pos ref) -> Doc
prettyFields fields =
    Nonempty.map prettyField fields
        |> Nonempty.toList
        |> Pretty.lines



--( String, Type pos ref, Properties )


prettyField : Field pos ref -> Doc
prettyField ( fname, ftype, fprops ) =
    [ string fname
    , string ":"
    , prettyType ftype
    ]
        |> Pretty.words


prettyDeclarable : ( String, Declarable pos ref ) -> Doc
prettyDeclarable ( name, decl ) =
    case decl of
        DAlias pos props typ ->
            [ string "alias"
            , string name
            , string "="
            , prettyType typ
            ]
                |> Pretty.words

        DSum pos props constructors ->
            [ string "sum"
            , string name
            ]
                |> Pretty.words

        DEnum pos props labels ->
            [ string "enum"
            , string name
            ]
                |> Pretty.words

        DRestricted pos props res ->
            [ string "restricted"
            , string name
            ]
                |> Pretty.words



--== Helpers


prettyMaybe : (a -> Doc) -> Maybe a -> Doc
prettyMaybe prettyFn maybeVal =
    Maybe.map prettyFn maybeVal
        |> Maybe.withDefault Pretty.empty


decrementIndent : Int -> Int -> Int
decrementIndent currentIndent spaces =
    let
        modded =
            modBy 4 (currentIndent - spaces)
    in
    if modded == 0 then
        4

    else
        modded


dot : Doc
dot =
    Pretty.string "."


quotes : Doc -> Doc
quotes doc =
    Pretty.surround (Pretty.char '"') (Pretty.char '"') doc


singleQuotes : Doc -> Doc
singleQuotes doc =
    Pretty.surround (Pretty.char '\'') (Pretty.char '\'') doc


sqParens : Doc -> Doc
sqParens doc =
    Pretty.surround (Pretty.string "[") (Pretty.string "]") doc


doubleLines : List Doc -> Doc
doubleLines =
    Pretty.join (Pretty.a Pretty.line Pretty.line)


escape : String -> String
escape val =
    val
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> String.replace "\n" "\\n"
        |> String.replace "\t" "\\t"


escapeChar : Char -> String
escapeChar val =
    case val of
        '\'' ->
            "\\'"

        c ->
            String.fromChar c


optionalGroup : Bool -> Doc -> Doc
optionalGroup flag doc =
    if flag then
        doc

    else
        Pretty.group doc


optionalParens : Bool -> Doc -> Doc
optionalParens flag doc =
    if flag then
        Pretty.parens doc

    else
        doc
