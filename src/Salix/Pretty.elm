module Salix.Pretty exposing (..)

{-| Pretty printer for L1.
-}

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), L1, Restricted(..), Type(..))
import List.Nonempty exposing (Nonempty(..))
import Maybe.Extra
import Pretty exposing (Doc)
import Set exposing (Set)


{-| Pretty prints an L1 model.
-}
prepareLayout : List ( String, Declarable pos check ) -> Doc
prepareLayout model =
    List.map
        (\decl ->
            prettyDeclarable decl
                |> Pretty.a Pretty.line
        )
        model
        |> doubleLines


prettyBasic : Basic -> Doc
prettyBasic _ =
    Debug.todo "prettyBasic"


prettyContainer : Container pos ref -> Doc
prettyContainer _ =
    Debug.todo "prettyContainer"


prettyRestricted : Restricted -> Doc
prettyRestricted _ =
    Debug.todo "prettyRestricted"


prettyType : Type pos ref -> Doc
prettyType _ =
    Debug.todo "prettyType"


prettyDeclarable : ( String, Declarable pos ref ) -> Doc
prettyDeclarable ( name, decl ) =
    case decl of
        DAlias pos props typ ->
            Pretty.string name
                |> Pretty.append Pretty.space
                |> Pretty.append (Pretty.string "alias")

        DSum pos props fields ->
            Pretty.string name
                |> Pretty.append Pretty.space
                |> Pretty.append (Pretty.string "sum")

        DEnum pos props labels ->
            Pretty.string name
                |> Pretty.append Pretty.space
                |> Pretty.append (Pretty.string "enum")

        DRestricted pos props res ->
            Pretty.string name
                |> Pretty.append Pretty.space
                |> Pretty.append (Pretty.string "restricted")


doubleLines : List Doc -> Doc
doubleLines =
    Pretty.join (Pretty.a Pretty.line Pretty.line)
