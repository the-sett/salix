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
prepareLayout : L1 pos -> Doc
prepareLayout model =
    Pretty.string "blah"


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


prettyDeclarable : Declarable pos ref -> Doc
prettyDeclarable _ =
    Debug.todo "prettyDeclarable"
