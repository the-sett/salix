module Salix.Pretty exposing (..)

{-| Pretty printer for L1.
-}

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), L1, Restricted(..), Type(..), Unchecked)
import List.Nonempty exposing (Nonempty(..))
import Maybe.Extra
import Pretty exposing (Doc)
import Set exposing (Set)


{-| Pretty prints an L1 model.
-}
prepareLayout : L1 pos -> Doc
prepareLayout model =
    Pretty.string "blah"
