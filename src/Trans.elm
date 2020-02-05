module Trans exposing (transform)

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), Declarations, L1, Restricted(..), Type(..))
import L2 exposing (RefChecked(..), L2)
import Maybe.Extra


type ModelCheckingError
    = UnresolvedRef String
    | MapKeyTypeNotAllowed


errorToString : ModelCheckingError -> String
errorToString err =
    case err of
        UnresolvedRef hint ->
            hint ++ " reference did not resolve."

        MapKeyTypeNotAllowed ->
            "Map .key is not an enum, restricted, or basic."


transform : L1 -> Result ModelCheckingError L2
transform _ =
    Dict.empty |> Ok
