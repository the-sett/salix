module Trans exposing (transform)

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), Declarations, Restricted(..), Type(..))
import L2 exposing (Flagged(..))
import Maybe.Extra


type TransformError
    = UnresolvedRef String
    | MapKeyTypeNotAllowed


errorToString : TransformError -> String
errorToString err =
    case err of
        UnresolvedRef hint ->
            hint ++ " reference did not resolve."

        MapKeyTypeNotAllowed ->
            "Map .key is not an enum, restricted, or basic."



--transform : L1 -> L2


transform _ =
    ()
