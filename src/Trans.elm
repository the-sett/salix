module Trans exposing (transform)

import AWS.Core.Service exposing (Signer(..))
import AWSApiModel exposing (AWSApiModel, Endpoint)
import AWSService exposing (AWSService, AWSType(..), Operation, Shape, ShapeRef)
import Console
import Dict exposing (Dict)
import Elm.CodeGen as CG exposing (Comment, DocComment, FileComment)
import Enum exposing (Enum)
import Errors exposing (Error)
import Html.Parser as HP
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
