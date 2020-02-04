module L2 exposing (Flagged(..))

import L1 exposing (Declarations)



-- L2
-- Purpose is to pull up some information about what kind of thing an alias
-- is referring to.
-- It also indicates that the the L1 model has been reference checked; all TNamed
-- in a set of Declarations are present.


type Flagged
    = FlEnum
    | FlRestricted L1.Basic
    | FlNone


type alias L2 =
    Declarations Flagged
