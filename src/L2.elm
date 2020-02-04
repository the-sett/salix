module L2 exposing (Flagged(..), L2)

import L1 exposing (Declarations)



-- Model TODO:
-- Rename to RefChecked
-- Add more than just enum and restricted - a summary of all the decl
-- kinds.
-- Need indication that something is recursive.
-- Derivation of relational model.
--  arity
--  directionality
-- Code TODO:
-- L1 to L2 transformation.


type Flagged
    = FlEnum
    | FlRestricted L1.Basic
    | FlNone



-- The L2 model


type alias L2 =
    Declarations Flagged
