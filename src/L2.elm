module L2 exposing (L2, RefChecked(..))

import L1 exposing (Declarations)



-- Model TODO:
-- Add more than just enum and restricted - a summary of all the decl
-- kinds.
-- Need indication that something is recursive.
-- Derivation of relational model.
--  arity
--  directionality
-- Code TODO:
-- L1 to L2 transformation.


type RefChecked
    = RcEnum
    | RcRestricted L1.Basic
    | RcNone



-- The L2 model


type alias L2 =
    Declarations RefChecked
