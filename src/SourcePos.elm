module SourcePos exposing (Region, RowCol)


type alias Region =
    { start : RowCol
    , end : RowCol
    }


type alias RowCol =
    { row : Int
    , col : Int
    }
