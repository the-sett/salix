module SourcePos exposing (Region, RowCol, SourceLines, sourceLinesForRegions)

import Dict exposing (Dict)
import Seq


type alias Region =
    { start : RowCol
    , end : RowCol
    }


type alias RowCol =
    { row : Int
    , col : Int
    }


{-| Captures a set of source code lines which are indexed by their line numbers,

A highlight region may also be specified, and if set is used to indicate where
within the source lines an error is located. All lines within the highlight
region are expected to be in the line dict.

-}
type alias SourceLines =
    { lines : Dict Int String
    , highlight : Maybe Region
    }


sourceLinesForRegions : Dict Int String -> List Region -> List SourceLines
sourceLinesForRegions lines regions =
    List.foldl
        (\region accum ->
            { lines = extractRegion lines region
            , highlight = Just region
            }
                :: accum
        )
        []
        regions


extractRegion : Dict Int String -> Region -> Dict Int String
extractRegion lines region =
    let
        startRow =
            region.start.row

        endRow =
            region.end.row

        seq =
            Seq.iterate ((+) 1) startRow
                |> Seq.take (endRow - startRow + 1)

        _ =
            Debug.log "extractRegion" { startRow = startRow, endRow = endRow, seq = seq, lines = lines }
    in
    Seq.foldl
        (\row accum ->
            case Dict.get row lines of
                Nothing ->
                    accum

                Just val ->
                    Dict.insert row val accum
        )
        Dict.empty
        seq
