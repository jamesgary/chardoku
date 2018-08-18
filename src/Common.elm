module Common exposing (..)

import Array exposing (Array)
import Dom


type alias Model =
    { cells : Cells
    , focusIndex : Maybe Int
    , status : Status
    }


type alias Cells =
    Array Cell


type alias Cell =
    Maybe Char


type Status
    = Valid
    | Invalid (List String)


type Msg
    = ReceiveInput String
    | ClickCell Int
    | Defocus


getCellAt : Int -> Int -> Cells -> Cell
getCellAt row col cells =
    cells
        |> Array.get (indexOf row col)
        |> Maybe.withDefault (Just '@')


indexOf : Int -> Int -> Int
indexOf row col =
    col + (3 * (row - 1)) - 1
