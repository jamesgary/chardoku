module Main exposing (..)

import Html exposing (Html, h1, input, table, td, text, tr)
import Html.Attributes exposing (maxlength, type_, value)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { cells : Cells }


type alias Cells =
    { cell1 : Cell
    , cell2 : Cell
    , cell3 : Cell
    , cell4 : Cell
    , cell5 : Cell
    , cell6 : Cell
    , cell7 : Cell
    , cell8 : Cell
    , cell9 : Cell
    }


type alias Cell =
    Maybe Char


init : ( Model, Cmd Msg )
init =
    ( { cells =
            { cell1 = Nothing
            , cell2 = Nothing
            , cell3 = Nothing
            , cell4 = Nothing
            , cell5 = Nothing
            , cell6 = Nothing
            , cell7 = Nothing
            , cell8 = Nothing
            , cell9 = Nothing
            }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        { cell1, cell2, cell3, cell4, cell5, cell6, cell7, cell8, cell9 } =
            model.cells
    in
    table []
        [ tr [] [ viewCell cell1, viewCell cell2, viewCell cell3 ]
        , tr [] [ viewCell cell4, viewCell cell5, viewCell cell6 ]
        , tr [] [ viewCell cell7, viewCell cell8, viewCell cell9 ]
        ]


viewCell : Cell -> Html Msg
viewCell cell =
    let
        val =
            case cell of
                Just char ->
                    String.fromChar char

                Nothing ->
                    ""
    in
    td [] [ input [ value val, maxlength 1, type_ "text" ] [] ]
