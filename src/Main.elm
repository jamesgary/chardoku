module Main exposing (..)

import Array exposing (Array)
import Char
import Dom
import Html exposing (Html, h1, input, table, td, text, tr)
import Html.Attributes exposing (id, maxlength, type_, value)
import Html.Events exposing (onFocus, onInput)
import Task


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { cells : Cells
    , focusIndex : Maybe Int
    }


type alias Cells =
    Array Cell


type alias Cell =
    Maybe Char


init : ( Model, Cmd Msg )
init =
    ( { cells = Array.repeat 9 Nothing
      , focusIndex = Nothing
      }
    , Cmd.none
    )


indexOf : Int -> Int -> Int
indexOf row col =
    col + (3 * (row - 1)) - 1


getCellAt : Int -> Int -> Cells -> Cell
getCellAt row col cells =
    cells
        |> Array.get (indexOf row col)
        |> Maybe.withDefault (Just '@')



-- UPDATE


type Msg
    = FocusCell Int
    | InputCell Int String
    | NoOp (Result Dom.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusCell index ->
            { model | focusIndex = Just index } ! []

        InputCell index str ->
            { model
                | focusIndex = Just (index + 1)
                , cells = setCellAt index str model.cells
            }
                ! [ Task.attempt NoOp (Dom.focus (index + 1 |> toString)) ]

        NoOp _ ->
            model ! []


setCellAt : Int -> String -> Cells -> Cells
setCellAt index str cells =
    let
        char =
            case String.uncons str of
                Just ( c, _ ) ->
                    Just c

                Nothing ->
                    Nothing
    in
    Array.set index char cells



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    table []
        [ viewRow 1 model.cells
        , viewRow 2 model.cells
        , viewRow 3 model.cells
        ]


viewRow : Int -> Cells -> Html Msg
viewRow row cells =
    tr []
        [ viewCell (indexOf row 1) (getCellAt row 1 cells)
        , viewCell (indexOf row 2) (getCellAt row 2 cells)
        , viewCell (indexOf row 3) (getCellAt row 3 cells)
        ]


viewCell : Int -> Cell -> Html Msg
viewCell index cell =
    let
        val =
            case cell of
                Just char ->
                    String.fromChar char

                Nothing ->
                    ""
    in
    td []
        [ input
            [ value val
            , maxlength 1
            , type_ "text"
            , onFocus (FocusCell index)
            , onInput (InputCell index)
            , id (toString index)
            ]
            []
        ]
