module Main exposing (..)

import Array exposing (Array)
import Char
import Dom
import Html exposing (Html, div, h1, input, li, table, td, text, tr, ul)
import Html.Attributes exposing (class, id, maxlength, type_, value)
import Html.Events exposing (onFocus, onInput)
import Set
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
    , status : Status
    }


type alias Cells =
    Array Cell


type alias Cell =
    Maybe Char


type Status
    = Valid
    | Invalid (List String)


init : ( Model, Cmd Msg )
init =
    let
        cells =
            Array.repeat 9 Nothing
    in
    ( { cells = cells
      , focusIndex = Nothing
      , status = validate cells
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


validate : Cells -> Status
validate cells =
    {-
       duplicate characters
       6 valid words
    -}
    let
        filledOutCells =
            cells
                |> Array.toList
                |> List.filterMap identity

        isIncomplete =
            List.length filledOutCells < 9

        uniqueCount =
            filledOutCells
                |> Set.fromList
                |> Set.size

        hasDuplicateChars =
            uniqueCount < List.length filledOutCells

        reasons =
            [ if isIncomplete then
                [ "Blank cells!" ]
              else
                []
            , if hasDuplicateChars then
                [ "Duplicate characters!" ]
              else
                []
            ]
                |> List.concat
    in
    if List.isEmpty reasons then
        Valid
    else
        Invalid reasons



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
            let
                cells =
                    setCellAt index str model.cells
            in
            { model
                | focusIndex = Just (index + 1)
                , cells = cells
                , status = validate cells
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
    div []
        [ table []
            [ viewRow 1 model.cells
            , viewRow 2 model.cells
            , viewRow 3 model.cells
            ]
        , viewStatus model.status
        ]


viewStatus : Status -> Html Msg
viewStatus status =
    case status of
        Valid ->
            h1 [ class "validated" ] [ text "You did it!" ]

        Invalid reasons ->
            ul [] (List.map viewReason reasons)


viewReason : String -> Html Msg
viewReason reason =
    li [] [ text reason ]


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
