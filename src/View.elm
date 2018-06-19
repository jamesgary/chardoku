module View exposing (view)

--import Html exposing (Html, div, h1, input, li, table, td, text, tr, ul)

import Common exposing (..)
import Css exposing (..)
import Css.Colors
import Html.Styled as Html exposing (Html, div, h1, input, li, td, text, tr, ul)
import Html.Styled.Attributes exposing (class, css, id, maxlength, type_, value)
import Html.Styled.Events exposing (onFocus, onInput)


view : Model -> Html Msg
view model =
    div []
        [ Html.table
            [ css
                [ margin2 (px 40) auto
                ]
            ]
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
    td
        [ css [ border3 (px 1) solid Css.Colors.gray ]
        ]
        [ input
            [ value val
            , maxlength 1
            , type_ "text"
            , onFocus (FocusCell index)
            , onInput (InputCell index)
            , id (toString index)
            , css
                [ fontSize (px 64)
                , width (px 75)
                , height (px 75)
                , textAlign center
                , verticalAlign middle
                , textTransform uppercase
                ]
            ]
            []
        ]