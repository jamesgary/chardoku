module View exposing (view)

import Common exposing (..)
import Css exposing (..)
import Css.Colors
import Css.Foreign exposing (global, selector)
import Html.Styled as Html exposing (Attribute, Html, div, h1, input, li, td, text, tr, ul)
import Html.Styled.Attributes exposing (class, css, id, maxlength, type_, value)
import Html.Styled.Events exposing (onFocus, onInput)


warmFlame =
    linearGradient2 (deg 45)
        (stop2 (hex "ff9a9e") <| pct 0)
        (stop2 (hex "fad0c4") <| pct 100)
        []


widthThird : Style
widthThird =
    width (pct 33)


view : Model -> Html Msg
view model =
    div
        [ css
            [ padding (px 20)
            , displayFlex
            , justifyContent center
            ]
        ]
        [ global
            [ selector "html, body"
                [ fontFamily sansSerif
                , backgroundImage
                    (linearGradient2 (deg 155)
                        (stop2 (hex "cfc9df") <| pct 0)
                        (stop2 (hex "e2ebf0") <| pct 100)
                        []
                    )
                , width (pct 100)
                , height (pct 100)
                ]
            ]
        , Html.div
            [ css
                [ color (hex "221563")
                , textAlign center
                , paddingTop (px 40)
                , widthThird
                ]
            ]
            [ h1
                [ css
                    [ fontSize (px 64)
                    , textShadow3 (px 1) (px 2) (rgba 255 255 255 0.5)
                    , fontWeight (int 100)
                    ]
                ]
                [ text "Chardoku" ]
            ]
        , Html.div
            [ css
                [ margin2 (px 40) zero
                , widthThird
                ]
            ]
            [ Html.table
                [ css
                    [ margin auto ]
                ]
                [ viewRow 1 model.cells
                , viewRow 2 model.cells
                , viewRow 3 model.cells
                ]
            ]
        , div
            [ css
                [ widthThird
                ]
            ]
            [ viewStatus model.status ]
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
        [--css [ border3 (px 1) solid Css.Colors.gray ]
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
                , width (px 77)
                , height (px 80)
                , textAlign center
                , verticalAlign middle
                , textTransform uppercase
                , borderRadius (px 20)
                , border3 (px 4) solid (rgba 33 30 51 0.84)
                , margin (px 4)
                , fontWeight (int 600)
                ]
            ]
            []
        ]
