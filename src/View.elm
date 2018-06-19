module View exposing (view)

import Common exposing (..)
import Css exposing (..)
import Css.Colors
import Css.Foreign exposing (global, selector)
import Html.Styled as Html exposing (Attribute, Html, a, div, h1, h2, input, li, p, td, text, tr, ul)
import Html.Styled.Attributes as Attributes exposing (class, css, href, id, maxlength, target, type_, value)
import Html.Styled.Events exposing (..)
import Json.Decode as Json


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
                , margin zero
                , padding zero
                ]
            ]
        , Html.div
            -- left title
            [ css
                [ color (hex "221563")
                , textAlign center
                , paddingTop (px 40)
                , margin2 (px 10) (px 30)
                , widthThird
                ]
            ]
            [ h1
                [ css
                    [ fontSize (px 64)
                    , textShadow3 (px 1) (px 2) (rgba 255 255 255 0.5)
                    , lineHeight zero
                    , fontWeight (int 100)
                    ]
                ]
                [ text "Chardoku" ]
            , h2
                [ css
                    [ fontSize (px 32)
                    , textShadow3 (px 1) (px 2) (rgba 255 255 255 0.5)
                    , fontWeight (int 100)
                    ]
                ]
                [ a
                    [ href "https://github.com/jamesgary/chardoku"
                    , Attributes.target "_blank"
                    ]
                    [ text "source code" ]
                ]
            , p
                [ css
                    [ fontSize (px 24)
                    ]
                ]
                [ text
                    """
Can you spell six 3-letter words (three words horizontally, three words vertically) without repeating any letter?
                """
                ]
            ]
        , Html.div
            -- game table
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
            -- reasons
            [ css
                [ widthThird
                , marginTop (px 66)
                ]
            ]
            [ viewStatus model.status ]
        ]


viewStatus : Status -> Html Msg
viewStatus status =
    case status of
        Valid ->
            div [ css [] ]
                [ text "You did it!" ]

        Invalid reasons ->
            ul
                [ css
                    [ listStyle none
                    , paddingTop (px 60)
                    , margin zero
                    , padding zero
                    ]
                ]
                (List.map viewReason reasons)


viewReason : String -> Html Msg
viewReason reason =
    li
        [ css
            [ backgroundColor (rgba 183 50 42 0.9)
            , color (hex "fff")
            , padding2 (px 8) (px 12)
            , fontSize (px 16)
            , borderRadius (px 7)
            , margin2 (px 11) zero
            , marginRight (px 31)
            ]
        ]
        [ text reason ]


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
            , onClick (ClickCell index)
            , onFocus (FocusCell index)
            , on "keydown"
                (Json.map (InputCell index) keyCode)
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
