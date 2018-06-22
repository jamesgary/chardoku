module View exposing (view)

import Common exposing (..)
import Css exposing (..)
import Css.Colors
import Css.Foreign exposing (global, selector)
import Html.Styled as Html exposing (Attribute, Html, a, div, h1, h2, input, li, p, td, text, tr, ul)
import Html.Styled.Attributes as Attributes exposing (class, css, href, id, maxlength, readonly, target, type_, value)
import Html.Styled.Events exposing (..)
import Json.Decode as Json


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
            , fontWeight (int 400)
            ]
        ]
        [ global
            [ selector "html, body"
                [ fontFamilies [ "Vollkorn", "serif" ]
                , backgroundImage
                    (linearGradient2 (deg 180)
                        (stop2 (hex "fad0c4") <| pct 0)
                        (stop2 (hex "ffd1ff") <| pct 100)
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
                [ color (hex "170d4c")
                , textAlign center
                , paddingTop (px 40)
                , margin2 (px 10) (px 30)
                , widthThird
                ]
            ]
            [ -- "Chardoku"
              h1
                [ css
                    [ fontSize (px 64)
                    , textShadow3 (px 1) (px 2) (rgba 80 35 255 0.5)
                    , lineHeight zero
                    , fontWeight (int 600)
                    ]
                ]
                [ text "Chardoku" ]

            -- "source code"
            , a
                [ href "https://github.com/jamesgary/chardoku"
                , Attributes.target "_blank"
                , css
                    [ fontSize (px 18)
                    , textShadow3 (px 1) (px 1) (rgba 80 35 255 0.2)
                    , fontFamily monospace
                    , textDecoration none
                    , color (hex "170d4c")
                    , lineHeight (pct 100)
                    , display inlineBlock
                    , borderBottom3 (px 1) solid (hex "170d4c")
                    , boxShadow4 zero (px 1) zero (rgba 193 136 185 0.7)
                    , hover
                        [ -- textDecoration underline -- regular underline too thicc
                          color (hex "a63ce4")
                        , borderBottom3 (px 1) solid (rgba 166 60 228 0.7)
                        , boxShadow4 zero (px 1) zero (rgba 193 136 185 0.7)
                        ]
                    ]
                ]
                [ text "source code" ]

            -- "Can you spell..."
            , p
                [ css
                    [ fontSize (px 28)
                    , color (hex "352a71")
                    , textShadow3 (px 1) (px 1) (rgba 80 35 255 0.1)
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
                , marginTop (px 40)
                ]
            ]
            [ viewStatus model.status ]
        ]


viewStatus : Status -> Html Msg
viewStatus status =
    case status of
        Valid ->
            div
                [ css
                    [ fontSize (px 42)
                    , backgroundColor (hex "00b500")
                    , padding (px 20)
                    , display inlineBlock
                    , color (hex "#fff")
                    , borderRadius (px 14)
                    , letterSpacing (px 1)
                    , textShadow4 (px 2) (px 2) zero (rgba 0 0 0 0.5)
                    , border3 (px 2) solid (rgba 0 0 0 0.5)
                    ]
                , class "tada"
                ]
                [ text "You did it!" ]

        Invalid reasons ->
            ul
                [ css
                    [ listStyle none
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
            , textShadow4 zero zero (px 4) (rgba 0 0 0 0.87)
            , padding3 (px 8) (px 15) (px 3)
            , fontSize (px 24)
            , border3 (px 2) solid (rgb 103 0 2)
            , borderRadius (px 7)
            , margin2 (px 11) zero
            , marginRight (px 31)
            , letterSpacing (px 1)
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
                    "    "
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
            , readonly True -- TODO IS THIS ACCESSIBLE??
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
                , border3 (px 4) solid (rgba 35 24 58 0.99)
                , margin (px 4)
                , fontWeight (int 600)
                , backgroundImage
                    (linearGradient2 (deg 180)
                        (stop2 (hex "fefbff") <| pct 20)
                        (stop2 (hex "fce5ff") <| pct 100)
                        []
                    )
                , selection
                    [ backgroundColor (hex "c0eaff")
                    ]
                ]
            ]
            []
        ]
