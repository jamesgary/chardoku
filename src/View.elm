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
            [ padding (pct 2)
            , displayFlex
            , justifyContent center
            , fontWeight (int 400)
            ]
        ]
        [ global
            [ selector "html"
                [ fontFamilies [ "Vollkorn", "serif" ]
                , backgroundImage
                    (linearGradient2 (deg 180)
                        (stop2 (hex "fad0c4") <| pct 0)
                        (stop2 (hex "ffd1ff") <| pct 100)
                        []
                    )
                , backgroundRepeat noRepeat
                , backgroundSize cover
                , backgroundAttachment fixed
                ]
            , selector "html, body, #main"
                [ width (pct 100)
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
                , margin2 (pct 1) (pct 3)
                , widthThird
                ]
            ]
            [ -- "Chardoku"
              h1
                [ css
                    [ fontSize (vw 5)
                    , textShadow3 (vw 0.1) (vw 0.2) (rgba 80 35 255 0.5)
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
                    [ fontSize (vw 2)
                    , textShadow3 (vw 0.1) (vw 0.2) (rgba 80 35 255 0.2)
                    , fontFamily monospace
                    , textDecoration none
                    , color (hex "170d4c")
                    , lineHeight (pct 100)
                    , display inlineBlock
                    , borderBottom3 (vw 0.1) solid (hex "170d4c")
                    , boxShadow4 zero (vw 0.1) zero (rgba 193 136 185 0.7)
                    , hover
                        [ -- textDecoration underline -- regular underline too thicc
                          color (hex "a63ce4")
                        , borderBottom3 (vw 0.1) solid (rgba 166 60 228 0.7)
                        , boxShadow4 zero (vw 0.1) zero (rgba 193 136 185 0.7)
                        ]
                    ]
                ]
                [ text "source code" ]

            -- "Can you spell..."
            , p
                [ css
                    [ fontSize (vw 2)
                    , color (hex "352a71")
                    , textShadow3 (vw 0.1) (vw 0.1) (rgba 80 35 255 0.1)
                    ]
                ]
                [ text
                    """
Can you spell six 3-letter words (three words horizontally, three words vertically) without repeating any letter?
                """
                ]
            , p
                [ css
                    [ fontSize (vw 1.5)
                    , color (hex "352a71")
                    , textShadow3 (vw 0.1) (vw 0.1) (rgba 80 35 255 0.1)
                    ]
                ]
                [ text "Fun fact: There are 85,068 solutions using the "
                , a [ href "http://scrabble.merriam.com/3-letter-words", Attributes.target "_blank" ]
                    [ text "Official Scrabble™ Players Dictionary"
                    ]
                , text "."
                ]
            ]
        , Html.div
            -- game table
            [ css
                [ marginTop (vw 2)
                , widthThird
                ]
            ]
            [ Html.table
                [ css
                    [ margin auto ]
                ]
                [ viewRow 1 model.focusIndex model.cells
                , viewRow 2 model.focusIndex model.cells
                , viewRow 3 model.focusIndex model.cells
                ]
            ]
        , div
            -- reasons
            [ css
                [ widthThird
                , marginTop (vw 2)
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
                    [ fontSize (vw 5)
                    , backgroundColor (hex "00b500")
                    , padding3 (vw 1) (vw 3) (vw 0.5)
                    , display inlineBlock
                    , color (hex "#fff")
                    , borderRadius (vw 1)
                    , letterSpacing (vw 0.05)
                    , textShadow4 (vw 0.2) (vw 0.2) zero (rgba 0 0 0 0.5)
                    , border3 (vw 0.4) solid (rgba 0 0 0 0.5)
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
            , textShadow4 zero zero (vw 0.2) (rgba 0 0 0 0.87)
            , padding3 (vw 1) (vw 2) (vw 0.5)
            , fontSize (vw 2)
            , border3 (vw 0.3) solid (rgb 103 0 2)
            , borderRadius (vw 1)
            , margin2 (vw 1) zero
            , letterSpacing (vw 0.1)
            ]
        ]
        [ text reason ]


viewRow : Int -> Maybe Int -> Cells -> Html Msg
viewRow row focusIndex cells =
    tr []
        [ viewCell focusIndex (indexOf row 1) (getCellAt row 1 cells)
        , viewCell focusIndex (indexOf row 2) (getCellAt row 2 cells)
        , viewCell focusIndex (indexOf row 3) (getCellAt row 3 cells)
        ]


viewCell : Maybe Int -> Int -> Cell -> Html Msg
viewCell focusIndex index cell =
    let
        val =
            case cell of
                Just char ->
                    String.fromChar char

                Nothing ->
                    ""

        isSelected =
            focusIndex == Just index
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
            , onWithOptions "keydown"
                { stopPropagation = False, preventDefault = False }
                (Json.map (KeyDownCell index) keyCode)
            , onInput (InputCell index)
            , id (toString index)
            , class
                (if isSelected then
                    "selected"
                 else
                    ""
                )
            , css
                (List.concat
                    [ [ fontSize (vw 5)
                      , textAlign center
                      , verticalAlign middle
                      , textTransform uppercase
                      , borderRadius (vw 1)
                      , fontWeight (int 600)
                      , boxSizing borderBox
                      , backgroundImage
                            (linearGradient2 (deg 180)
                                (stop2 (hex "fefbff") <| pct 20)
                                (stop2 (hex "fce5ff") <| pct 100)
                                []
                            )
                      , border3 (vw 0.4) solid (rgba 35 24 58 0.99)
                      , width (vw 8)
                      , height (vw 8)
                      , margin (vw 0.2)

                      -- focus
                      , focus
                            [ border3 (vw 0.4) solid (rgba 71 102 193 0.99)
                            , backgroundImage
                                (linearGradient2 (deg 180)
                                    (stop2 (hex "fefbe0") <| pct 20)
                                    (stop2 (hex "fce5e0") <| pct 100)
                                    []
                                )
                            ]
                      , outline zero -- remove, since we have custom focus
                      ]
                    ]
                 --|> List.reverse
                )
            ]
            []
        ]
