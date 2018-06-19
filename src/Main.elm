module Main exposing (..)

import Array exposing (Array)
import Char
import Common exposing (..)
import Dom
import Html.Styled as Html
import Set exposing (Set)
import Task
import View exposing (view)
import Words exposing (words)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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


getCharAt : Int -> Cells -> Maybe Char
getCharAt index cells =
    Array.get index cells
        |> Maybe.withDefault (Just '%')



-- should always find the cell


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

        checkedWordReasons =
            [ checkWord 0 Right words cells
            , checkWord 1 Right words cells
            , checkWord 2 Right words cells
            , checkWord 0 Down words cells
            , checkWord 1 Down words cells
            , checkWord 2 Down words cells
            ]
                |> List.filterMap identity

        reasons =
            [ if isIncomplete then
                [ "Blank cells!" ]
              else
                []
            , if hasDuplicateChars then
                [ "Duplicate characters!" ]
              else
                []
            , checkedWordReasons
            ]
                |> List.concat
    in
    if List.isEmpty reasons then
        Valid
    else
        Invalid reasons


type Dir
    = Right
    | Down


checkWord : Int -> Dir -> Set String -> Cells -> Maybe String
checkWord index dir words cells =
    let
        maybeWord =
            getWord index dir cells
    in
    case maybeWord of
        Just word ->
            if Set.member word words then
                Nothing
            else
                Just (String.toUpper word ++ " is not a word!")

        Nothing ->
            Nothing


getWord : Int -> Dir -> Cells -> Maybe String
getWord index dir cells =
    case dir of
        Right ->
            let
                chars =
                    [ getCharAt (index * 3) cells
                    , getCharAt (index * 3 + 1) cells
                    , getCharAt (index * 3 + 2) cells
                    ]
            in
            if List.member Nothing chars then
                Nothing
            else
                chars
                    |> List.map (Maybe.withDefault '&')
                    |> listCharsToStr
                    |> Just

        Down ->
            let
                chars =
                    [ getCharAt index cells
                    , getCharAt (index + 3) cells
                    , getCharAt (index + 6) cells
                    ]
            in
            if List.member Nothing chars then
                Nothing
            else
                chars
                    |> List.map (Maybe.withDefault '&')
                    |> listCharsToStr
                    |> Just


listCharsToStr : List Char -> String
listCharsToStr chars =
    chars
        |> List.map (\c -> String.cons c "")
        |> String.concat



-- UPDATE


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
