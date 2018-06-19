port module Main exposing (..)

import Array exposing (Array)
import Char
import Common exposing (..)
import Dom
import Html.Styled as Html
import Set exposing (Set)
import Task
import View exposing (view)
import Words exposing (words)


port select : String -> Cmd msg


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
    , select "0"
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


nextIndex : Int -> Int
nextIndex index =
    if index + 1 > 8 then
        0
    else
        index + 1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusCell index ->
            { model | focusIndex = Just index } ! []

        ClickCell index ->
            { model | focusIndex = Just index }
                ! [ select (index |> toString) ]

        InputCell index keycode ->
            case keycodeToAction keycode index of
                MoveToIndex index ->
                    { model | focusIndex = Just index }
                        ! [ select (index |> toString) ]

                NewChar char ->
                    let
                        cells =
                            setCellAt index char model.cells
                    in
                    { model
                        | focusIndex = Just (nextIndex index)
                        , cells = cells
                        , status = validate cells
                    }
                        ! [ select (nextIndex index |> toString) ]

                NoOp ->
                    model ! []


type KeycodeAction
    = MoveToIndex Int
    | NewChar Char
    | NoOp


keycodeToAction : Int -> Int -> KeycodeAction
keycodeToAction keycode index =
    case keycode of
        38 ->
            -- up
            if (index - 3) < 0 then
                MoveToIndex <| index
            else
                MoveToIndex <| index - 3

        40 ->
            -- down
            if (index + 3) > 8 then
                MoveToIndex <| index
            else
                MoveToIndex <| index + 3

        37 ->
            -- left
            if List.member index [ 0, 3, 6 ] then
                MoveToIndex <| index
            else
                MoveToIndex <| index - 1

        39 ->
            -- right
            if List.member index [ 2, 5, 8 ] then
                MoveToIndex <| index
            else
                MoveToIndex <| index + 1

        nonarrow ->
            if nonarrow >= 65 && nonarrow <= 90 then
                nonarrow
                    |> Char.fromCode
                    |> Char.toLower
                    |> NewChar
            else
                NoOp


setCellAt : Int -> Char -> Cells -> Cells
setCellAt index char cells =
    Array.set index (Just char) cells



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
