module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Iso8601
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { yy : Int
    , mm : Time.Month
    , tz : Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 2019 Time.May Time.utc
    , Task.map2 (\a b -> ( a, b )) Time.now Time.here
        |> Task.perform SetTime
    )



-- UPDATE


type Msg
    = ChangeYY String
    | ChangeMM String
    | SetTime ( Time.Posix, Time.Zone )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeYY strY ->
            case String.toInt strY of
                Nothing ->
                    ( model, Cmd.none )

                Just y ->
                    ( { model | yy = y }, Cmd.none )

        ChangeMM strM ->
            case String.toInt strM of
                Nothing ->
                    ( model, Cmd.none )

                Just mInt ->
                    if mInt == 0 then
                        ( { model | mm = Time.Dec, yy = model.yy - 1 }, Cmd.none )

                    else if mInt == 13 then
                        ( { model | mm = Time.Jan, yy = model.yy + 1 }, Cmd.none )

                    else
                        intToMonth mInt
                            |> Maybe.andThen (\m -> Just ( { model | mm = m }, Cmd.none ))
                            |> Maybe.withDefault ( model, Cmd.none )

        SetTime ( time, tz ) ->
            let
                m =
                    Time.toMonth tz time

                y =
                    Time.toYear tz time
            in
            ( Model y m tz, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "slidelendar" ]
        [ viewCalendar model
        , viewInput model
        ]


viewInput : Model -> Html Msg
viewInput model =
    div [ class "slidelendar-inputs" ]
        [ input
            [ value <| String.fromInt model.yy
            , type_ "number"
            , onInput ChangeYY
            ]
            []
        , input
            [ value <| String.fromInt <| monthToInt model.mm
            , type_ "number"
            , onInput ChangeMM
            ]
            []
        ]


viewCalendar : Model -> Html msg
viewCalendar model =
    let
        frameLeftEnd =
            case firstDayOfMonth model.tz model.yy model.mm of
                -- This should never happen.
                -- If it somehow did, the red frame will not appear
                Nothing ->
                    -9

                Just wd ->
                    weekdayToInt wd - 1

        cellContents =
            [ [ "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  " ]
            , [ "  ", "  ", "  ", "  ", "  ", "  ", "  ", " 1", " 2", " 3", " 4", " 5", " 6", " 7", "  " ]
            , [ "  ", " 2", " 3", " 4", " 5", " 6", " 7", " 8", " 9", "10", "11", "12", "13", "14", "  " ]
            , [ "  ", " 9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "  " ]
            , [ "  ", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "  " ]
            , [ "  ", "23", "24", "25", "26", "27", "28", "29", "30", "31", "  ", "  ", "  ", "  ", "  " ]
            , [ "  ", "30", "31", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  " ]
            , [ "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  ", "  " ]
            ]

        isFrame : Int -> Int -> Bool
        isFrame i j =
            if i == 0 || i == 7 then
                frameLeftEnd <= j && j <= frameLeftEnd + 8

            else
                frameLeftEnd == j || j == frameLeftEnd + 8

        makeCell : Int -> Int -> String -> Html msg
        makeCell i j content =
            let
                cellIsFrame =
                    isFrame i j

                content_ =
                    if cellIsFrame then
                        if i == 0 && frameLeftEnd < j && j < frameLeftEnd + 8 then
                            j - frameLeftEnd |> intToWeekdayStr

                        else
                            "  "

                    else
                        content
            in
            div
                [ classList
                    [ ( "slidelendar-frame", isFrame i j )
                    , ( "slidelendar-cell", True )
                    ]
                ]
                [ text content_ ]

        makeRow : Int -> List String -> Html msg
        makeRow i contents =
            div
                [ class "slidelendar-row" ]
            <|
                List.indexedMap (makeCell i) contents
    in
    div
        [ class "slidelendar-calendar" ]
        (List.indexedMap makeRow cellContents)


firstDayOfMonth : Time.Zone -> Int -> Time.Month -> Maybe Time.Weekday
firstDayOfMonth tz y m =
    let
        yyyy =
            String.fromInt y |> String.padLeft 4 '0'

        mm =
            monthToInt m |> String.fromInt |> String.padLeft 2 '0'
    in
    case Iso8601.toTime <| yyyy ++ "-" ++ mm ++ "-" ++ "01" of
        Ok time ->
            Just <| Time.toWeekday tz time

        _ ->
            Nothing


monthToInt : Time.Month -> Int
monthToInt m =
    case m of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


intToMonth : Int -> Maybe Time.Month
intToMonth i =
    case i of
        1 ->
            Just Time.Jan

        2 ->
            Just Time.Feb

        3 ->
            Just Time.Mar

        4 ->
            Just Time.Apr

        5 ->
            Just Time.May

        6 ->
            Just Time.Jun

        7 ->
            Just Time.Jul

        8 ->
            Just Time.Aug

        9 ->
            Just Time.Sep

        10 ->
            Just Time.Oct

        11 ->
            Just Time.Nov

        12 ->
            Just Time.Dec

        _ ->
            Nothing


weekdayToInt : Time.Weekday -> Int
weekdayToInt wd =
    case wd of
        Time.Mon ->
            1

        Time.Tue ->
            2

        Time.Wed ->
            3

        Time.Thu ->
            4

        Time.Fri ->
            5

        Time.Sat ->
            6

        Time.Sun ->
            7


intToWeekdayStr : Int -> String
intToWeekdayStr i =
    case i of
        1 ->
            "一"

        2 ->
            "二"

        3 ->
            "三"

        4 ->
            "四"

        5 ->
            "五"

        6 ->
            "六"

        7 ->
            "日"

        _ ->
            "  "
