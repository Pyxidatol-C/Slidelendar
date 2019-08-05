module Main exposing (main)

import Browser
import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
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
            case String.toInt strM |> Maybe.andThen intToMonth of
                Nothing ->
                    ( model, Cmd.none )

                Just m ->
                    ( { model | mm = m }, Cmd.none )

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
    div []
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
