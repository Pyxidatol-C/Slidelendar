module Main exposing (main)

import Browser
import Html exposing (Html, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Time



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { yy : Int
    , mm : Int
    }


init : Model
init =
    { yy = 2019, mm = 8 }



-- UPDATE


type Msg
    = ChangeYY String
    | ChangeMM String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeYY strY ->
            let
                mY =
                    String.toInt strY
            in
            case mY of
                Nothing ->
                    model

                Just y ->
                    { model | yy = y }

        ChangeMM strM ->
            let
                mM =
                    String.toInt strM
            in
            case mM of
                Nothing ->
                    model

                Just m ->
                    if 1 <= m && m <= 12 then
                        { model | mm = m }

                    else
                        model



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
            [ value <| String.fromInt model.mm
            , type_ "number"
            , onInput ChangeMM
            ]
            []
        ]
