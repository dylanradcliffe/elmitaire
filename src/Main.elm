module Main exposing (main)

import Browser exposing (sandbox)
import Cards exposing (Game, initGame)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate)
import Random.List


type alias ModelGame =
    { game : Game
    }


type Model
    = Start
    | InGame ModelGame


type Msg
    = Initialise (List Int)


view model =
    div []
        [ button [] [ text "-" ]
        , text (toString model)
        , button [] [ text "+" ]
        ]


update msg model =
    case msg of
        Initialise shuffled ->
            ( InGame { game = initGame shuffled }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init flags =
    ( Start, generate Initialise (Random.List.shuffle (List.range 0 52)) )


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }
