module Main exposing (main)

--import Cards exposing (Card, Game, Suit, initGame)

import Browser exposing (sandbox)
import Cards exposing (Card(..), Game, Suit(..), initGame)
import Debug exposing (toString)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Random exposing (Seed, generate)
import Random.List
import View exposing (viewColumn, viewInGame, viewStart)


type alias ModelGame =
    { game : Game
    }


type Model
    = Start
    | InGame ModelGame


view model =
    case model of
        InGame m ->
            viewInGame m.game

        Start ->
            viewStart


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
