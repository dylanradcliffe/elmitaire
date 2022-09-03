module Main exposing (main)

import Browser exposing (sandbox)
import Cards exposing (Game, initGame)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { game : Game
    }


type Msg
    = Initialise (List Int)


view : a -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , text (toString model)
        , button [ onClick Increment ] [ text "+" ]
        ]


update : Msg -> number -> number
update msg model =
    case msg of
        Decrement ->
            model - 1

        Increment ->
            model + 1


main : Program () number Msg
main =
    Browser.sandbox { init = 0, view = view, update = update }
