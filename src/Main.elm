module Main exposing (main)

--import Cards exposing (Card, Game, Suit, initGame)

import Browser exposing (sandbox)
import Cards exposing (Card(..), Game, Suit(..), chrBase, initGame, suitColour)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate)
import Random.List



--------
-- MODEL
-------


type Msg
    = Initialise (List Int)
    | Select Selection
    | Unselect


type Selection
    = SelectedColumn Int Int
    | SelectedTarget Int
    | SelectedPreview


type Target
    = TargetColumn Int
    | TargetTarget Int


type alias ModelGame =
    { game : Game
    , selection : Maybe Selection
    }


type Model
    = Start
    | InGame ModelGame



--------
-- VIEW
--------


cardStyles =
    [ style "font-size" "160px"
    , style "-webkit-user-select" "none"
    , style "-ms-user-select" "none"
    , style "user-select" "none"
    , style "background-color" "white"
    , style "line-height" "0.94em"
    , style "font-family" "Arial"
    , style "border-style" "none"
    ]


viewColumn x y selected j column =
    let
        isSelected ii jj =
            case selected of
                Just (SelectedColumn n m) ->
                    (ii >= n) && (jj == m)

                _ ->
                    False

        cardColour i s sel =
            if i < column.flipsAt then
                "#000000"

            else
                suitColour s sel

        cardChar i (Card s f) =
            (if i < column.flipsAt then
                0x0001F0A0

             else
                chrBase s
                    + f
                    -- bypass the "knight"
                    + (if f > 10 then
                        2

                       else
                        1
                      )
            )
                |> Char.fromCode
                |> String.fromChar
    in
    List.indexedMap
        (\i (Card s f) ->
            button
                ([ style "position" "absolute"
                 , style "left" (String.fromFloat (x + 0.7 * toFloat j) ++ "em")
                 , style "top" (String.fromFloat (y + 0.25 * toFloat i) ++ "em")
                 , style "color" (cardColour i s (isSelected i j))
                 , onClick (Select (SelectedColumn i j))
                 ]
                    ++ cardStyles
                )
                [ text (cardChar i (Card s f)) ]
        )
        column.cards


viewInGame : Game -> Maybe Selection -> Html Msg
viewInGame game selected =
    div
        []
        (List.indexedMap (viewColumn 0.5 1.5 selected) game.columns
            |> List.concat
        )


viewStart : Html msg
viewStart =
    div [] []


view model =
    case model of
        InGame m ->
            viewInGame m.game m.selection

        Start ->
            viewStart



--------
-- UPDATE
--------


update msg model =
    case msg of
        Initialise shuffled ->
            ( InGame { game = initGame shuffled, selection = Nothing }, Cmd.none )

        Select selection ->
            let
                newModel =
                    case model of
                        InGame m ->
                            InGame { m | selection = Just selection }

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        Unselect ->
            let
                newModel =
                    case model of
                        InGame m ->
                            InGame { m | selection = Maybe.Nothing }

                        _ ->
                            model
            in
            ( newModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> ( Model, Cmd Msg )
init flags =
    ( Start, generate Initialise (Random.List.shuffle (List.range 0 52)) )



--------
-- MAIN
--------


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }
