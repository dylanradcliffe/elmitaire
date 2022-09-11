module Main exposing (main)

--import Cards exposing (Card, Game, Suit, initGame)

import Browser exposing (sandbox)
import Cards exposing (Card(..), Game, Suit(..), cardBackColour, chrBack, chrCard, dealPile, initGame, previewSize, resetPile, suitColour)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate)
import Random.List



--------
-- MODEL
-------


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

        isPrimarySelected ii jj =
            case selected of
                Just (SelectedColumn n m) ->
                    (ii == n) && (jj == m)

                _ ->
                    False

        cardColour i s sel =
            if i < column.flipsAt then
                cardBackColour

            else
                suitColour s sel

        cardChar i (Card s f) =
            (if i < column.flipsAt then
                chrBack

             else
                chrCard s f
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
                 ]
                    ++ cardStyles
                    ++ (if i < column.flipsAt then
                            []

                        else
                            [ onClick
                                (if isPrimarySelected i j then
                                    Unselect

                                 else
                                    Select (SelectedColumn i j)
                                )
                            ]
                       )
                )
                [ text (cardChar i (Card s f)) ]
        )
        column.cards


viewPile : List Card -> Float -> Float -> Html Msg
viewPile pile x y =
    button
        ([ style "position" "absolute"
         , style "left" (String.fromFloat x ++ "em")
         , style "top" (String.fromFloat y ++ "em")
         , style "color" cardBackColour
         , onClick
            (if List.isEmpty pile then
                ResetPile

             else
                DealPile
            )
         ]
            ++ cardStyles
        )
        [ text
            (if List.isEmpty pile then
                -- Em Space
                0x2003 |> Char.fromCode |> String.fromChar

             else
                chrBack
                    |> Char.fromCode
                    |> String.fromChar
            )
        ]


viewPreview : List Card -> Maybe Selection -> Float -> Float -> Html Msg
viewPreview pile selected x y =
    let
        isSelected ii =
            case selected of
                Just SelectedPreview ->
                    ii == previewSize - 1

                _ ->
                    False

        previewCard i (Card s f) =
            button
                ([ style "position" "absolute"
                 , style "left" (String.fromFloat (x + 0.25 * toFloat i) ++ "em")
                 , style "top" (String.fromFloat y ++ "em")
                 , style "color" (suitColour s (isSelected i))
                 ]
                    ++ cardStyles
                    ++ (if i == previewSize - 1 then
                            [ onClick
                                (if isSelected i then
                                    Unselect

                                 else
                                    Select SelectedPreview
                                )
                            ]

                        else
                            []
                       )
                )
                [ text (chrCard s f |> Char.fromCode |> String.fromChar) ]
    in
    div []
        (List.indexedMap
            previewCard
            (List.take previewSize (List.reverse pile))
        )


viewInGame : Game -> Maybe Selection -> Html Msg
viewInGame game selected =
    div
        []
        [ div [] [ viewPile game.pile 4.0 0.3 ]
        , div [] [ viewPreview game.preview selected 4.7 0.3 ]
        , div []
            -- columns
            (List.indexedMap (viewColumn 0.5 1.5 selected) game.columns
                |> List.concat
            )
        ]


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


type Msg
    = Initialise (List Int)
    | Select Selection
    | Unselect
    | DealPile
    | ResetPile


update msg model =
    ( updateModelOnly msg model, Cmd.none )


updateModelOnly msg model =
    case model of
        InGame m ->
            case msg of
                Select selection ->
                    InGame { m | selection = Just selection }

                Unselect ->
                    InGame { m | selection = Maybe.Nothing }

                ResetPile ->
                    InGame { game = resetPile m.game, selection = Nothing }

                Initialise shuffled ->
                    InGame { game = initGame shuffled, selection = Nothing }

                DealPile ->
                    InGame { game = dealPile m.game, selection = Nothing }

        _ ->
            case msg of
                Initialise shuffled ->
                    InGame { game = initGame shuffled, selection = Nothing }

                _ ->
                    model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--------
-- INIT
--------


init : () -> ( Model, Cmd Msg )
init flags =
    ( Start, generate Initialise (Random.List.shuffle (List.range 0 52)) )



--------
-- MAIN
--------


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }
