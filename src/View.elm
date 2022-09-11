module View exposing (viewColumn, viewInGame, viewStart)

import Cards exposing (Card(..), Game, Suit(..), initGame)
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Messages exposing (Msg(..))
import String


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


viewColumn j column =
    let
        chrBase s =
            case s of
                Spades ->
                    0x0001F0A0

                Hearts ->
                    0x0001F0B0

                Diamonds ->
                    0x0001F0C0

                Cards.Clubs ->
                    0x0001F0D0

        suitColour s =
            case s of
                Spades ->
                    "#000000"

                Clubs ->
                    "#000000"

                Hearts ->
                    "#d10808"

                Diamonds ->
                    "#d10808"

        cardColour i s =
            if i < column.flipsAt then
                "#000000"

            else
                suitColour s

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
                 , style "left" (String.fromFloat (0.5 + 0.7 * toFloat j) ++ "em")
                 , style "top" (String.fromFloat (1.5 + 0.25 * toFloat i) ++ "em")
                 , style "color" (cardColour i s)
                 ]
                    ++ cardStyles
                )
                [ text (cardChar i (Card s f)) ]
        )
        column.cards


viewInGame : Game -> Html msg
viewInGame game =
    div
        []
        (List.indexedMap viewColumn game.columns
            |> List.concat
        )


viewStart : Html msg
viewStart =
    div [] []
