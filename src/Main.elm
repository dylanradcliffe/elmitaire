module Main exposing (main)

--import Cards exposing (Card, Game, Suit, initGame)

import Array
import Browser exposing (sandbox)
import Cards exposing (Card(..), Game, Selection(..), Suit(..), Target(..), cardBackColour, chrBack, chrCard, dealPile, initGame, moveCards, previewSize, resetPile, suitColour)
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate)
import Random.List



--------
-- MODEL
-------


type alias ModelGame =
    { game : Game
    , selection : Maybe Selection
    }


anySelected maybeSelected =
    case maybeSelected of
        Just s ->
            True

        Nothing ->
            False


type Model
    = Start
    | InGame ModelGame


type CardSide
    = FaceSide Card Bool
    | BackSide
    | Space



--------
-- VIEW
--------


cardStyles side =
    let
        common =
            [ style "font-size" "160px"
            , style "-webkit-user-select" "none"
            , style "-ms-user-select" "none"
            , style "user-select" "none"
            , style "background-color" "white"
            , style "line-height" "0.94em"
            , style "font-family" "Arial"
            ]
    in
    case side of
        FaceSide (Card suit n) selected ->
            common ++ [ style "border-style" "none", style "color" (suitColour suit selected) ]

        BackSide ->
            common ++ [ style "border-style" "none", style "color" cardBackColour ]

        Space ->
            common ++ [ style "border-style" "dotted", style "color" "white" ]


viewCard : CardSide -> Float -> Float -> Maybe Msg -> Html Msg
viewCard cardSide x y onC =
    let
        ocParam =
            case onC of
                Just oc ->
                    [ onClick oc ]

                Nothing ->
                    []

        cardc =
            case cardSide of
                FaceSide (Card suit n) sel ->
                    chrCard suit n

                BackSide ->
                    chrBack

                Space ->
                    -- white joker, will be white anyway
                    0x0001F0DF
    in
    button
        ([ style "position" "absolute"
         , style "left" (String.fromFloat x ++ "em")
         , style "top" (String.fromFloat y ++ "em")
         ]
            ++ ocParam
            ++ cardStyles cardSide
        )
        [ text
            (cardc
                |> Char.fromCode
                |> String.fromChar
            )
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

        cardSide i c sel =
            if i < column.flipsAt then
                BackSide

            else
                FaceSide c sel

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
            viewCard (cardSide i (Card s f) (isSelected i j))
                (x + 0.7 * toFloat j)
                (y + 0.25 * toFloat i)
                (Just
                    (if anySelected selected then
                        Targeting (TargetColumn j)

                     else
                        Select (SelectedColumn i j)
                    )
                )
        )
        column.cards


viewPile : List Card -> Float -> Float -> Html Msg
viewPile pile x y =
    viewCard
        (if List.isEmpty pile then
            Space

         else
            BackSide
        )
        x
        y
        (Just
            (if List.isEmpty pile then
                ResetPile

             else
                DealPile
            )
        )


viewPreview : List Card -> Maybe Selection -> Float -> Float -> Html Msg
viewPreview pile selected x y =
    let
        isSelected ii =
            case selected of
                Just SelectedPreview ->
                    ii == 0

                _ ->
                    False

        previewCard n i c =
            viewCard (FaceSide c (isSelected i))
                (x + 0.25 * toFloat (n - i))
                y
                (Just
                    (if anySelected selected then
                        Unselect

                     else
                        Select SelectedPreview
                    )
                )

        visiblePreview =
            List.take previewSize pile
    in
    div []
        (List.indexedMap
            (previewCard (List.length visiblePreview))
            visiblePreview
            |> List.reverse
        )


viewInGame : Game -> Maybe Selection -> Html Msg
viewInGame game selected =
    div
        []
        [ div [] [ viewPile game.pile 4.0 0.3 ]
        , div [] [ viewPreview game.preview selected 4.7 0.3 ]
        , div []
            -- columns
            (List.indexedMap (viewColumn 0.5 1.5 selected) (Array.toList game.columns)
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
    | Targeting Target
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

                Targeting target ->
                    InGame (executeTarget m target)

                Unselect ->
                    InGame { m | selection = Nothing }

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


executeTarget : ModelGame -> Target -> ModelGame
executeTarget game target =
    case game.selection of
        Nothing ->
            game

        Just curSel ->
            { game = moveCards game.game curSel target, selection = Nothing }


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
