module Main exposing (main)

--import Cards exposing (Card, Game, Suit, initGame)

import Array
import Browser exposing (sandbox)
import Cards exposing (Card(..), Column, Game, Goal(..), Selection(..), Suit(..), Target(..), cardBackColour, chrBack, chrCard, dealPile, initGame, isGameWon, moveCards, previewSize, resetPile, suitColour)
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

        boxStyle =
            [ style "width" "0.52em", style "height" "0.85em", style "margin-left" "0.1em", style "margin-top" "0.05em" ]
    in
    case side of
        FaceSide (Card suit n) selected ->
            common
                ++ [ style "border-style" "none", style "color" (suitColour suit selected) ]

        BackSide ->
            common ++ [ style "border-style" "none", style "color" cardBackColour ]

        Space ->
            common
                ++ [ style "border-style" "dotted", style "color" "white" ]
                ++ boxStyle


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
                    -- picking a thin character will be white anyway
                    0x2595
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


viewWon : Game -> List (Html Msg)
viewWon game =
    if isGameWon game then
        [ div
            [ style "font-size" "40px"
            , style "background-color" "white"
            , style "line-height" "0.94em"
            , style "font-family" "Arial"
            , onClick ResetGame
            ]
            [ text "You Won!"
            ]
        ]

    else
        []


viewControls : Game -> Html Msg
viewControls game =
    div []
        ([ button [ onClick ResetGame ] [ text "Reset" ] ]
            ++ viewWon game
        )


viewColumn : Float -> Float -> Maybe Selection -> Int -> Column -> List (Html Msg)
viewColumn x y selected j column =
    if List.length column.cards > 0 then
        viewColumnNonEmpty x y selected j column

    else
        -- if no cards left in column
        [ viewCard Space
            (x + 0.7 * toFloat j)
            y
            (if anySelected selected then
                Just (Targeting (TargetColumn j))

             else
                Nothing
            )
        ]


viewColumnNonEmpty : Float -> Float -> Maybe Selection -> Int -> Column -> List (Html Msg)
viewColumnNonEmpty x y selected j column =
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
                (x + 0.25 * toFloat (n - i - 1))
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


viewGoal : Float -> Float -> Maybe Selection -> Int -> Goal -> Html Msg
viewGoal x y maybeSel i (Goal cards) =
    let
        xx =
            x + 0.7 * toFloat i
    in
    case List.head cards of
        Just card ->
            viewCard (FaceSide card False)
                xx
                y
                (Just
                    (if anySelected maybeSel then
                        Targeting (TargetGoal i)

                     else
                        Select (SelectedGoal i)
                    )
                )

        Nothing ->
            viewCard Space
                xx
                y
                (if anySelected maybeSel then
                    Just (Targeting (TargetGoal i))

                 else
                    Nothing
                )


viewGoals : Float -> Float -> Maybe Selection -> Array.Array Goal -> List (Html Msg)
viewGoals x y maybeSel arr =
    List.indexedMap (viewGoal x y maybeSel) (Array.toList arr)


viewInGame : Game -> Maybe Selection -> Html Msg
viewInGame game selected =
    div
        []
        [ div [] [ viewControls game ]
        , div []
            -- columns
            (List.indexedMap (viewColumn 0.5 1.5 selected) (Array.toList game.columns)
                |> List.concat
            )
        , div [] (viewGoals 0.5 0.3 selected game.goals)
        , div [] [ viewPile game.pile 3.5 0.3 ]
        , div [] [ viewPreview game.preview selected 4.2 0.3 ]
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
    | ResetGame


updateModelOnlyIngameNotWon msg m =
    case msg of
        Select selection ->
            ( InGame { m | selection = Just selection }, Cmd.none )

        Targeting target ->
            ( InGame (executeTarget m target), Cmd.none )

        Unselect ->
            ( InGame { m | selection = Nothing }, Cmd.none )

        ResetPile ->
            ( InGame { game = resetPile m.game, selection = Nothing }, Cmd.none )

        Initialise shuffled ->
            ( InGame { game = initGame shuffled, selection = Nothing }, Cmd.none )

        DealPile ->
            ( InGame { game = dealPile m.game, selection = Nothing }, Cmd.none )

        ResetGame ->
            ( InGame m, resetGame )


updateModelOnlyIngameWon msg m =
    case msg of
        Initialise shuffled ->
            ( InGame { game = initGame shuffled, selection = Nothing }, Cmd.none )

        ResetGame ->
            ( InGame m, resetGame )

        -- ignore other messages if won
        _ ->
            ( InGame m, Cmd.none )


updateModelOnlyIngame msg m =
    if isGameWon m.game then
        updateModelOnlyIngameWon msg m

    else
        updateModelOnlyIngameNotWon msg m


update msg model =
    case model of
        InGame m ->
            updateModelOnlyIngame msg m

        _ ->
            case msg of
                Initialise shuffled ->
                    ( InGame { game = initGame shuffled, selection = Nothing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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


resetGame =
    generate Initialise (Random.List.shuffle (List.range 0 52))


init : () -> ( Model, Cmd Msg )
init flags =
    ( Start, resetGame )



--------
-- MAIN
--------


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }
