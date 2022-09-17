--module Cards exposing (Card, Game, Suit, initGame)


module Cards exposing (Card(..), Column, Game, Goal(..), Selection(..), Suit(..), Target(..), cardBackColour, chrBack, chrBase, chrCard, dealPile, initGame, isGameWon, moveCards, previewSize, resetPile, suitColour)

import Array exposing (Array)
import Basics exposing (modBy)
import Html exposing (col, s)
import Maybe exposing (andThen, withDefault)


type Suit
    = Spades
    | Hearts
    | Diamonds
    | Clubs


suitNumberToSuit n =
    case n of
        0 ->
            Spades

        1 ->
            Hearts

        2 ->
            Diamonds

        _ ->
            Clubs


type Card
    = Card Suit Int


initDeck cardList =
    let
        cardNumberToSuit n =
            suitNumberToSuit (n // 13)

        cardNumberToFace n =
            modBy 13 n

        cardNumbertoCard n =
            Card (cardNumberToSuit n) (cardNumberToFace n)
    in
    List.map cardNumbertoCard cardList


type Goal
    = -- first card is on top
      Goal (List Card)


type alias Column =
    { cards : List Card
    , flipsAt : Int
    }


type alias Game =
    { goals : Array Goal
    , preview : List Card -- first one is the one on "top"
    , pile : List Card
    , columns : Array Column
    }


initGame : List Int -> Game
initGame shuffledList =
    let
        columnList cardList n =
            let
                fullList =
                    List.take (n + 1) (List.drop ((n * (n + 1)) // 2) cardList)
            in
            { cards = fullList, flipsAt = n }
    in
    { goals = Array.fromList [ Goal [], Goal [], Goal [], Goal [] ]
    , preview = []
    , pile = initDeck (List.drop 28 shuffledList)
    , columns =
        List.map (columnList (initDeck shuffledList)) (List.range 0 6)
            |> Array.fromList
    }



{--
    { goals =
        Array.fromList (List.map (\x -> Goal (List.reverse (initDeck (List.range (x * 13) (x * 13 + 11))))) (List.range 0 3))
    , preview = []
    , pile = []
    , columns = Array.fromList (List.map (\x -> { cards = x, flipsAt = 0 }) [ [ Card Spades 12 ], [ Card Hearts 12 ], [ Card Diamonds 12 ], [ Card Clubs 12 ] ])
    }
    --}


resetPile : Game -> Game
resetPile game =
    { game | pile = List.reverse game.preview, preview = [] }


dealPile : Game -> Game
dealPile game =
    { game
        | preview = (List.take previewSize game.pile |> List.reverse) ++ game.preview
        , pile = List.drop previewSize game.pile
    }


chrBase s =
    case s of
        Spades ->
            0x0001F0A0

        Hearts ->
            0x0001F0B0

        Diamonds ->
            0x0001F0C0

        Clubs ->
            0x0001F0D0


chrBack =
    0x0001F0A0


chrCard s f =
    chrBase s
        + f
        -- bypass the "knight"
        + (if f > 10 then
            2

           else
            1
          )


suitColour s highlight =
    let
        baseColour =
            case s of
                Spades ->
                    ( "#000000", "#87ed1a" )

                Clubs ->
                    ( "#000000", "#87ed1a" )

                Hearts ->
                    ( "#d10808", "#87ed1a" )

                Diamonds ->
                    ( "#d10808", "#87ed1a" )
    in
    if highlight then
        Tuple.second baseColour

    else
        Tuple.first baseColour


cardBackColour =
    "#000000"


previewSize =
    3


type Selection
    = SelectedColumn Int Int
    | SelectedGoal Int
    | SelectedPreview


type Target
    = TargetColumn Int
    | TargetGoal Int


cardsFromColumns : Array Column -> Int -> Int -> List Card
cardsFromColumns columns i j =
    Array.get j columns
        |> withDefault { cards = [], flipsAt = 0 }
        |> .cards
        |> List.drop i


lastCardFromColumns : Array Column -> Int -> Maybe Card
lastCardFromColumns columns j =
    Array.get j columns
        |> withDefault { cards = [], flipsAt = 0 }
        |> .cards
        |> List.reverse
        |> List.head


cardsFromSelection : Game -> Selection -> List Card
cardsFromSelection game sel =
    case sel of
        SelectedColumn i j ->
            cardsFromColumns game.columns i j

        SelectedPreview ->
            List.take 1 game.preview

        SelectedGoal i ->
            game.goals
                |> Array.get i
                |> withDefault (Goal [])
                |> (\(Goal cards) -> List.take 1 cards)


cardFromTarget : Game -> Target -> Maybe Card
cardFromTarget game tar =
    case tar of
        TargetColumn i ->
            lastCardFromColumns game.columns i

        TargetGoal i ->
            game.goals
                |> Array.get i
                |> Maybe.andThen (\(Goal cards) -> List.head cards)


suitRed : Suit -> Bool
suitRed s =
    case s of
        Spades ->
            False

        Clubs ->
            False

        _ ->
            True



-- Need to check as can only have one goal per suit


isGoalFull : Goal -> Bool
isGoalFull (Goal cards) =
    List.length cards == 13


isGameWon : Game -> Bool
isGameWon game =
    (List.filter isGoalFull (Array.toList game.goals)
        |> List.length
    )
        == 4


compatibleColumnTarget : List Card -> Maybe Card -> Bool
compatibleColumnTarget selCards maybeTarget =
    case selCards of
        [] ->
            False

        (Card originSuit originFace) :: _ ->
            case maybeTarget of
                -- kings can go on nothing
                Nothing ->
                    originFace == 12

                Just (Card targetSuit targetFace) ->
                    xor (suitRed originSuit) (suitRed targetSuit) && (originFace == targetFace - 1)


compatibleGoalTarget : Game -> List Card -> Maybe Card -> Bool
compatibleGoalTarget game selCards maybeTarget =
    case selCards of
        -- only allow one card to be selected!
        (Card originSuit originFace) :: [] ->
            case maybeTarget of
                Nothing ->
                    originFace == 0

                Just (Card targetSuit targetFace) ->
                    (originSuit == targetSuit) && (originFace == targetFace + 1)

        _ ->
            False


columnRemoveCards : Column -> Int -> ( Column, List Card )
columnRemoveCards column i =
    ( { cards = List.take i column.cards
      , flipsAt = min column.flipsAt (i - 1)
      }
    , List.drop i column.cards
    )


columnAddCards : Column -> List Card -> Column
columnAddCards column cards =
    { column | cards = column.cards ++ cards }


moveCardsFromColumn : Int -> Int -> Game -> ( Game, List Card )
moveCardsFromColumn i j game =
    let
        oldColumn =
            Array.get j game.columns
                |> withDefault { cards = [], flipsAt = 0 }

        ( newColumn, takenCards ) =
            columnRemoveCards oldColumn i

        newColumns =
            Array.set j newColumn game.columns
    in
    ( { game | columns = newColumns }
    , takenCards
    )


moveCardsToColumn : Int -> ( Game, List Card ) -> Game
moveCardsToColumn j ( game, cards ) =
    let
        oldColumn =
            Array.get j game.columns
                |> withDefault { cards = [], flipsAt = 0 }

        newColumn =
            columnAddCards oldColumn cards

        newColumns =
            Array.set j newColumn game.columns
    in
    { game | columns = newColumns }


moveCardsToGoal : Int -> ( Game, List Card ) -> Game
moveCardsToGoal i ( game, cards ) =
    let
        oldGoal =
            Array.get i game.goals
                |> withDefault (Goal [])

        oldGoalCards =
            case oldGoal of
                Goal c ->
                    c

        newGoal =
            Goal (cards ++ oldGoalCards)

        newGoals =
            Array.set i newGoal game.goals
    in
    { game | goals = newGoals }



-- todo


moveCardsFromPreview : Game -> ( Game, List Card )
moveCardsFromPreview game =
    let
        newPreview =
            List.drop 1 game.preview

        takenCards =
            List.take 1 game.preview
    in
    ( { game | preview = newPreview }
    , takenCards
    )


moveCardsFromGoal : i -> Game -> ( Game, List Card )
moveCardsFromGoal i game =
    -- to fill in
    ( game, [] )



{--

findMoveCards : Game -> Selection -> Target -> ( Maybe Card, Maybe Card )
findMoveCards g s t =
    ( cardFromSelection g s
    , cardFromTarget g t
    )
--}


compatibleTarget : Game -> Target -> Selection -> Bool
compatibleTarget game target selection =
    let
        tarCard =
            cardFromTarget game target

        selCards =
            cardsFromSelection game selection
    in
    case target of
        TargetColumn j ->
            compatibleColumnTarget selCards tarCard

        TargetGoal i ->
            compatibleGoalTarget game selCards tarCard


moveCards : Game -> Selection -> Target -> Game
moveCards g s t =
    let
        source =
            case s of
                SelectedColumn i j ->
                    moveCardsFromColumn i j

                SelectedPreview ->
                    moveCardsFromPreview

                SelectedGoal i ->
                    moveCardsFromGoal i

        target =
            case t of
                TargetColumn j ->
                    moveCardsToColumn j

                TargetGoal i ->
                    moveCardsToGoal i
    in
    if compatibleTarget g t s then
        source g
            |> target

    else
        g
