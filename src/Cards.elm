--module Cards exposing (Card, Game, Suit, initGame)


module Cards exposing (Card(..), Game, Selection(..), Suit(..), Target(..), cardBackColour, chrBack, chrBase, chrCard, dealPile, initGame, moveCards, previewSize, resetPile, suitColour)

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


type alias Goal =
    { card :
        List Card
    , suit : Suit
    }


type alias Column =
    { cards : List Card
    , flipsAt : Int
    }


type alias Game =
    { goals : List Goal
    , preview : List Card
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
    { goals = []
    , preview = []
    , pile = initDeck (List.drop 28 shuffledList)
    , columns =
        List.map (columnList (initDeck shuffledList)) (List.range 0 6)
            |> Array.fromList
    }


resetPile : Game -> Game
resetPile game =
    { game | pile = game.preview, preview = [] }


dealPile : Game -> Game
dealPile game =
    { game | preview = game.preview ++ List.take previewSize game.pile, pile = List.drop previewSize game.pile }


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


cardFromColumns : Array Column -> Int -> Int -> Maybe Card
cardFromColumns columns i j =
    Array.get j columns
        |> withDefault { cards = [], flipsAt = 0 }
        |> .cards
        |> Array.fromList
        |> Array.get i


lastCardFromColumns : Array Column -> Int -> Maybe Card
lastCardFromColumns columns j =
    Array.get j columns
        |> withDefault { cards = [], flipsAt = 0 }
        |> .cards
        |> List.reverse
        |> List.head


cardFromSelection : Game -> Selection -> Maybe Card
cardFromSelection game sel =
    case sel of
        SelectedColumn i j ->
            cardFromColumns game.columns i j

        SelectedPreview ->
            List.reverse game.preview |> List.head

        SelectedGoal i ->
            Array.fromList game.goals
                |> Array.get i
                |> Maybe.andThen (\g -> List.head g.card)


cardFromTarget : Game -> Target -> Maybe Card
cardFromTarget game tar =
    case tar of
        TargetColumn i ->
            lastCardFromColumns game.columns i

        TargetGoal i ->
            Array.fromList game.goals
                |> Array.get i
                |> Maybe.andThen (\g -> List.head g.card)


suitRed : Suit -> Bool
suitRed s =
    case s of
        Spades ->
            False

        Clubs ->
            False

        _ ->
            True


compatibleColumnTarget : Card -> Card -> Bool
compatibleColumnTarget (Card originSuit originFace) (Card targetSuit targetFace) =
    xor (suitRed originSuit) (suitRed targetSuit) && (originFace == targetFace - 1)


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
    game



-- todo


moveCardsFromPreview : Game -> ( Game, List Card )
moveCardsFromPreview game =
    let
        newPreview =
            List.take (List.length game.preview - 1) game.preview

        takenCards =
            List.drop (List.length game.preview - 1) game.preview
    in
    ( { game | preview = newPreview }
    , takenCards
    )


moveCardsFromGoal : i -> Game -> ( Game, List Card )
moveCardsFromGoal i game =
    -- to fill in
    ( game, [] )


findMoveCards : Game -> Selection -> Target -> ( Maybe Card, Maybe Card )
findMoveCards g s t =
    ( cardFromSelection g s
    , cardFromTarget g t
    )


compatibleTarget : Target -> Card -> Card -> Bool
compatibleTarget target selCard tarCard =
    case target of
        TargetColumn _ ->
            compatibleColumnTarget selCard tarCard

        _ ->
            False


moveCards : Game -> Selection -> Target -> Game
moveCards g s t =
    case findMoveCards g s t of
        ( Just selCard, Just tarCard ) ->
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
            if compatibleTarget t selCard tarCard then
                source g
                    |> target

            else
                g

        _ ->
            g



{--Just selCard ->
            case cardFromTarget g t of
                Nothing ->
                    g

                Just tarCard ->
                    case t of
                        TargetColumn i ->
                            if compatibleColumnTarget selCard tarCard then
                                case s of
                                    SelectedColumn n m ->
                                        let
                                            newOrig =
                                                List.take (n - 1) (Array.get m g.columns)

                                            newTarg =
                                                List.take (Array.get i g.columns)
                                        in
                                        g

                                    _ ->
                                        g

                            else
                                g

                        TargetGoal i ->
                            g
--}
