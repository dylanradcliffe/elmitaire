module Cards exposing (Game, initGame)

import Basics exposing (modBy)


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
    , displayed : Bool
    }


type alias Game =
    { goals : List Goal
    , preview : List Card
    , pile : List Card
    , columns : List Column
    }


initGame shuffledList =
    let
        columnList cardList n =
            List.take (n + 1) (List.drop ((n * (n + 1)) // 2) cardList)
    in
    { goals = []
    , preview = []
    , pile = initDeck (List.drop 28 shuffledList)
    , columns =
        List.map
            (\x -> { cards = initDeck x, displayed = True })
            (List.map (columnList shuffledList) (List.range 0 6))
    }
