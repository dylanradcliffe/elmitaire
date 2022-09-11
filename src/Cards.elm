--module Cards exposing (Card, Game, Suit, initGame)


module Cards exposing (Card(..), Game, Suit(..), cardBackColour, chrBack, chrBase, chrCard, dealPile, initGame, previewSize, resetPile, suitColour)

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
    , flipsAt : Int
    }


type alias Game =
    { goals : List Goal
    , preview : List Card
    , pile : List Card
    , columns : List Column
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
