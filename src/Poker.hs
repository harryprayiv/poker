{-# LANGUAGE DeriveGeneric #-}

module Poker where

import Data.List (sort, sortBy)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import System.Random (StdGen, randomR)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Ord, Show, Read, Generic)

type Deck = [Card]
type Hand = [Card]
type Board = [Card]

data PokerHand = HighCard | Pair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)


fullDeck :: Deck
fullDeck = [Card r s | s <- [minBound .. maxBound], r <- [minBound .. maxBound]]


shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck _ [] = []
shuffleDeck g deck = let (i, g') = randomR (0, length deck - 1) g
                         (left, x:right) = splitAt i deck
                     in x : shuffleDeck g' (left ++ right)


dealCards :: Int -> Deck -> [(Hand, Deck)]
dealCards n deck = deal n (map pure deck)
  where
    deal _ [] = []
    deal 0 _  = []
    deal i d  = let (h, d') = splitAt n d in h : deal (i - 1) d'


evaluateHand :: Hand -> Board -> (PokerHand, [Card])
evaluateHand hand board = bestHand $ map (`evaluate` allCards) [minBound .. maxBound]
  where
    allCards = hand ++ board
    evaluate ph cs = (ph, bestCombination ph $ sort cs)
    bestHand = maximumBy (comparing fst)

    bestCombination :: PokerHand -> [Card] -> [Card]
    bestCombination HighCard = take 5
    bestCombination Pair = bestGroup 2
    bestCombination TwoPairs = \cs -> bestGroup 2 cs ++ bestGroup 2 (removeGroup 2 cs)
    bestCombination ThreeOfAKind = bestGroup 3
    bestCombination Straight = bestStraight
    bestCombination Flush = \cs -> take 5 $ filter ((== bestSuit cs) . suit) cs
    bestCombination FullHouse = \cs -> bestGroup 3 cs ++ bestGroup 2 cs
    bestCombination FourOfAKind = bestGroup 4
    bestCombination StraightFlush = \cs -> bestStraight $ filter ((== bestSuit cs) . suit) cs
    bestCombination RoyalFlush = \cs -> bestStraight $ filter ((== bestSuit cs) . suit) cs

    bestSuit cs = snd . maximum . map (\s -> (length (filter ((== s) . suit) cs), s)) $ [minBound .. maxBound]

    bestStraight cs = last . filter (isStraight . map rank) $ combinations 5 cs
      where
        isStraight rs = and . zipWith (==) rs $ tail rs

    bestGroup n cs = last . filter ((== n) . length) $ combinations n cs

    removeGroup n cs = filter (`notElem` bestGroup n cs) cs

    combinations 0 _  = [[]]
    combinations _ [] = []
    combinations n (x:xs) = map (x:) (combinations (n - 1) xs) ++ combinations n xs
