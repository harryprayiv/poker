{-# LANGUAGE DeriveGeneric #-}

module Poker where

import Data.List (sort, sortBy)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import System.Random (StdGen, getStdGen, randomR)

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

data HandScore = HandScore { pokerHand :: PokerHand, primaryRanks :: [Rank], secondaryRanks :: [Rank] }
  deriving (Eq, Ord, Show)

data Player = Player
  { playerId :: Int
  , playerHand :: Hand
  , playerChips :: Int
  , playerAction :: PlayerAction
  }
  deriving (Eq, Show)

data PlayerAction = Fold | Check | Call | Raise Int
  deriving (Eq, Show)

data GameState = GameState
  { players :: [Player]
  , board :: Board
  , deck :: Deck
  , pot :: Int
  , currentPlayer :: Int
  , currentBet :: Int
  , playerBets :: [Int]
  , smallBlind :: Int
  , bigBlind :: Int
  , dealerPosition :: Int
  }
  deriving (Eq, Show)




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


evaluateHand :: Hand -> Board -> HandScore
evaluateHand hand board = bestHand $ map (`evaluate` allCards) [minBound .. maxBound]
  where
    allCards = hand ++ board
    evaluate ph cs = HandScore ph (bestCombination ph $ sort cs) (secondaryCards ph $ sort cs)
    bestHand = maximum

    bestCombination :: PokerHand -> [Card] -> [Rank]
    bestCombination HighCard = map rank . take 5
    bestCombination Pair = map rank . bestGroup 2
    bestCombination TwoPairs = \cs -> map rank (bestGroup 2 cs ++ bestGroup 2 (removeGroup 2 cs))
    bestCombination ThreeOfAKind = map rank . bestGroup 3
    bestCombination Straight = map rank . bestStraight
    bestCombination Flush = \cs -> map rank $ take 5 $ filter ((== bestSuit cs) . suit) cs
    bestCombination FullHouse = \cs -> map rank (bestGroup 3 cs ++ bestGroup 2 cs)
    bestCombination FourOfAKind = map rank . bestGroup 4
    bestCombination StraightFlush = map rank . bestStraight . filter ((== bestSuit allCards) . suit)
    bestCombination RoyalFlush = map rank . bestStraight . filter ((== bestSuit allCards) . suit)

    secondaryCards :: PokerHand -> [Card] -> [Rank]
    secondaryCards HighCard _ = []
    secondaryCards Pair = map rank . filter (not . (`elem` bestGroup 2 allCards))
    secondaryCards TwoPairs = map rank . filter (not . (`elem` (bestGroup 2 allCards ++ bestGroup 2 (removeGroup 2 allCards))))
    secondaryCards ThreeOfAKind = map rank . filter (not . (`elem` bestGroup 3 allCards))
    secondaryCards Straight _ = []
    secondaryCards Flush _ = []
    secondaryCards FullHouse _ = []
    secondaryCards FourOfAKind = map rank . filter (not . (`elem` bestGroup 4 allCards))
    secondaryCards StraightFlush _ = []
    secondaryCards RoyalFlush _ = []

    -- ... other helper functions from previous implementation ...

initialGameState :: Int -> Int -> Int -> Int -> GameState
initialGameState numPlayers startingChips sb bb = GameState
  { players = zipWith (\i h -> Player i h startingChips Check) [1 .. numPlayers] initialHands
  , board = []
  , deck = remainingDeck
  , pot = 0
  , currentPlayer = 1
  , currentBet = 0
  , playerBets = replicate numPlayers 0
  , smallBlind = sb
  , bigBlind = bb
  , dealerPosition = 1
  }
  where
    (initialHands, remainingDeck) = unzip . dealCards numPlayers . shuffleDeck' $ fullDeck
    shuffleDeck' = fst . shuffleDeck (getStdGen)


--  The Big Kahuna
playGame :: GameState -> IO ()
playGame gameState = do
  putStrLn "Starting a new hand..."
  -- Reset the board, currentBet, and playerBets
  let gameState' = gameState { board = [], currentBet = 0, playerBets = replicate (length $ players gameState) 0 }

  -- Pre-flop betting round
  gameState'' <- bettingRound gameState'

  -- Dealing the flop
  putStrLn "Dealing the flop..."
  let (flopCards, deck') = splitAt 3 $ deck gameState''
  let gameState''' = gameState'' { board = flopCards, deck = deck' }

  -- Post-flop betting round
  gameState'''' <- bettingRound gameState'''

  -- Dealing the turn
  putStrLn "Dealing the turn..."
  let (turnCard : deck''') = deck gameState''''
  let gameState''''' = gameState'''' { board = turnCard : board gameState'''', deck = deck''' }

  -- Post-turn betting round
  gameState'''''' <- bettingRound gameState'''''

  -- Dealing the river
  putStrLn "Dealing the river..."
  let (riverCard : deck'''') = deck gameState''''''
  let gameState''''''' = gameState'''''' { board = riverCard : board gameState'''''', deck = deck'''' }

  -- Post-river betting round
  gameState'''''''' <- bettingRound gameState'''''''

  -- Determining the winner(s) and distributing the pot
  let winner = determineWinner gameState''''''''
  putStrLn $ "Player " ++ show (playerId winner) ++ " wins the pot!"

  -- Handling the end of the game or moving on to the next hand
  putStrLn "Press 'q' to quit or any other key to play another hand."
  continue <- getLine
  if continue == "q"
    then putStrLn "Thanks for playing!"
    else playGame gameState''''''''

  where
    bettingRound :: GameState -> IO GameState
    bettingRound gs = do
      -- TODO: Implement the actual betting round with player actions (bet, call, fold, etc.) and user input handling.
      -- For now, this is a stub that returns the same game state.
      return gs

    determineWinner :: GameState -> Player



