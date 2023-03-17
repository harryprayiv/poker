{-# LANGUAGE DeriveGeneric #-}

module TexasHoldem where

import Data.List (sort, sortBy, groupBy, maximumBy)
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

data PlayerStatus = Active | Folded | Check
  deriving (Eq, Show)

data Player = Player
  { playerId :: Int
  , playerHand :: Hand
  , playerChips :: Int
  , playerStatus :: PlayerStatus
  , allInAmount :: Int
  }
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

bestSuit :: [Card] -> Suit
bestSuit cards = fst $ maximumBy (comparing snd) suitCounts
  where
    suitGroups = groupBy (\c1 c2 -> suit c1 == suit c2) . sortBy (comparing suit) $ cards
    suitCounts = map (\group -> (suit (head group), length group)) suitGroups

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

determineWinner :: GameState -> GameState
determineWinner gameState =
  let activePlayers = filter (\p -> playerStatus p == Active) $ players gameState
      playerHandScores = map (\p -> (p, evaluateHand (playerHand p) (board gameState))) activePlayers
      sortedPlayerHandScores = sortBy (flip (comparing snd)) playerHandScores
      bestHandScore = snd $ head sortedPlayerHandScores
      winners = map fst $ takeWhile ((== bestHandScore) . snd) sortedPlayerHandScores
  in distributePot gameState winners

initialGameState :: Int -> Int -> Int -> Int -> IO GameState
initialGameState numPlayers startingChips sb bb = do
  stdGen <- getStdGen
  let (initialHands, remainingDeck) = unzip . dealCards numPlayers . shuffleDeck stdGen $ fullDeck
  return GameState
    { players = zipWith (\i h -> Player i h startingChips Active 0) [1 .. numPlayers] initialHands
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

postBlinds :: GameState -> GameState
postBlinds gameState = gameState
  { players = updatedPlayers
  , pot = pot gameState + smallBlind gameState + bigBlind gameState
  , dealerPosition = newDealerPosition
  , currentPlayer = newCurrentPlayer
  , playerBets = updatedPlayerBets
  }
  where
    numPlayers = length $ players gameState
    oldDealerPosition = dealerPosition gameState
    newDealerPosition = (oldDealerPosition `mod` numPlayers) + 1
    smallBlindPosition = (newDealerPosition `mod` numPlayers) + 1
    bigBlindPosition = (smallBlindPosition `mod` numPlayers) + 1
    newCurrentPlayer = (bigBlindPosition `mod` numPlayers) + 1

    updatePlayerChips pos blind = \player ->
      if playerId player == pos
        then player { playerChips = playerChips player - blind }
        else player

    updatePlayerBet pos blind = \bet player ->
      if playerId player == pos
        then blind
        else bet

    updatedPlayers =
      map (updatePlayerChips smallBlindPosition (smallBlind gameState)) .
      map (updatePlayerChips bigBlindPosition (bigBlind gameState)) $
      players gameState

    updatedPlayerBets = zipWith (updatePlayerBet smallBlindPosition (smallBlind gameState)) (playerBets gameState) (players gameState) ++
                        zipWith (updatePlayerBet bigBlindPosition (bigBlind gameState)) (playerBets gameState) (players gameState)

postBlindsHeadsUp :: GameState -> GameState
postBlindsHeadsUp gameState = gameState
  { players = updatedPlayers
  , pot = pot gameState + smallBlind gameState + bigBlind gameState
  , dealerPosition = newDealerPosition
  , currentPlayer = newCurrentPlayer
  , playerBets = updatedPlayerBets
  }
  where
    numPlayers = length $ players gameState
    oldDealerPosition = dealerPosition gameState
    newDealerPosition = (oldDealerPosition `mod` numPlayers) + 1
    smallBlindPosition = newDealerPosition
    bigBlindPosition = (newDealerPosition `mod` numPlayers) + 1
    newCurrentPlayer = bigBlindPosition

    updatePlayerChips pos blind = \player ->
      if playerId player == pos
        then player { playerChips = playerChips player - blind }
        else player

    updatePlayerBet pos blind = \bet player ->
      if playerId player == pos
        then blind
        else bet

    updatedPlayers =
      map (updatePlayerChips smallBlindPosition (smallBlind gameState)) .
      map (updatePlayerChips bigBlindPosition (bigBlind gameState)) $
      players gameState

    updatedPlayerBets = zipWith (updatePlayerBet smallBlindPosition (smallBlind gameState)) (playerBets gameState) (players gameState) ++
                        zipWith (updatePlayerBet bigBlindPosition (bigBlind gameState)) (playerBets gameState) (players gameState)

    -- ... other helper functions from previous implementation ...
distributePot :: GameState -> [Player] -> GameState
distributePot gameState winners =
  let sortedWinners = sortBy (comparing playerChips) winners
      (_, updatedGameState) = foldl distributeChips (0, gameState) sortedWinners
  in updatedGameState
  where
    distributeChips :: (Int, GameState) -> Player -> (Int, GameState)
    distributeChips (previousAllIn, gameState) player =
      let playerId = Poker.playerId player
          playerIndex = playerId - 1
          playerContribution = playerBets gameState !! playerIndex
          distributablePot = min (pot gameState - previousAllIn) playerContribution
          playerShare = distributablePot `div` length winners
          updatedChips = playerChips player + playerShare
          updatedPlayer = player { playerChips = updatedChips }
          updatedPlayers = take playerIndex (players gameState) ++ [updatedPlayer] ++ drop (playerIndex + 1) (players gameState)
          updatedPot = pot gameState - playerShare
          newAllIn = previousAllIn + playerShare
      in (newAllIn, gameState { players = updatedPlayers, pot = updatedPot })


bettingRound :: GameState -> IO GameState
bettingRound gameState = do
  let numPlayers = length $ players gameState
  let activePlayers = filter (\p -> playerStatus p == Active) $ players gameState

  -- Recursive function to handle each player's turn
  let playerTurn gameState currentPlayerId = do
        let player = players gameState !! (currentPlayerId - 1)
        if playerStatus player == Folded
          then return gameState
          else do
            putStrLn $ "Player " ++ show currentPlayerId ++ "'s turn."
            putStrLn "Choose an action (bet, call, fold):"
            action <- getLine
            case action of
              "bet" -> do
                putStrLn "Enter your bet:"
                betAmount <- min (playerChips player) <$> readLn
                let allIn = playerChips player <= betAmount
                let updatedPlayer = player { playerChips = playerChips player - betAmount, allInAmount = if allIn then playerChips player else 0 }
                let updatedPlayers = take (currentPlayerId - 1) (players gameState) ++ [updatedPlayer] ++ drop currentPlayerId (players gameState)                
                let updatedPlayerBets = take (currentPlayerId - 1) (playerBets gameState) ++ [currentBet gameState + betAmount] ++ drop currentPlayerId (playerBets gameState)
                let updatedGameState = gameState { players = updatedPlayers, currentBet = currentBet gameState + betAmount, playerBets = updatedPlayerBets }
                playerTurn updatedGameState ((currentPlayerId + 1 - 1) `mod` length activePlayers + 1)
              "call" -> do
                let callAmount = min (playerChips player) (currentBet gameState - (playerBets gameState !! (currentPlayerId - 1)))
                let updatedPlayer = player { playerChips = playerChips player - callAmount }
                let updatedPlayers = take (currentPlayerId - 1) (players gameState) ++ [updatedPlayer] ++ drop currentPlayerId (players gameState)
                let updatedPlayerBets = take (currentPlayerId - 1) (playerBets gameState) ++ [currentBet gameState] ++ drop currentPlayerId (playerBets gameState)
                let updatedGameState = gameState { players = updatedPlayers, playerBets = updatedPlayerBets }
                playerTurn updatedGameState ((currentPlayerId + 1 - 1) `mod` length activePlayers + 1)
              "fold" -> do
                let updatedPlayer = player { playerStatus = Folded }
                let updatedPlayers = take (currentPlayerId - 1) (players gameState) ++ [updatedPlayer] ++ drop currentPlayerId (players gameState)
                let updatedGameState = gameState { players = updatedPlayers }
                playerTurn updatedGameState ((currentPlayerId + 1 - 1) `mod` length activePlayers + 1)
              _ -> do
                putStrLn "Invalid action. Please enter 'bet', 'call', or 'fold'."
                playerTurn gameState currentPlayerId

  -- Start the betting round with the first active player
  playerTurn gameState (currentPlayer gameState)

playGame :: Int -> Int -> Int -> Int -> IO ()
playGame numPlayers startingChips sb bb = do
  initialState <- initialGameState numPlayers startingChips sb bb
  gameLoop initialState
  where
    gameLoop :: GameState -> IO ()
    gameLoop gameState = do
      putStrLn "Starting a new hand..."
      -- Reset the board, currentBet, and playerBets
      let gameState' = gameState { board = [], currentBet = 0, playerBets = replicate (length $ players gameState) 0 }

      -- Post blinds and update dealer position
      let gameState'' = postBlinds gameState'

      -- Pre-flop betting round
      gameState''' <- bettingRound gameState''

      -- Dealing the flop
      putStrLn "Dealing the flop..."
      let (flopCards, deck') = splitAt 3 $ deck gameState'''
      let gameState'''' = gameState''' { board = flopCards, deck = deck' }

      -- Post-flop betting round
      gameState''''' <- bettingRound gameState''''

      -- Dealing the turn
      putStrLn "Dealing the turn..."
      let (turnCard : deck'') = deck gameState'''''
      let gameState'''''' = gameState''''' { board = turnCard : board gameState''''', deck = deck'' }

      -- Post-turn betting round
      gameState''''''' <- bettingRound gameState''''''

      -- Dealing the river
      putStrLn "Dealing the river..."
      let (riverCard : deck''') = deck gameState'''''''
      let gameState'''''''' = gameState'''''''' { board = riverCard : board gameState''''''', deck = deck''' }

      -- Post-river betting round
      gameState''''''''' <- bettingRound gameState'''''''''

      -- Determining the winner(s) and distributing the pot
      let updatedGameState = determineWinner gameState'''''''''
      let winners = filter (\p -> playerStatus p == Active && playerChips p > 0) (players updatedGameState)
      putStrLn $ "Winners: " ++ (unwords . map (show . playerId) $ winners) ++ " win the pot!"

      -- Handling the end of the game or moving on to the next hand
      putStrLn "Do you want to play another hand? (yes/no)"
      continue <- getLine
      if continue == "yes" || continue == "y"
        then gameLoop updatedGameState
        else putStrLn "Thank you for playing!"
