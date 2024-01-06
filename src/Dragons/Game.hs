{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |mAbstract framework for a two-player, turn-based, perfect information game.
module Dragons.Game
  ( Player(..)
  , player
  , otherPlayer
  , Turn(..)
  , Outcome(..)
  , MoveSource(..)
  , DebugFlag(..)
  , GameRules(..)
  , GameConfig(..)
  , GameUI(..)
  , runGame
  , GenericAIFunc
  , Seconds
  , aiFailedToMove
  , aiIllegalMove
  , networkIllegalMove
  , printLookaheadTrace
  , forceListWithTimeout
  ) where

import Fanorona (Player(..), otherPlayer, Outcome(..), Turn(..))
import Control.DeepSeq (NFData(..), force)
import Control.Monad.State (StateT, runStateT, get, gets, put)
import Control.Monad.Trans (lift)
import Data.Aeson
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Dragons.Game.Network (GameSocket)
import System.Timeout (timeout)

-- | The catamorphism (fold) for 'Player'.
player :: a -> a -> Player -> a
player p1 p2 p = case p of
  Player1 -> p1
  Player2 -> p2

data MoveSource st mv f
  = Human 
  | AI String (GenericAIFunc st mv) 
  | Network (f GameSocket)


data DebugFlag = DebugLookahead deriving (Eq, Show)


data GameRules st mv = GameRules
  { gameInitialState :: st
  , gameGetTurn :: st -> Turn 
  , gameApplyMove :: mv -> st -> Maybe st
  }

-- | Configuration data that's independent of UIs.
data GameConfig st mv = GameConfig
  { configMoveSource :: Player -> MoveSource st mv IORef
    -- ^ Where moves come from.
  , configAITimeout :: Seconds -- ^ How long to wait for AI players.
  , configDebugFlags :: [DebugFlag] -- ^ Debugging flags.
  }

-- | Collection of callback functions for a game's UI.
data GameUI st mv = GameUI
  { uiInitialUpdate :: st -> IO () -- ^ Called when the game starts.
  , uiAskMove :: Player -> st -> Maybe (Player, mv) -> IO mv
    -- ^ Ask for a player's next move. @Maybe ('Player', mv)@ is the
    -- previous move.
  , uiFinalUpdate :: Outcome -> st -> IO () -- ^ Called when the game is over.
  , uiIllegalMove :: Player -> mv -> IO ()
    -- ^ Called when the submitted move was illegal. Will be followed
    -- by either 'uiAskP1Move' or 'uiAskP2Move'.
  }

-- | Actually run a game, by providing the rules and a UI for the same game
runGame :: forall st mv . GameRules st mv -> GameUI st mv -> IO ()
runGame rules ui = do
  let initialSt = gameInitialState rules
  uiInitialUpdate ui initialSt
  (outcome, (finalSt, _)) <- runStateT gameLoop (initialSt, Nothing)
  uiFinalUpdate ui outcome finalSt

  where
    gameLoop :: StateT (st, Maybe (Player, mv)) IO Outcome
    gameLoop = do
      t <- currentTurn
      case t of
        Turn p -> nextMove p *> gameLoop
        GameOver o -> pure o

    currentTurn :: StateT (st, Maybe (Player, mv)) IO Turn
    currentTurn = gets (gameGetTurn rules . fst)

    nextMove :: Player -> StateT (st, Maybe (Player, mv)) IO ()
    nextMove p = do
      (st, mLastMove) <- get
      mv <- askMove st mLastMove
      case gameApplyMove rules mv st of
        Nothing -> lift (uiIllegalMove ui p mv) *> nextMove p
        Just st' -> put (st', Just (p, mv))

      where
        askMove s mLast = lift $ uiAskMove ui p s mLast

-- | An 'GenericAIFunc' is how bots decide what move to make. 
type GenericAIFunc st mv = st -> [mv]

-- | Type of timeouts.
type Seconds = Double

-- | Signal that an AI failed to move.
aiFailedToMove :: Player -> String -> a
aiFailedToMove p name =
  error $ "AI (" <> show p <> ", " <> name <> ") failed to move."

-- | Signal that an AI has made an illegal move.
aiIllegalMove :: Player -> String -> a
aiIllegalMove p name =
  error $ "AI (" <> show p <> ", " <> name <> ") made an illegal move."

-- | Signal that an illegal move came in over the network.
networkIllegalMove :: Player -> a
networkIllegalMove p = error $ show p <> " (network) made an illegal move."

-- | Print out a list of moves considered by a lookahead function.
printLookaheadTrace :: Show mv => Player -> String -> [mv] -> IO ()
printLookaheadTrace p name mvs = putStrLn $ concat
  [ "AI ("
  , show p
  , ", "
  , show name
  , ") looked ahead up to "
  , show (length mvs)
  , " moves.\n"
  , "Moves considered: " ++ show mvs
  ]

forceListWithTimeout :: NFData a => Seconds -> [a] -> IO [a]
forceListWithTimeout time lazyList = do
  results <- newIORef []

  let
    walk list = case list of
      [] -> pure ()
      x:xs -> force x `seq` do
        modifyIORef results (x:)
        walk xs

    timeLimit = round (time * 1000000)
  _ <- timeout timeLimit (walk lazyList)

  reverse <$> readIORef results



instance ToJSON Player where
  toJSON p = case p of
    Player1 -> "player1"
    Player2 -> "player2"

instance ToJSON Outcome where
  toJSON o = case o of
    Winner p -> object
      [ "$type" .= String "winner"
      , "player" .= p
      ]
    Draw -> object
      [ "$type" .= String "draw"
      ]

instance ToJSON Turn where
  toJSON t = case t of
    Turn p -> object
      [ "$type" .= String "turn"
      , "player" .= p
      ]
    GameOver o -> object
      [ "$type" .= String "gameOver"
      , "outcome" .= o
      ]
