{-# OPTIONS_GHC -Wno-deprecations #-}

{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}


module Dragons.Game.UI.CodeWorld
  ( -- * Overview
    -- $overview
    -- * Types
    CodeWorldUI(..)
  , UIMode(..)
  , UIResponse
    -- * Entry Point
  , codeWorldUI
  ) where

import           Prelude hiding (filter)

import qualified CodeWorld as CW
import           CodeWorld.Reflex
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.DeepSeq (NFData)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Functor ((<&>), void)
import           Data.Witherable ((<$?>), (<&?>), filter)
import           Dragons.Game
import           Dragons.Game.Network (recv, send)
import           Reflex
import           Safe (lastMay)


-- A CodeWorld UI running in a separate thread to the actual game

data CodeWorldUI st mv model = CodeWorldUI
  { cwInitialModel :: model
  , cwView :: UIMode mv -> st -> model -> Picture
  , cwUpdate
    :: UIMode mv -> st -> model -> CW.Event -> (model, Maybe (UIResponse mv))
  }

data UIMode mv
  = Started
  | AwaitingMove Player (Maybe (Player, mv)) (mv -> UIResponse mv)
  | AIThinking Player String
  | AIFailedToMove Player String
  | AIIllegalMove Player String
  | NetworkIllegalMove Player
  | Finished Outcome (UIResponse mv)

data UIResponse mv
  = Move mv
  | Shutdown
  deriving Eq

data CodeWorldTriggers st mv = CodeWorldTriggers
  { triggerMode :: UIMode mv -> IO ()
  , triggerState :: st -> IO ()
  }

-- | Construct a 'GameUI' from a 'CodeWorldUI'.
codeWorldUI
  :: forall st mv model
   . (FromJSON mv, NFData mv, Eq mv, Show mv, ToJSON mv)
  => GameConfig st mv
  -> CodeWorldUI st mv model
  -> IO (GameUI st mv)
codeWorldUI config cw = do
  mvTriggers :: MVar (CodeWorldTriggers st mv) <- liftIO newEmptyMVar
  mvInitialState :: MVar st <- liftIO newEmptyMVar
  mvMove :: MVar mv <- liftIO newEmptyMVar
  mvShutdown :: MVar () <- liftIO newEmptyMVar

  -- Fork off a thread to run CodeWorld.
  void . liftIO . forkIO $
    reflexOf $ mdo
      eCW <- getCodeWorldEvent

      (eMode :: Event _ (UIMode mv), fireMode) <- newTriggerEventWithOnComplete
      (eSt :: Event _ st, fireState) <- newTriggerEventWithOnComplete

      dModel :: Dynamic _ model <- holdDyn (cwInitialModel cw) eNewModel
      dMode :: Dynamic _ (UIMode mv) <- holdDyn Started eMode
      dSt :: Dynamic _ st <- holdDyn st eSt

      let
        eUpdate :: Event _ (model, [UIResponse mv])
        eUpdate =
          let
            applyUpdates
              :: UIMode mv
              -> st
              -> model
              -> [CW.Event]
              -> (model, [UIResponse mv])
            applyUpdates mode s =
              let
                go responses model events = case events of
                  [] -> (model, reverse responses)
                  (e:es) -> case cwUpdate cw mode s model e of
                    (model', Nothing) -> go responses model' es
                    (model', Just resp) -> go (resp:responses) model' es
              in
                go []
          in
            current (applyUpdates <$> dMode <*> dSt <*> dModel) <@> eCW

        eNewModel :: Event _ model
        eResponses :: Event _ [UIResponse mv]
        (eNewModel, eResponses) = splitE eUpdate

        eMove :: Event _ mv
        eMove =
          let
            findFirstMove :: [UIResponse mv] -> Maybe mv
            findFirstMove responses = case responses of
              [] -> Nothing
              (Move mv:_) -> Just mv
              (_:rs) -> findFirstMove rs
          in findFirstMove <$?> eResponses

        eShutdown :: Event _ ()
        eShutdown = eResponses <&?> \responses ->
          if any (== Shutdown) responses
          then Just ()
          else Nothing

      performEvent_ $ eMove <&> (liftIO . putMVar mvMove)
      performEvent_ $ eShutdown <&> (liftIO . putMVar mvShutdown)

      draw $ cwView cw <$> dMode <*> dSt <*> dModel
      liftIO . putMVar mvTriggers $ CodeWorldTriggers
        { triggerMode = makeSynchronous fireMode
        , triggerState = makeSynchronous fireState
        }

      st <- liftIO $ takeMVar mvInitialState
      pure ()

  CodeWorldTriggers
    { triggerMode = mode
    , triggerState = state
    } <- takeMVar mvTriggers

  let
    initial :: st -> IO ()
    initial st = putMVar mvInitialState st *> state st

    askMove :: Player -> st -> Maybe (Player, mv) -> IO mv
    askMove p st mLastMove = do
      state st
      mv <- case configMoveSource config p of
        Human -> do
          mode $ AwaitingMove p mLastMove Move
          takeMVar mvMove
        AI name aiFunc -> do
          mode $ AIThinking p name
          mvs <- forceListWithTimeout (configAITimeout config) (aiFunc st)

          when (DebugLookahead `elem` configDebugFlags config) $
            printLookaheadTrace p name mvs
          case lastMay mvs of
            Nothing -> do
              mode $ AIFailedToMove p name
              aiFailedToMove p name
            Just mv -> pure mv
        Network gs -> recv gs

      case configMoveSource config (otherPlayer p) of
        Network gs -> send gs mv
        _ -> pure ()

      pure mv

    final :: Outcome -> st -> IO ()
    final outcome st = do
      state st
      mode $ Finished outcome Shutdown
      takeMVar mvShutdown

    illegal :: Player -> mv -> IO ()
    illegal p _ = case configMoveSource config p of
      Human -> pure ()
      AI name _ -> do
        mode $ AIIllegalMove p name
        aiIllegalMove p name
      Network _ -> do
        mode $ NetworkIllegalMove p
        networkIllegalMove p

  pure $ GameUI
    { uiInitialUpdate = initial
    , uiAskMove = askMove
    , uiFinalUpdate = final
    , uiIllegalMove = illegal
    }

getCodeWorldEvent :: ReflexCodeWorld t m => m (Event t [CW.Event])
getCodeWorldEvent = do
  eKeyPress <- getKeyPress
  eKeyRelease <- getKeyRelease
  eTextEntry <- getTextEntry
  dPointerPosition <- getPointerPosition
  dPointerDown <- isPointerDown
  eTimePassing <- getTimePassing

  let
    bPointerPosition = current dPointerPosition
    ePointerDown = updated dPointerDown
    ePointerClick = bPointerPosition <@ filter id ePointerDown
    ePointerRelease = bPointerPosition <@ filter not ePointerDown

  pure $ mconcat
    [ pure . CW.KeyPress <$> eKeyPress
    , pure . CW.KeyRelease <$> eKeyRelease
    , pure . CW.PointerPress <$> ePointerClick
    , pure . CW.PointerRelease <$> ePointerRelease
    , pure . CW.PointerMovement <$> updated dPointerPosition
    , pure . CW.TextEntry <$> eTextEntry
    , pure . CW.TimePassing <$> eTimePassing
    ]

makeSynchronous :: (a -> IO () -> IO ()) -> a -> IO ()
makeSynchronous fire a = do
  mvDone <- newEmptyMVar
  fire a (putMVar mvDone ())
  takeMVar mvDone
