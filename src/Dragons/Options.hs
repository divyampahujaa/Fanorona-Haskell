{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Dragons.Options
  ( AITable
  , Options(..)
  , DisplayUI(..)
  , Course(..)
  , NetworkMode(..)
  , parseOptions
  ) where

import           Data.ByteString (ByteString)
import           Data.Foldable (asum)
import           Data.Functor ((<&>))
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Dragons.Game (DebugFlag(..), GenericAIFunc, MoveSource(..))
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import           Text.Read (readMaybe)

parseOptions :: AITable st mv -> IO (Options st mv)
parseOptions ais = do
  opts <- execParser . info (helper <*> config ais) $ mconcat
    [ fullDesc
    , progDesc "Fanorona"
    , header "Fanorona"
    ]
  validateOptions opts
  pure opts

  where
    validateOptions opts = case (optPlayer1 opts, optPlayer2 opts) of
      (Network _, Network _) ->
        hPutStrLn stderr "Cannot have two network players." *> exitFailure
      (Network _, _) -> checkNetworkMode opts
      (_, Network _) -> checkNetworkMode opts
      _ -> pure ()

    checkNetworkMode opts = case optNetworkMode opts of
      Just _ -> pure ()
      _ -> do
        hPutStrLn stderr "Need to provide either the --host\
                         \ or --connect if using network players."
        exitFailure

-- | Type for the list of registered AIs.
type AITable st mv = [(String, GenericAIFunc st mv)]

-- | Type representing all config information collected from the command line.
data Options st mv = Options
  { optUI :: DisplayUI 
  , optHeight :: Int 
  , optWidth :: Int
  , optTimeout :: Double 
  , optPlayer1 :: MoveSource st mv Proxy 
  , optPlayer2 :: MoveSource st mv Proxy
  , optDebugFlags :: [DebugFlag]
  , optNetworkMode :: Maybe NetworkMode
  }

data DisplayUI = Text | CodeWorld | Json deriving (Eq, Show)
data NetworkMode = Host Int | Connect ByteString Int deriving (Eq, Show)

config :: AITable st mv -> Parser (Options st mv)
config ais = Options
  <$> displayUI
  <*> height
  <*> width
  <*> timeout
  <*> player ais "p1"
  <*> player ais "p2"
  <*> debugFlags
  <*> networkMode

height :: Parser Int
height = option auto $ mconcat
  [ long "height"
  , metavar "LENGTH"
  , help "Height of the board, rounded up to nearest odd number"
  , value 5
  , showDefault
  ]

width :: Parser Int
width = option auto $ mconcat
  [ long "width"
  , metavar "LENGTH"
  , help "Width of the board, rounded up to nearest odd number"
  , value 9
  , showDefault
  ]

timeout :: Parser Double
timeout = option auto $ mconcat
  [ long "timeout"
  , metavar "DURATION"
  , help "How long to wait for AI moves, in decimal seconds"
  , value 4.0
  , showDefault
  ]

displayUI :: Parser DisplayUI
displayUI = option readDisplayUI $ mconcat
  [ long "ui"
  , metavar "TYPE"
  , help "Which UI to run. Valid options: text, codeworld"
  , value CodeWorld
  , showDefault
  ]
  where
    readDisplayUI = maybeReader $ \case
      "text" -> Just Text
      "codeworld" -> Just CodeWorld
      "json" -> Just Json
      _ -> Nothing

player
  :: forall st mv . AITable st mv
  -> String
  -> Parser (MoveSource st mv Proxy)
player ais argname = option readMoveSource $ mconcat
  [ long argname
  , metavar "PLAYER"
  , help "Who is playing as this player. Valid options: human, ai, ai:AINAME"
  ]
  where
    readMoveSource = maybeReader $ \case
      "human" -> Just Human
      "network" -> Just $ Network Proxy
      "ai" -> findAI "default"
      'a':'i':':':name -> findAI name
      _ -> Nothing

    findAI :: String -> Maybe (MoveSource st mv Proxy)
    findAI n = AI n <$> lookup n ais

debugFlags :: Parser [DebugFlag]
debugFlags = many $ asum [debugLookahead]
  where
    debugLookahead :: Parser DebugFlag
    debugLookahead = flag' DebugLookahead $ mconcat
      [ long "debug-lookahead"
      , help "Show how far the AI looks ahead, and what moves it considers."
      ]

networkMode :: Parser (Maybe NetworkMode)
networkMode = asum [ host, connect, pure Nothing ]
  where
    host = Just . Host <$> option auto (mconcat
      [ long "host"
      , metavar "PORT"
      , help "What port to listen on."
      ])

    connect = option readConnect $ mconcat
      [ long "connect"
      , metavar "HOST:PORT"
      , help "Connect to another running game."
      ]

    readConnect = maybeReader $ \s -> case break (== ':') s of
      (remoteHost, ':':portStr) -> readMaybe portStr <&> \port ->
        Just $ Connect (encodeUtf8 $ T.pack remoteHost) port
      _ -> Nothing
