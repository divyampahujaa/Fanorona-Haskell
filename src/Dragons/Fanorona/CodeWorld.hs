{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Dragons.Fanorona.CodeWorld where

import           Fanorona
import           CodeWorld
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Dragons.Fanorona ()
import           Dragons.Game
import           Dragons.Game.UI.CodeWorld as UI

-- | UI-specific state.
data UIModel
  = Idle
  | SelectPiece
  | MovePiece Location Bool
  | CompleteMove Location Location
  deriving (Eq, Show)

-- | Convert complex codeworld events to simpler form
data SimpleEvent
  = Esc
  | Passing
  | ClickLocation Location
  | SetMoveType MoveType
  deriving (Eq, Show)

codeWorldUI :: GameConfig GameState Move -> IO (GameUI GameState Move)
codeWorldUI config = UI.codeWorldUI config $ CodeWorldUI
  { cwInitialModel = Idle
  , cwView = view
  , cwUpdate = update
  }

-- | Render the whole scene as a 'Picture'.
view :: UIMode Move -> GameState -> UIModel -> Picture
view mode state model = pictures
  [ drawModeText mode
  , drawModelText model
  , drawScore state
  , drawHighlights model state
  , drawLastMove state mode
  , drawBoard state
  ]

-- | Describe the 'UIMode'
drawModeText :: UIMode Move -> Picture
drawModeText mode = translated (-6) 7 . scaled 0.5 0.5 . lettering $ case mode of
  Started -> "Initialising"
  AwaitingMove p _ _ -> pieceName p <> " to move"
  AIThinking p name -> pieceName p <> " (" <> T.pack name <> ") is thinking"
  AIFailedToMove p name -> pieceName p <> " (" <> T.pack name <> ") failed to move"
  AIIllegalMove p name -> pieceName p <> " (" <> T.pack name <> ") made an illegal move"
  NetworkIllegalMove p -> pieceName p <> " (network) made an illegal move"
  Finished o _ -> "Game over. " <> case o of
    Winner p -> pieceName p <> " wins!"
    Draw -> "It's a draw!"

-- | Additional labels from the 'UIModel' to track exactly what
-- we're asking of the player as they build up a move.
drawModelText :: UIModel -> Picture
drawModelText model = translated (-6) 6 . scaled 0.5 0.5 $ case model of
  Idle-> blank
  SelectPiece -> lettering "Click a piece to move"
  MovePiece _ b -> lettering . T.pack $ "Click a point to move this piece into (ESC to cancel)" 
    ++ if b then " or press \"P\" to pass" else []
  CompleteMove _ _ -> lettering "Press A to capture by Approach, or W to capture by Withdrawal"

-- | Show current scores(number of pieces controlled by each player).
drawScore :: GameState -> Picture
drawScore state
  = translated 6 7 (scaled 0.5 0.5 (lettering p1))
  & translated 6 6 (scaled 0.5 0.5 (lettering p2))
  where
    p1 = pieceName Player1 <> ": " <> T.pack (show p1count)
    p2 = pieceName Player2 <> ": " <> T.pack (show p2count)
    (p1count, p2count) = countPieces state

-- | Draw highlights (which pieces are selectable).
drawHighlights :: UIModel -> GameState -> Picture
drawHighlights model state = case model of
  Idle -> blank
  SelectPiece -> centreGrid state $ foldMap highlight playerPieceLocs
  MovePiece loc _ -> centreGrid state . foldMap highlight $ destinationLocs loc
  CompleteMove loc1 loc2 -> centreGrid state $ foldMap highlight [loc1, loc2] 

  where
    highlight :: Location -> Picture
    highlight (Location x y) = translated (fromIntegral x) (fromIntegral (-y)) $
      colored grey (thickCircle 0.1 0.25)

    playerPieceLocs :: [Location]
    playerPieceLocs = nub . mapMaybe moveFrom $ legalMoves state

    destinationLocs :: Location -> [Location]
    destinationLocs loc = nub . mapMaybe fromHere $ legalMoves state
      where
        fromHere :: Move -> Maybe Location
        fromHere mv
          | moveFrom mv == Just loc = moveTo mv
          | otherwise = Nothing

-- | Draw the previous move, if there is one.
drawLastMove :: GameState -> UIMode Move -> Picture
drawLastMove state mode = case mode of
  AwaitingMove _ (Just (_, Move _ (Location fx fy) (Location tx ty))) _ ->
    centreGrid state $ pictures (map showCapture visited ++
      [ translated fx' (-fy') (coloured grey (solidCircle 0.1))
      , translated tx' (-ty') (colored grey (solidCircle 0.15))
      , colored grey (thickPolyline 0.1 [(fx', -fy'), (tx', -ty')])
      ])
    where
      
      visited = case captor state of
        None -> []
        Captor _ passed -> passed 

      showCapture (Location x y) = translated x' (-y') $ coloured red $ solidCircle 0.1
         where (x', y') = (fromIntegral x, fromIntegral y)
      
      (fx', fy') = (fromIntegral fx, fromIntegral fy)
      (tx', ty') = (fromIntegral tx, fromIntegral ty)

  _ -> blank

-- | Draw the board and all its pieces to the centre of the screen.
drawBoard :: GameState -> Picture
drawBoard state = centreGrid state $
  pictures (zipWith draw (concat (board state)) (allLocations state)) & grid state
  where
    draw :: Square -> Location -> Picture
    draw sp (Location x y) = translated rx (-ry) (drawSpace sp)

      where 

        rx = fromIntegral x
        ry = fromIntegral y
    
allLocations :: GameState -> [Location]
allLocations state = [Location x y | y <- [0..h-1], x <- [0..w-1]]
  where (w,h) = bounds state

-- | Draw a board square in a square 1.0 units each side.
drawSpace :: Square -> Picture
drawSpace sp = case sp of
  Empty -> solidCircle 0.1
  Piece p -> case p of
    Player1 -> coloured (light . light . light $ brown)
     (solidCircle 0.25) & circle 0.25
    Player2 -> coloured (dark brown)
     (solidCircle 0.25) & circle 0.25

                 
-- | Labels for pieces that match how we draw them.
pieceName :: Player -> Text
pieceName = player "Light" "Dark"

-- | The grid that makes up the board.
grid :: GameState -> Picture
grid state
  = pictures (map web (allLocations state))
  & coloured (dull yellow) $ solidPolygon [(-0.5,0.5),(w-0.5,0.5),(w-0.5,-h+0.5),(-0.5,-h+0.5)] 
  
  where
    web :: Location -> Picture
    web loc = pictures 
      (map (connect loc) (filter (half loc) (neighbourhood state loc))) 
    
    connect :: Location -> Location -> Picture
    connect (Location x y) (Location x' y') = polyline 
      [ (fromIntegral x, fromIntegral (-y)), 
        (fromIntegral x', fromIntegral (-y'))
      ]

    (w, h) = (\(x,y) -> (fromIntegral x, fromIntegral y)) $ bounds state

-- | Translate the grid into the centre of the screen.
centreGrid :: GameState -> Picture -> Picture
centreGrid state = scaled sf sf . translated ((-w'+1)/2) ((h'-1)/2)
  where
    sf = min (18/w') (10/h')
    (w, h) = bounds state
    (w', h') = (fromIntegral w, fromIntegral h)


update
  :: UIMode Move
  -> GameState
  -> UIModel
  -> Event
  -> (UIModel, Maybe (UIResponse Move))
update mode state model ev = case mode of
  Started -> idle
  Finished _ quit -> (Idle, Just quit)
  AIThinking _ _ -> idle
  AIFailedToMove _ _ -> idle
  AIIllegalMove _ _ -> idle
  NetworkIllegalMove _ -> idle
  AwaitingMove _ _ respond -> case model of
    Idle -> (SelectPiece, Nothing)
    SelectPiece -> case captor state of 
      Captor loc _ -> (MovePiece loc True, Nothing)
      _ -> withSimpleEvent $ \case
        ClickLocation loc -> case find ok (legalMoves state) of
          Nothing -> ignore
          Just _ -> (MovePiece loc False, Nothing)
          where ok mv = Just loc == moveFrom mv
        _ -> ignore
    MovePiece from mayPass -> withSimpleEvent $ \case
      ClickLocation to -> case find ok (legalMoves state) of 
        Nothing -> ignore
        Just _ -> (CompleteMove from to, Nothing)
        where ok mv = Just from == moveFrom mv && Just to == moveTo mv  
      Passing
        | mayPass -> (Idle, Just $ respond Pass)
        | otherwise -> ignore
      Esc -> idle
      _ -> ignore
    CompleteMove from to -> case buildMove from to of 
      Nothing -> withSimpleEvent $ \case
        Esc -> idle
        SetMoveType mt
          | Move mt from to `elem` legalMoves state ->
            (Idle, Just . respond $ Move mt from to)
          | otherwise -> idle
        _ -> ignore
      Just mv -> (Idle, Just $ respond mv)

  where

    buildMove :: Location -> Location -> Maybe Move
    buildMove from to = case filter (with from to) (captures state) of 
        [mv] -> Just mv
        []
          | to `elem` neighbourhood state from -> Just (Move Paika from to)
          | otherwise -> Nothing
        _ -> Nothing 

    with :: Location -> Location -> Move -> Bool
    with _ _ Pass = False
    with from to (Move _ from' to') = from == from' && to == to' 

    -- Parse CodeWorld event into SimpleEvent if possible.
    simpleEvent :: Maybe SimpleEvent
    simpleEvent = case ev of
      KeyPress "Esc" -> Just Esc
      KeyPress "A" -> Just (SetMoveType Approach)
      KeyPress "W" -> Just (SetMoveType Withdrawal)
      KeyPress "P" -> Just Passing
      PointerPress p -> ClickLocation <$> gridPoint state p
      _ -> Nothing

    -- If the current event parses to a SimpleEvent, feed it to the
    -- function. Otherwise do nothing.
    withSimpleEvent
      :: (SimpleEvent -> (UIModel, Maybe (UIResponse Move)))
      -> (UIModel, Maybe (UIResponse Move))
    withSimpleEvent f = maybe ignore f simpleEvent

    idle = (Idle, Nothing)
    ignore = (model, Nothing)

-- | Convert a 'Point' in screen space into a 'Location' on the game board
gridPoint :: GameState -> Point -> Maybe Location
gridPoint state (px, py) = find onPoint (allLocations state)
  where
    onPoint (Location x y) =  dx2 + dy2 < 0.5 * 0.5
      where
        x', y' :: Double
        (x', y') = ((px * sf + w') / 2
                   , ((-py) * sf + h') / 2
                   )
        (dx, dy) =  (fromIntegral x - x', fromIntegral y - y')
        (dx2, dy2) = (dx * dx, dy * dy)
        
        sf = max (w' / 8) (h' / 4)

        (w', h') = (fromIntegral w - 1, fromIntegral h - 1)
        (w, h) = bounds state
