module Dragons.Fanorona.Text where

import Fanorona
import Data.Char
import Data.Maybe
import Data.List
import Dragons.Fanorona ()
import Dragons.Game
import Dragons.Game.UI.Text as UI

textUI :: GameConfig GameState Move -> GameUI GameState Move
textUI config = UI.textUI config $ TextUI
  { textRenderState = renderState
  , textReadMove = readMove
  }

renderState :: GameState -> String
renderState state = unlines $ catMaybes
  [ renderTurn (turn state)
  , Just $ show $ captor state
  , Just $ renderBoard state
  ]

renderTurn :: Turn -> Maybe String
renderTurn t = case t of
  Turn Player1 -> Just "Player 1 (O) to move"
  Turn Player2 -> Just "Player 2 (X) to move"
  GameOver _ -> Nothing

renderBoard :: GameState -> String
renderBoard state = unlines $ intersperse ' ' heading : interleave rows mids
  where
    rows =  zipWith (:) ['0'..'9'] (map (((' ':) . intersperse '-') . map renderSquare) brd)

    brd = board state 
    (w, h) = bounds state

    heading = ' ' : take w ['A'..'Z']

    evens = take (2*w-1) $ cycle ['|','\\','|','/']
    odds = take (2*w-1) $ cycle ['|','/','|','\\']

    mids = map ((' ':) . (' ':)) $ take (h-1) $ intersperse odds (repeat evens)
    
    interleave :: [a] -> [a] -> [a]
    interleave (a:as) (b:bs) = a:b:interleave as bs
    interleave [] bs = bs
    interleave as [] = as  


    renderSquare :: Square -> Char
    renderSquare sq = case sq of
      Empty -> ' '
      Piece Player1 -> 'o'
      Piece Player2 -> 'x'
     
-- | Ask for a move, check that it's sensible, and if it isn't, ask again.
readMove :: GameState -> Maybe (Player, Move) -> IO Move
readMove state _ = loop
  where
    loop = do
      putStrLn $ "Enter a move. Examples: " ++ intercalate ", " (map renderMove (legalMoves state)) ++ "or enter P to pass if possible."
      line <- getLine
      case parseMove state line of
        Nothing -> errLoop "Malformed move, try again."
        Just mv
          | mv `elem` legalMoves state -> pure mv
          | (moveFrom mv >>= get state) /= Just (Piece p) ->
              errLoop "You don't own a piece at the move's origin."
          | isNothing (moveTo mv >>= get state) ->
              errLoop "Destination square non-existent."
          | (moveTo mv >>= get state) /= Just Empty ->
              errLoop "Destination square non-empty."
          | otherwise -> case captor state of 
            Captor curr (prev:more)
              | moveTo mv `elem` map Just (prev:more) -> 
                errLoop "You have already visited this square on this move sequence."
              | moveTo mv == Just (continue curr prev) -> 
                 errLoop "A capturing piece cannot continue twice in the same direction."
            _ -> case mv of
              Pass -> errLoop "You cannot pass before making a capture"
              Move Paika _ _ -> errLoop "You must capture if possible."
              _ ->  errLoop "Not a valid move."

    errLoop s = putStrLn s *> loop

    Turn p = turn state

-- | Parse a 'String' that should describe a 'Move'
parseMove :: GameState -> String -> Maybe Move
parseMove state s = case map toUpper s of
  [ff, fr, '-', tf, tr, '-', mt] ->
    case (fromFile ff, fromRank fr, fromFile tf, fromRank tr, moveType) of
      (Just fx, Just fy, Just tx, Just ty, Just t) ->
        Just (Move t (Location fx fy) (Location tx ty))
      _ -> Nothing

    where
      fromFile r
        | x >= 0 && x < w = Just x
        | otherwise = Nothing
        where
          x = ord r - ord 'A'

      fromRank f
        | y >= 0 && y < h = Just y
        | otherwise = Nothing
        where
          y = ord f - ord '0'

      moveType = case mt of
        'W' -> Just Withdrawal
        'P' -> Just Paika
        'A' -> Just Approach
        _ -> Nothing 

      (w,h) = bounds state
  "P" -> Just Pass
  _ -> Nothing

renderMove :: Move -> String
renderMove Pass = "P"
renderMove (Move how (Location fx fy) (Location tx ty))
  = fs ++ "-" ++ ts ++ "-" ++ [(head . show) how]
  where
    fs = [ff, fr]
    ts = [tf, tr]

    ff = toFile fx
    fr = toRank fy
    tf = toFile tx
    tr = toRank ty

    toRank n = ['0'..'9'] !! n
    toFile n = ['A'..'Z'] !! n
