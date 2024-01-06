{-|
This module contains the data types, rules, and logic for the
Fanorona game. 
-}
module Fanorona where

import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable
import           GHC.Generics    (Generic)
import           Data.Maybe

-- | The game state contains
--  1. What the current turn is.
--  2. Whether that move is part of a capturing sequence, and if so,
--     that sequence.
--  3. The size of the board
--  4. The current occupation of each piece on the board, and
--  5. A history of previous boards that could possibly be repeated.
data GameState = State Turn Captor (Int,Int) Board History
  deriving (Eq, Show)

-- | It is either a player's turn, or the game is over.
data Turn = Turn Player | GameOver Outcome deriving (Eq, Show)

data Player
  = Player1 -- ^ Light
  | Player2 -- ^ Dark
  deriving (Eq, Show)

data Outcome = Winner Player | Draw deriving (Eq, Show)

-- | If the current move is in a capturing sequence, then there must be
-- a single "captor", i.e. the piece that is making a string of captures.
-- We record the captor's current location, and the list of locations
-- the captor has passed.
data Captor = None | Captor Location [Location] deriving (Eq, Show)

-- | A location on the board is an X then a Y coordinate.
data Location = Location Int Int
    deriving (Eq, Generic, NFData, Show)

-- | A 'Board' always consists of 'gameHeight' rows, each of length
-- 'gameWidth'. The top-left corner is @'Location' 0 0@.
type Board = [[Square]]

-- | A square on the board.
data Square
  = Piece Player -- ^ Occupied by a player's piece.
  | Empty -- ^ Empty space.
  deriving (Eq, Show)

-- | The 'History' of a game state contains past boards
-- which may be repeated. It is cleared after each capturing move. 
type History = [Board]


turn :: GameState -> Turn
turn (State trn _ _ _ _) = trn

-- | Given a function to update a Turn, update a whole GameState
adjustTurn :: (Turn -> Turn) -> GameState -> GameState
adjustTurn f (State trn _ bnds brd hist) = State (f trn) None bnds brd hist

-- | Given a Turn, set Turn in a GameState to that value.
setTurn :: Turn -> GameState -> GameState
setTurn trn = adjustTurn (const trn)

bounds :: GameState -> (Int, Int)
bounds (State _ _ bnds _ _) = bnds

board :: GameState -> Board
board (State _ _ _ brd _) = brd

setBoard :: Board -> GameState -> GameState
setBoard brd (State trn cap bnds  _ hist) = State trn cap bnds brd hist

history :: GameState -> History
history (State _ _ _ _ hist) = hist

captor :: GameState -> Captor
captor (State _ cap _ _ _) = cap

-- | Add the current Board to the board History.
updateHistory :: GameState -> GameState
updateHistory (State trn cap bnds brd hist) = State trn cap bnds brd (brd:hist)

clearHistory :: GameState -> GameState
clearHistory (State trn cap bnds brd _) = State trn cap bnds brd []

-- | Given a change in Location, update the Captor to be in
-- its new Location, remembering its last location.
adjustCaptor :: Location -> Location -> GameState -> GameState
adjustCaptor from to (State trn cap bnds brd hist) = case cap of
 None           -> State trn (Captor to [from]) bnds brd hist
 Captor from' past -> State trn 
     (if from == from' then Captor to (from:past) else None)
     bnds brd hist

clearCaptor :: GameState -> GameState
clearCaptor (State trn _ bnds brd hist) = State trn None bnds brd hist

-- | Given positive integers n, m, give an initial state with dimensions 
-- with 2n+1 columns and 2m+1 rows
initialState :: (Int, Int) -> GameState
initialState (w,h) = State (Turn Player1) None (2*w+1,2*h+1) blocks []
    where
        blocks = darks ++ centre:lights
        darks = replicate h (replicate (2*w+1) (Piece Player2))
        lights = replicate h (replicate (2*w+1) (Piece Player1))
        centre = leftCentre ++ Empty:rightCentre
        (leftCentre, rightCentre) = (splitAt w . concat . replicate w) 
          [Piece Player2, Piece Player1] 
        
otherPlayer :: Player -> Player
otherPlayer p = case p of
  Player1 -> Player2
  Player2 -> Player1

-- | Apply integer operations between Locations.
operate :: (Int -> Int -> Int) -> Location -> Location -> Location
operate op (Location x y) (Location x' y') = Location (x `op` x') (y `op` y') 

-- | Is the 'Location' on our board?
onBoard :: GameState -> Location -> Bool
onBoard state (Location x y) = x >= 0 && x < w && y >= 0 && y < h
  where (w, h) = bounds state

-- | Given a GameState and a Location, 'get' the Square at that location
-- if the Location is valid for that GameState. 
get :: GameState -> Location -> Maybe Square
get state (Location x y)
  | onBoard state (Location x y) = Just (board state !! y !! x)
  | otherwise = Nothing

-- | Given a Location and a function that adjusts a square,
-- adjust a GameState to change the square at that location as
-- specified. 
adjustSquareAt :: Location -> (Square -> Square) -> GameState -> GameState
adjustSquareAt loc@(Location x y) adjust state = case get state loc of
  Nothing -> state
  Just square -> state'
    where
      state' = setBoard newBoard state

      newBoard = beforeRows ++ changedRow:afterRows
      changedRow = beforeSquares ++ adjust square:afterSquares

      (beforeSquares, _:afterSquares) = splitAt x changingRow
      (beforeRows, changingRow:afterRows) = splitAt y (board state)

setSquare :: Location -> Square -> GameState -> GameState
setSquare loc s = adjustSquareAt loc (const s)

clearSquare :: Location -> GameState -> GameState
clearSquare loc = setSquare loc Empty

-- | The Location in front of a piece in (or against) its direction
-- of movement is often important.
-- Given a two Locations, return the next Location in sequence
continue :: Location -> Location -> Location
continue from through = operate (+) through delta
    where delta = operate (-) through from

-- | filter neighbourhood of a given location with this predicate
-- to get the half of the neighbours that are either right of or
-- directly below the given point.
half :: Location -> Location -> Bool
half (Location x y) (Location x' y') = x < x' || (x == x' && y > y')

-- | Given a GameState and a Location, return all the Locations
-- adjacent to that Location in the given GameState
neighbourhood :: GameState -> Location -> [Location]
neighbourhood state loc = filter (onBoard state) (map (operate (+) loc) neighbours)
    where
        neighbours
                | oddParity loc = filter oddParity directions
                | otherwise = directions
        directions = [ Location (-1) 1   , Location 0 1   , Location 1 1
                     , Location (-1) 0                    , Location 1 0
                     , Location (-1) (-1), Location 0 (-1), Location 1 (-1)
                     ]
        oddParity (Location a b) = odd (a + b) 

-- | Given a type of Square and a GameState, return the Locations of each
-- of those squares in the given gameState.
locationsOf :: Square -> GameState -> [Location]
locationsOf square state = snd (foldl' go (Location 0 0, []) (concat (board state)))
    where
        go (location, matches) square' = (next location
            , if square == square' then location:matches else matches)
        
        next loc
          | onBoard state loc' = loc'
          | otherwise = operate (+) loc' (Location (-w) 1)
          where loc' = operate (+) (Location 1 0) loc

        (w, _) = bounds state

-- | Fanorona has two main move types.
-- A Paika is a single move from one location to another, it is only
-- available if no capturing moves are possible.
-- Otherwise, a player may capture by Approaching **OR** 
-- by Withdrawing from an opponent's piece, but not both.
-- If it is possible to capture multiple times in succession, 
-- a player may do so, but captures in a sequence after the first are not
-- compulsory.
data Move = Move MoveType Location Location
          | Pass
         deriving (Eq, Generic, NFData, Show)

data MoveType = Paika | Approach | Withdrawal
   deriving (Eq, Generic, NFData, Show)

moveFrom :: Move -> Maybe Location
moveFrom (Move _ from _) = Just from
moveFrom Pass = Nothing

moveTo :: Move -> Maybe Location
moveTo (Move _ _ to) = Just to
moveTo Pass = Nothing


-- | Only apply a move if it is valid.
applyMove :: Move -> GameState -> Maybe GameState
applyMove move state
  | validateMove state move = fmap (nextTurn move) (makeMove move state)
  | otherwise = Nothing

-- | Check whether a Move is legal in a given GameState.
validateMove :: GameState -> Move -> Bool
validateMove (State _ (Captor _ _) _ _ _) Pass = True
validateMove _ state move = move `elem` legalMoves state

-- | Given a Move and a GameState, make that move in the GameState.
makeMove :: Move -> GameState -> Maybe GameState
makeMove move state = case turn state of
    Turn player ->  case move of
        Pass -> Just state
        Move moveType from to -> Just . setCaptor . eat .
            setSquare to (Piece player) .
            clearSquare from $ state
          where
            eat = case moveType of
              Paika -> id
              Withdrawal -> clearAlong from (continue to from)
              Approach -> clearAlong to (continue from to)
            setCaptor = case moveType of
              Paika -> id
              _ -> adjustCaptor from to
    _ -> Nothing
  where 
    clearAlong from to = clearSquare to .
      if get state to == get state next then clearAlong to next else id
        where next = continue from to

-- | Set the next turn in the game state:
--
-- 1. If the current board exists in our history, or there
--    are too many boards in our history, it's a draw.
-- 2. If the other player can move, it is their turn.
-- 3. If the other player cannot, the game is over; the current player wins. 
nextTurn :: Move -> GameState -> GameState
nextTurn move state = adjustHistory (case history state of
  boards
    | board state `elem` boards || length boards > 20 ->
      setTurn (GameOver Draw) state
    | otherwise -> case (legalMoves', turn state) of
      ([], Turn player) -> setTurn (GameOver (Winner player)) state
      ([Pass], _) -> nextTurn Pass state
      _            -> state')  
  where
    legalMoves' = case captor state' of
      None -> legalMoves state'
      _ -> Pass : legalMoves state'

    state' = case (captor state, move) of 
      (None, _) -> adjustTurn next state
      (_, Pass) -> adjustTurn next state
      _ -> state
    
    adjustHistory = case move of
      Pass -> id
      Move Paika _ _ -> updateHistory
      _ -> clearHistory

    next trn = case trn of
      Turn p     -> Turn (otherPlayer p)
      GameOver _ -> trn


-- | Given a GameState, what moves are possible?
legalMoves :: GameState -> [Move]
legalMoves state = case turn state of
  GameOver _ -> []
  Turn player -> case (captures state, captor state) of
    ([], None) -> concatMap (\from -> 
        map (Move Paika from) 
        (filter ((Just Empty ==) . get state) (neighbourhood state from)))
        (locationsOf (Piece player) state)
    ([], _) -> []
    (captureMoves, _) -> captureMoves

-- | Given a GameState, what capturing moves are possible?
captures :: GameState -> [Move]
captures state = case turn state of 
  GameOver _ -> []
  Turn player -> approaches ++ withdrawals
    where
      approaches = concatMap (moves Approach) (locationsOf Empty state)
      withdrawals = concatMap (moves Withdrawal) (locationsOf (Piece player) state)

      moves :: MoveType -> Location -> [Move]
      moves mt = filter checkCaptor .
            mapMaybe (extractCapture mt) .
            trios

      trios :: Location -> [[Location]]
      trios loc =
        map (\loc' -> [continue loc' loc, loc, loc']) $ 
        (filter (half loc) . neighbourhood state) loc 

      checkCaptor :: Move -> Bool
      checkCaptor Pass = captor state /= None
      checkCaptor (Move Paika _ _) = captor state == None
      checkCaptor (Move _ from to) = case captor state of
            Captor current (left:past) -> to `notElem` (left:past)
                && from == current
                && continue left current /= to
            _ -> True
      extractCapture :: MoveType -> [Location] -> Maybe Move
      extractCapture Paika _ = Nothing 
      extractCapture mt locs = eaten *> liftM2 (Move mt) from to
        where 
          from = find ((Just (Piece player) ==) . get state) locs
          to = find ((Just Empty ==) . get state) locs
          eaten = find 
            ((Just (Piece (otherPlayer player)) ==) . get state) locs

countPieces :: GameState -> (Int, Int)
countPieces st = foldl' count (0, 0) (concat (board st))
  where
    count :: (Int, Int) -> Square -> (Int, Int)
    count (p1,p2) sq = case sq of
      Piece Player1 -> (p1 + 1, p2)
      Piece Player2 -> (p1, p2 + 1)
      _             -> (p1, p2)
