module AI where

import           Fanorona
import Data.Maybe
import Data.List

-- | Type of AI functions
data AIFunc
  = NoLookahead (GameState -> Move)
  | WithLookahead (GameState -> Int -> Move)
ais :: [(String, AIFunc)]
ais = [ ("first", NoLookahead (firstLegalMove))
      , ("second", NoLookahead (simpleAI))
      , ("third", NoLookahead (simpleAI2))
      , ("fourth", NoLookahead (simpleAI3))
      , ("fifth", NoLookahead (simpleAI3'))
      , ("default", WithLookahead (lookAheadAI))
      , ("seventh", WithLookahead (lookAheadAI2))
      ]

-- | A very simple AI, which passes whenever it can, and if not,
-- picks the first move returned by the 'legalMoves' function.
firstLegalMove :: Course -> GameState -> Move
firstLegalMove course state = case applyMove course Pass state of
  Nothing -> head (legalMoves state)
  _ -> Pass


data Gtree a = Gtree a [Gtree a] 
  deriving (Show,Eq)

-- Given number of pieces of p1 and p2 give count of piece of only one player
singlePieceCounter :: Int -> [(Int,Int)] -> [Int]
singlePieceCounter player count = case player of 
  1 -> case count of
    []       -> []
    (x,_):xs -> x: singlePieceCounter 1 xs 
  _ -> case count of
    []      -> []
    (_,x):xs -> x: singlePieceCounter 2 xs 
  

-- simple AI which tries to maximise its pieces on board (Greedy) 
simpleAI :: Course -> GameState -> Move
simpleAI course cGS = case (mbGsToGs course (legalMoves cGS) cGS)of 
  []   -> Pass
  list -> case (turn cGS, map (countPieces) list) of
    (Turn Player1, count) -> moves !! fromJust (elemIndex (maximum (spc 1 count)) 
                                                          (spc 1 count)
                                               )

    (Turn Player2, count) -> moves !! fromJust (elemIndex (maximum (spc 2 count)) 
                                                          (spc 2 count)
                                              )
    (_,_)                 -> Pass
  where
    moves = legalMoves cGS
    spc   = singlePieceCounter

-- similar to simpleAI1 which tries to maximise its pieces when it is light and minimise light's pieces when its dark
simpleAI2 :: Course -> GameState -> Move
simpleAI2 course cGS = case (mbGsToGs course (legalMoves cGS) cGS)of 
  []   -> Pass
  list -> case (turn cGS, map (countPieces) list) of
    (Turn Player1, count) -> moves !! fromJust (elemIndex (maximum (spc 1 count)) 
                                                          (spc 1 count)
                                               )

    (Turn Player2, count) -> moves !! fromJust (elemIndex (minimum (spc 1 count)) 
                                                          (spc 1 count)
                                               )
    (_,_) -> Pass
  where
    moves = legalMoves cGS
    spc   = singlePieceCounter

--(Greedy) an AI which does a move that ensures that the difference bw its pieces and opponent's is maximum
simpleAI3 :: Course -> GameState -> Move
simpleAI3 course cGS = case (mbGsToGs course (legalMoves cGS) cGS) of 
  [] -> Pass
  list -> moves !! fromJust (elemIndex (maximum (diff (count list))) 
                                       (diff (count list))
                            )
  where
    diff  = pieceDiff (turn cGS)
    moves = legalMoves cGS
    count = map (countPieces)
    




--Greedy which considers multiple moves. This looks at the very end of the gametree and analyse it's moves while in a capturing sequence
simpleAI3' :: Course -> GameState -> Move
simpleAI3' course cGS = case (captor cGS) of 
  None -> case (mbGsToGs course (legalMoves cGS) cGS) of 
    [] -> Pass
    list -> simpleAI3 course cGS

  -- AI in a capturing sequence
  _  -> case stt of
    []    -> Pass
    _     -> bestmove
    where
      --gives a list of tree for every gamestate
      stateTree = map (\x -> aheadMoves course x 
                             (mbGsToGs course (legalMoves x) x)
                      ) 
                      (stt)
      bestmove = moves !! fromJust (elemIndex (maxOfGSlist cGS 
                                                      (maxOfGsTree stateTree)
                                              ) 
                                              (maxOfGsTree stateTree)
                                  )
      stt = (mbGsToGs course ((legalMoves cGS)) cGS)

      moves = legalMoves cGS 


-- gives max of list of maximum gamestate of each sub game trees
maxOfGSlist:: GameState -> [GameState] -> GameState
maxOfGSlist cGs lnodes =  lnodes !! fromJust (elemIndex (maximum (diffList)) 
                                                         diffList
                                             )
  where
    diffList = pieceDiff (turn cGs) (map countPieces lnodes)

-- generates the game tree to a certain depth
gameTreeGen :: Course -> Depth -> GameState -> Gtree GameState
gameTreeGen c d cGS = case d of
  0 -> Gtree cGS []
  _ -> Gtree cGS (map (gameTreeGen c (d-1)) states)
  where
    states =  (mbGsToGs c (legalMoves cGS) cGS)

-- give max gamestate of each sub game tree
maxOfGsTree :: [Gtree GameState] -> [GameState]
maxOfGsTree stateTree = case stateTree of 
  []                -> []
  Gtree gs nodes:xs -> (maxLeafNode gs (Gtree gs nodes)): maxOfGsTree xs

-- heuristic
pieceDiff:: Turn -> [(Int,Int)] -> [Int]
pieceDiff trn count = case trn of
  Turn Player1 -> case count of  
    []        -> []
    (a,b):xs  -> (a-b): pieceDiff trn xs
  Turn Player2 -> case count of  
    []        -> []
    (a,b):xs  -> (b-a): pieceDiff trn xs
  _ -> [0]


-- Identifies leaf gamestate with best max pieces
maxLeafNode:: GameState -> Gtree GameState -> GameState
maxLeafNode cGS tree = (lve) !! fromJust(elemIndex (maximum (diff (count lve)))
                                                   (diff (count lve))
                                        )
  where
    diff  = pieceDiff (turn cGS)
    lve = leaves tree
    count = map (countPieces)

    -- gives list of leaf nodes of a Gtree
    leaves :: Gtree GameState -> [GameState]
    leaves treee = case treee of 
      Gtree gs []   -> [gs]
      Gtree _ list -> concatMap leaves list



-- Given a gamestate tree, 
-- evaluate each leaf gamestate further while capturing moves are possible and return a tree
aheadMoves:: Course -> GameState -> [GameState] -> Gtree GameState 
aheadMoves course cGs states = case (captor cGs) of
  None -> Gtree cGs []
  _    -> case states of
    [] -> Gtree cGs []
    _  -> Gtree cGs (map (\x -> aheadMoves course x 
                                 (mbGsToGs course (legalMoves x) x)
                           ) 
                           states
                      )

-- based on the current gameState, give next possible gamestates for each legal move
-- maybe Gamestates to gamestates
mbGsToGs :: Course -> [Move] -> GameState -> [GameState]
mbGsToGs course moves cGS = case moves of 
  []    -> []
  x:xs -> case (applyMove course x cGS) of
    Nothing    -> mbGsToGs course xs cGS
    (Just stt) -> stt : (mbGsToGs course xs cGS)



-- if there is a capturing sequence player may terminate its turn by passing
legalMoves' :: GameState -> [Move]
legalMoves' gs = case captor gs of
  None          -> legalMoves gs
  Captor _ []   -> legalMoves gs
  Captor _ _    -> Pass : legalMoves gs

type Depth =Int

-- minimax will tell best of what each gamestate could lead to
miniMax :: Course -> GameState -> Depth  -> Int
miniMax course gs d  = case d of 
  0 -> case turn gs of
    Turn Player1 -> case pieceCountlist of
      [] -> max (-100000) (1000000)
      _  -> max (-100000) (maximum pieceCountlist)
    Turn Player2 -> case pieceCountlist of
      [] -> min (100000) (-100000000)
      _  -> min (100000) (minimum pieceCountlist) 
    GameOver (Winner Player1) -> 1000000
    GameOver (Winner Player2) -> -1000000
    GameOver Draw             -> 0
  _ -> case turn gs of 
    GameOver (Winner Player1) -> 1000000
    GameOver (Winner Player2) -> -1000000
    GameOver Draw  -> 0
    Turn Player1 -> (maximum (map (\x -> miniMax course x (d-1)) states))
    Turn Player2 -> (minimum (map (\x -> miniMax course x (d-1)) states))
  where
    states = (mbGsToGs course (legalMoves gs) gs)
    pieceCountlist = map (pieceDiff' (Turn Player1)) (map (countPieces) states)


-- works on minimax algorithm
lookAheadAI :: Course -> GameState -> Int -> Move
lookAheadAI c cGS i = case (legalMoves cGS) of 
  []    -> Pass
  moves -> case (turn cGS) of
    Turn Player1 -> moves !! fromJust (elemIndex (maximum mmResult) mmResult)
    Turn Player2 -> moves !! fromJust (elemIndex (minimum mmResult) mmResult)
    _            -> Pass

    where
      -- which gamestate leads to the most favourable situation?
      mmResult = map (\x -> miniMax c x i ) states
      -- next possible states 
      states = (mbGsToGs c (legalMoves cGS) cGS)


-- modified piece difference heuristic to match type signatures in some cases
pieceDiff' :: Turn -> (Int,Int) -> Int
pieceDiff' trn (a,b) = case trn of 
  Turn Player1 -> (a-b)
  Turn Player2 -> (b-a)  
  _            -> 0     


-- alpha beta optimisation to minimax algorithm
type Alpha = Int
type Beta  = Int


-- works on alpha beta optimised minimax algorithm
lookAheadAI2 :: Course -> GameState -> Int -> Move
lookAheadAI2 c cGS i = case (legalMoves cGS) of 
  []    -> Pass
  moves -> case (turn cGS) of
    Turn Player1 -> moves !! fromJust (elemIndex (maximum mmResult) mmResult)
    Turn Player2 -> moves !! fromJust (elemIndex (maximum mmResult) mmResult)

    where
      -- which gamestate leads to the most favourable situation?
      mmResult = map (\x -> alphaBeta (-10000000) (10000000) (gameTreeGen c i x)) states
      -- next possible states 
      states = (mbGsToGs c (legalMoves cGS) cGS)


alphaBeta:: Alpha -> Beta -> Gtree GameState -> Int
alphaBeta a b tree = case tree of
  Gtree x [] -> case turn x of
    Turn Player1 -> max a (pieceDiff' (Turn Player1) (countPieces x))
    Turn Player2 -> max b (pieceDiff' (Turn Player2) (countPieces x))
    GameOver (Winner Player1) -> 1000000
    GameOver (Winner Player2) -> -1000000
    GameOver Draw             -> 0
  (Gtree x ts) ->helper a b ts

helper:: Alpha -> Beta -> [Gtree GameState] -> Int
helper a b tree = case tree of
  []     -> a
  (t:ts) -> case t of
    Gtree x _ -> case turn x of
      Turn Player2
        | alpha' == b || b < alpha' -> alpha'
        | otherwise                 -> helper alpha' b ts
        where 
          alpha' = (alphaBeta (b) (a) t)
      Turn Player1 
        | alpha' == b || b < alpha' -> alpha'
        | otherwise                 -> helper alpha' b ts
        where
          alpha' = (alphaBeta (a) (b) t)
      GameOver (Winner Player1) -> 1000000
      GameOver (Winner Player2) -> -1000000
      GameOver Draw             -> 0