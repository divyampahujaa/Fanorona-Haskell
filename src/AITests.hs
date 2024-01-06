module AITests where

import           AI
import           Fanorona
import           Testing

aiTests :: Test
aiTests = TestGroup "AI"
  [ helperFunctions
  , heuristicTests 
  , withoutLookAheadAITests
  , miniMaxTest
  , lookAheadAiTest
  ]

-- TEST BOARDS


myTestBoard :: Board
myTestBoard = toBoard
  [
-- ABCDEFGHI
  "xxxxxxxxx", -- 0
  "xxxxxxxxx", -- 1
  "xoxo xoxo", -- 2
  "ooooooooo", -- 3
  "ooooooooo"  -- 4
  ]

myTestBoard2 :: Board
myTestBoard2= toBoard
  [
-- ABCDEFGHI
  "xx xxxxxx", -- 0
  "xxx xxxxx", -- 1
  "xoxooxoxo", -- 2
  "ooooo ooo", -- 3
  "ooooooooo"  -- 4
  ]

toBoard :: [String] -> Board
toBoard = (map . map) parsePiece
  where
   parsePiece c = case c of
    ' ' -> Empty
    'x' -> Piece Player2
    'o' -> Piece Player1
    _   -> error ("parsePiece: " ++ show c ++ "is not a piece.")

-- test for heuristics used 
heuristicTests :: Test
heuristicTests = TestGroup "Heuristcs"  [ hTest1, hTest2, hTest3, hTest4, hTest5, hTest6 ]
  where
    hTest1::Test
    hTest1 = Test "single Piece Counter" (assertEqual (singlePieceCounter 1 [(7,8), (8,9), (3,4)]) 
                                               ([7,8,3]))
    
    hTest2::Test
    hTest2 = Test "single Piece Counter" (assertEqual (singlePieceCounter 2 [(7,8), (8,9), (3,4)]) 
                                               ([8,9,4]))
    
    hTest3::Test
    hTest3 = Test "piece Diff" (assertEqual (pieceDiff (Turn Player1) [(7,8), (8,9), (3,4)]) 
                                      ([-1,-1,-1])) 
    
    hTest4::Test
    hTest4 = Test "piece Diff" (assertEqual (pieceDiff (Turn Player2) [(7,8), (8,9), (3,4)]) 
                                      ([1,1,1]))
    
    hTest5::Test
    hTest5 = Test "piece Diff'" (assertEqual (pieceDiff' (Turn Player2) (9,8)) (-1))
    
    hTest6::Test
    hTest6 = Test "piece Diff'"  (assertEqual (pieceDiff' (Turn Player1) (9,8)) (1))


-- tests for AIs that don't look ahead
withoutLookAheadAITests :: Test
withoutLookAheadAITests = TestGroup "simpleAIs" [test1, test2, test3, test4, test5, test6]
  where 
    test1::Test
    test1 = Test "simple AI" ( assertEqual (simpleAI  (State (Turn Player1) None  (9, 5) myTestBoard []))
                                     (Move Approach (Location 5 3) (Location 4 2)))
    
    test2::Test
    test2 = Test "simple AI2" (assertEqual (simpleAI2  (State (Turn Player2) None  (9, 5) myTestBoard [])) 
                                     (Move Approach (Location 3 1) (Location 4 2)))

    test3::Test
    test3 = Test "simple AI3" (assertEqual (simpleAI3  (State (Turn Player1) None  (9, 5) myTestBoard [])) 
                                     (Move Approach (Location 5 3) (Location 4 2)))
    
    test4::Test
    test4 = Test "simple AI3'" (assertEqual (simpleAI3'  (State (Turn Player1) None  (9, 5) myTestBoard2 [])) 
                                      (Move Approach (Location 5 4) (Location 5 3)))
    
    test5::Test
    test5 = Test "simple AI2" (assertEqual (simpleAI2  (State (Turn Player1) None  (9, 5) myTestBoard2 [])) 
                                     (Move Approach (Location 5 4) (Location 5 3)))
    
    test6::Test
    test6 = Test "simple AI" (assertEqual (simpleAI  (State (Turn Player2) None  (9, 5) myTestBoard2 []) ) 
                                    (Move Approach (Location 5 2) (Location 5 3)))



helperFunctions :: Test
helperFunctions = TestGroup "helper functions" [testA, testB, testC, testD]
  where 
    testA::Test
    testA = Test "Maybe Gamestates To Gamestates" 
            (assertEqual (mbGsToGs  [Move Approach (Location 5 4) (Location 5 3),Move Approach (Location 3 2) (Location 3 1),Move Withdrawal (Location 6 2) (Location 5 3)]  (State (Turn Player1) None  (9, 5) myTestBoard2 []) )
                         
                         ([State (Turn Player2) None (9,5) [[Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2],
                                                            [Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2],
                                                            [Piece Player2,Piece Player1,Piece Player2,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player2,Piece Player1],
                                                            [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1],
                                                            [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player1,Piece Player1]] []
                                                    
                          ,State (Turn Player2) None (9,5) [[Piece Player2,Piece Player2,Empty,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2],
                                                            [Piece Player2,Piece Player2,Piece Player2,Piece Player1,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2],
                                                            [Piece Player2,Piece Player1,Piece Player2,Empty,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1],
                                                            [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player1,Piece Player1],
                                                            [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []
                                                    
                          ,State (Turn Player2) None (9,5) [[Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Empty],
                                                            [Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2],
                                                            [Piece Player2,Piece Player1,Piece Player2,Piece Player1,Piece Player1,Piece Player2,Empty,Piece Player2,Piece Player1],
                                                            [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1],
                                                            [Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []]
                        )
            )

    
    testB::Test
    testB = Test "Game tree" 
            (assertEqual (gameTreeGen  1 (State (Turn Player1) None  (9, 5) myTestBoard []))
                         (Gtree (State (Turn Player1) None (9,5) [[Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                 ,[Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                 ,[Piece Player2,Piece Player1,Piece Player2,Piece Player1,Empty,Piece Player2,Piece Player1,Piece Player2,Piece Player1]
                                                                 ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]
                                                                 ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []) 
                                
                                [Gtree (State (Turn Player2) None (9,5) [[Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                        ,[Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                        ,[Piece Player2,Piece Player1,Piece Player2,Piece Player1,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1]
                                                                        ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player1,Piece Player1]
                                                                        ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []) []
                                
                                ,Gtree (State (Turn Player2) None (9,5) [[Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                        ,[Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                        ,[Piece Player2,Piece Player1,Piece Player2,Empty,Piece Player1,Empty,Piece Player1,Piece Player2,Piece Player1]
                                                                        ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]
                                                                        ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []) []
                                
                                ,Gtree (State (Turn Player2) None (9,5) [[Piece Player2,Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                        ,[Piece Player2,Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                        ,[Piece Player2,Piece Player1,Piece Player2,Piece Player1,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1]
                                                                        ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player1,Piece Player1,Piece Player1]
                                                                        ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []) []
                                
                                ,Gtree (State (Turn Player2) None (9,5) [[Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2]
                                                                        ,[Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2]
                                                                        ,[Piece Player2,Piece Player1,Piece Player2,Piece Player1,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1]
                                                                        ,[Piece Player1,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]
                                                                        ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []) []
                                
                                ,Gtree (State (Turn Player2) None (9,5) [[Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                        ,[Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                        ,[Piece Player2,Piece Player1,Empty,Empty,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1]
                                                                        ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]
                                                                        ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []) []]
                         )
           )

    testC::Test
    testC = Test "aheadMoves" (assertEqual (aheadMoves  (State (Turn Player1) None  (9, 5) myTestBoard2 [])
                                                                (mbGsToGs  [Move Approach (Location 5 4) (Location 5 3),Move Approach (Location 3 2) (Location 3 1),Move Withdrawal (Location 6 2) (Location 5 3)]  
                                                                                   (State (Turn Player1) None  (9, 5) myTestBoard2 []) 
                                                                )
                                           )
                                           (Gtree (State (Turn Player1) None (9,5) [[Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                                   ,[Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                                   ,[Piece Player2,Piece Player1,Piece Player2,Piece Player1,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1]
                                                                                   ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player1,Piece Player1]
                                                                                   ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []) []
                                          )
                            
                             )

    testD::Test
    testD = Test "maxLeafNode of game tree" (assertEqual (maxLeafNode (State (Turn Player1) None  (9, 5) myTestBoard2 []) (Gtree (State (Turn Player1) None (9,5) [[Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                                                                                                                  ,[Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                                                                                                                  ,[Piece Player2,Piece Player1,Piece Player2,Piece Player1,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1]
                                                                                                                                                                  ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player1,Piece Player1]
                                                                                                                                                                  ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []) []
                                                                                                                          )
                                                        )

                                                        (State (Turn Player1) None (9,5) [[Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                                         ,[Piece Player2,Piece Player2,Piece Player2,Empty,Piece Player2,Piece Player2,Piece Player2,Piece Player2,Piece Player2]
                                                                                         ,[Piece Player2,Piece Player1,Piece Player2,Piece Player1,Piece Player1,Piece Player2,Piece Player1,Piece Player2,Piece Player1]
                                                                                         ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Empty,Piece Player1,Piece Player1,Piece Player1]
                                                                                         ,[Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1,Piece Player1]] []
                                                        )
                                            )

miniMaxTest :: Test
miniMaxTest = Test "mini max" (assertEqual (miniMax  (State (Turn Player1) None  (9, 5) myTestBoard2 []) 6)
                                           (2)  
                              )

lookAheadAiTest :: Test
lookAheadAiTest = Test "AI on MiniMax algo" (assertEqual (lookAheadAI  (State (Turn Player1) None  (9, 5) myTestBoard2 []) 6)
                                                         (Move Approach (Location 3 2) (Location 3 1))
                                        )