module Fifteen.Logic where

import Data.Maybe ( fromJust )
import Numeric.Natural
import System.Random

import qualified Data.Bimap as B

data Tile = T1  | T2  | T3  | T4
          | T5  | T6  | T7  | T8
          | T9  | T10 | T11 | T12
          | T13 | T14 | T15 | T0
  deriving (Eq, Show, Read, Ord)

class SuccPred a where
  mSucc :: a -> Maybe a
  mPred :: a -> Maybe a

data Row = R1 | R2 | R3 | R4
  deriving (Eq, Show, Read, Ord)

instance SuccPred Row where
  mSucc R1 = Just R2
  mSucc R2 = Just R3
  mSucc R3 = Just R4
  mSucc R4 = Nothing

  mPred R1 = Nothing
  mPred R2 = Just R1
  mPred R3 = Just R2
  mPred R4 = Just R3

instance SuccPred Col where
  mSucc C1 = Just C2
  mSucc C2 = Just C3
  mSucc C3 = Just C4
  mSucc C4 = Nothing

  mPred C1 = Nothing
  mPred C2 = Just C1
  mPred C3 = Just C2
  mPred C4 = Just C3

data Col = C1 | C2 | C3 | C4
  deriving (Eq, Show, Read, Ord)

newtype Board = Board (B.Bimap (Row,Col) Tile)
  deriving (Eq, Show)

solvedBoard :: Board
solvedBoard = Board $ B.fromList $
  [ ((R1, C1), T1)
  , ((R1, C2), T2)
  , ((R1, C3), T3)
  , ((R1, C4), T4)
  , ((R2, C1), T5)
  , ((R2, C2), T6)
  , ((R2, C3), T7)
  , ((R2, C4), T8)
  , ((R3, C1), T9)
  , ((R3, C2), T10)
  , ((R3, C3), T11)
  , ((R3, C4), T12)
  , ((R4, C1), T13)
  , ((R4, C2), T14)
  , ((R4, C3), T15)
  , ((R4, C4), T0)
  ]

tileNum :: Tile -> Natural
tileNum T1  = 1
tileNum T2  = 2
tileNum T3  = 3
tileNum T4  = 4
tileNum T5  = 5
tileNum T6  = 6
tileNum T7  = 7
tileNum T8  = 8
tileNum T9  = 9
tileNum T10 = 10
tileNum T11 = 11
tileNum T12 = 12
tileNum T13 = 13
tileNum T14 = 14
tileNum T15 = 15
tileNum T0  = 0

tileStr :: Tile -> String
tileStr T0 = ""
tileStr t = show (tileNum t)

tileAt :: Board -> Row -> Col -> Tile
tileAt (Board bm) r c = fromJust $ B.lookup (r, c) bm

tileNumAt :: Board -> Row -> Col -> Natural
tileNumAt b r c = tileNum (tileAt b r c)

-- | A single move -- slide a tile up, down, left, or right.
data Move = MUp | MDown | MLeft | MRight
  deriving (Eq, Show, Read, Enum)

instance Random Move where
  randomR = error "randomR not implemented for Move"
  random g = let (i, g') = random g
             in (toEnum (i`mod`4), g')

randomTake :: (RandomGen g, Random a) => g -> Int -> ([a], g)
randomTake g i | i <= 0 = ([], g)
               | otherwise = let (a, g') = random g
                                 (as, g'') = randomTake g' (i-1)
                             in (a:as, g'')

shuffleBoard :: RandomGen g => g -> Board -> (Board, g)
shuffleBoard g b = let (moves, g') = randomTake g 1000
                   in (foldr move b moves, g')

move :: Move -> Board -> Board
move m (Board bm) =
  let (blankR, blankC) = fromJust $ B.lookupR T0 bm
      -- Determine the row and column of the tile we are moving
      moveR = case m of
        MUp -> mSucc blankR
        MDown -> mPred blankR
        _ -> Just blankR
      moveC = case m of
        MLeft -> mSucc blankC
        MRight -> mPred blankC
        _ -> Just blankC
  in case (moveR, moveC) of
    (Just r, Just c) -> let t = fromJust $ B.lookup (r, c) bm
                        in Board $
                           B.insert (blankR, blankC) t $
                           B.insert (r, c) T0 $
                           bm
    _ -> Board bm

isSolved :: Board -> Bool
isSolved b = tileNumAt b R1 C1 == 1  &&
             tileNumAt b R1 C2 == 2  &&
             tileNumAt b R1 C3 == 3  &&
             tileNumAt b R1 C4 == 4  &&
             tileNumAt b R2 C1 == 5  &&
             tileNumAt b R2 C2 == 6  &&
             tileNumAt b R2 C3 == 7  &&
             tileNumAt b R2 C4 == 8  &&
             tileNumAt b R3 C1 == 9  &&
             tileNumAt b R3 C2 == 10 &&
             tileNumAt b R3 C3 == 11 &&
             tileNumAt b R3 C4 == 12 &&
             tileNumAt b R4 C1 == 13 &&
             tileNumAt b R4 C2 == 14 &&
             tileNumAt b R4 C3 == 15 &&
             tileNumAt b R4 C4 == 0
