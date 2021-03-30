module Main where

import Control.Monad (void, forever)

import Brick
import Brick.BChan
import Data.Time.Clock
import Fifteen.Logic
import Fifteen.UI
import Graphics.Vty
import System.Random

import Control.Concurrent

main :: IO ()
main = do
  g <- getStdGen
  let (b, g') = shuffleBoard g solvedBoard
      builder = mkVty defaultConfig
  initialVty <- builder
  eventChannel <- newBChan 10
  void . forkIO $ forever $ do
    t <- getCurrentTime
    writeBChan eventChannel (Tick t)
    threadDelay 100000

  st <- getCurrentTime
  void $ customMain
    initialVty
    builder
    (Just eventChannel)
    fifteenApp
    (GameState b g' InProgress st st)
