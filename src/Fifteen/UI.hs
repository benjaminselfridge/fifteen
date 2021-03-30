{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Fifteen.UI where

import Fifteen.Logic

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Graphics.Vty as V
import Data.Time.Clock
import Lens.Micro.Platform
import System.Random
import Text.Printf ( printf )

type Resource = ()

-- | The only additional event we use is a timer event from the outside world
-- telling us the current time so we can update the 'GameState'. It doesn't
-- matter how often these ticks are received, as long as they are requently
-- enough that we can know how many seconds has passed (so something like every
-- tenth of a second should be sufficient).
data FifteenEvent = Tick UTCTime
                    -- ^ When we receive a 'Tick', we update the current time in
                    -- the 'GameState'.

data GameState = GameState { _gsBoard :: Board
                           , _gsRandomGen :: StdGen
                           , _gsGameMode :: GameMode
                           , _gsStartTime :: UTCTime
                             -- ^ Time when the current game was started
                           , _gsCurrentTime :: UTCTime
                             -- ^ Time right now
                           }

data GameMode = InProgress
              -- | The 'Int' is the number of seconds to solve.
              | Solved Int

makeLenses ''GameState

fifteenApp :: B.App GameState FifteenEvent Resource
fifteenApp = B.App
  { B.appDraw = draw
  , B.appChooseCursor = (\_ _ -> Nothing)
  , B.appHandleEvent = handleEvent
  , B.appStartEvent = startEvent
  , B.appAttrMap = attrMap
  }

draw :: GameState -> [B.Widget n]
draw gs = [ B.center ((B.vLimit 5 $ B.center $ B.str "15") B.<=>
                      (B.vLimit 20 $ B.center $ drawBoard (gs ^. gsBoard)) B.<=>
                      (B.vLimit 1 $ B.center $ status (gs ^. gsGameMode) seconds) B.<=>
                      (B.center $ help))
          ]
  where seconds = secondsElapsed gs

drawBoard :: Board -> B.Widget n
drawBoard b =
  (drawTile (tileAt b R1 C1) B.<+>
   drawTile (tileAt b R1 C2) B.<+>
   drawTile (tileAt b R1 C3) B.<+>
   drawTile (tileAt b R1 C4)) B.<=>
  (drawTile (tileAt b R2 C1) B.<+>
   drawTile (tileAt b R2 C2) B.<+>
   drawTile (tileAt b R2 C3) B.<+>
   drawTile (tileAt b R2 C4)) B.<=>
  (drawTile (tileAt b R3 C1) B.<+>
   drawTile (tileAt b R3 C2) B.<+>
   drawTile (tileAt b R3 C3) B.<+>
   drawTile (tileAt b R3 C4)) B.<=>
  (drawTile (tileAt b R4 C1) B.<+>
   drawTile (tileAt b R4 C2) B.<+>
   drawTile (tileAt b R4 C3) B.<+>
   drawTile (tileAt b R4 C4))

drawTile :: Tile -> B.Widget n
drawTile t = B.border $ case t of
  T0 -> B.padLeftRight 2 $ B.padTopBottom 1 $ B.str s
  _ -> B.withDefAttr "tile" $ B.padLeftRight 2 $ B.padTopBottom 1 $ B.str s
  where s = printf "%2s" (tileStr t)

secondsElapsed :: GameState -> Int
secondsElapsed gs = floor $ nominalDiffTimeToSeconds $
  diffUTCTime (gs ^. gsCurrentTime) (gs ^. gsStartTime)

status :: GameMode -> Int -> B.Widget n
status InProgress i = B.str $ "Time: " ++ show i ++ "s"
status (Solved i) _ = B.str $ "Solved (" ++ show i ++ "s)! Nice job!"

help :: B.Widget n
help = B.hBox
  [ B.padLeftRight 1 $
    B.vBox [ B.str "up/down/left/right"
           , B.str "n"
           , B.str "q"
           ]
  , B.padLeftRight 1 $
    B.vBox [ B.str "move tile"
           , B.str "new game"
           , B.str "quit"
           ]
  ]

-- | Apply a move to a 'GameState'. First apply it to the board, and if the
-- resulting board is solved, update the 'GameMode' to 'Solved', with the amount
-- of time the user took to solve it.
gsMakeMove :: Move -> GameState -> GameState
gsMakeMove m gs0 =
  let gs1 = gs0 & gsBoard %~ move m
      gs2 = gs1 & gsGameMode .~ case isSolved (gs1 ^. gsBoard) of
        True -> Solved (secondsElapsed gs0)
        False -> InProgress
  in gs2

-- | Start a new game.
gsNewGame :: GameState -> GameState
gsNewGame gs =
  let (b, g') = shuffleBoard (gs ^. gsRandomGen) (gs ^. gsBoard)
  in GameState b g' InProgress (gs ^. gsCurrentTime) (gs ^. gsCurrentTime)

handleEvent :: GameState
            -> B.BrickEvent Resource FifteenEvent
            -> B.EventM Resource (B.Next GameState)
handleEvent gs be = case gs ^. gsGameMode of
  InProgress -> case be of
    B.VtyEvent (V.EvKey V.KUp []) -> B.continue (gsMakeMove MUp gs)
    B.VtyEvent (V.EvKey V.KDown []) -> B.continue (gsMakeMove MDown gs)
    B.VtyEvent (V.EvKey V.KLeft []) -> B.continue (gsMakeMove MLeft gs)
    B.VtyEvent (V.EvKey V.KRight []) -> B.continue (gsMakeMove MRight gs)
    B.VtyEvent (V.EvKey (V.KChar 'n') []) -> B.continue (gsNewGame gs)
    B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt gs
    B.AppEvent (Tick currentTime) -> B.continue (gs & gsCurrentTime .~ currentTime)
    _ -> B.continue gs
  Solved _ -> case be of
    B.VtyEvent (V.EvKey (V.KChar 'n') []) -> B.continue (gsNewGame gs)
    B.VtyEvent (V.EvKey (V.KChar 'q') []) -> B.halt gs
    B.AppEvent (Tick currentTime) -> B.continue (gs & gsCurrentTime .~ currentTime)
    _ -> B.continue gs

startEvent :: GameState -> B.EventM Resource GameState
startEvent gs = return gs

attrMap :: GameState -> B.AttrMap
attrMap gs = case gs ^. gsGameMode of
  InProgress ->
    B.attrMap V.defAttr
    [ ("tile", V.withBackColor V.defAttr V.blue)
    ]
  Solved _ ->
    B.attrMap V.defAttr
    [ ("tile", V.withBackColor V.defAttr V.green)
    ]
