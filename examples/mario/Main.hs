{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE BangPatterns      #-}
module Main where

import           Data.Function
import qualified Data.Map      as M
import           Data.Monoid

import           Miso
import           Miso.String (MisoString, pack)

import           Paths_miso

data Action
  = GetArrows !Arrows
  | Time !Double
  | WindowCoords !(Int,Int)

foreign import javascript unsafe "$r = performance.now();"
  now :: IO Double

main :: IO ()
main = do
    time <- now
    imgs <- loadImages
    let m = mario time
    startApp App
      { model = m
      , view = display imgs
      , update = updateMario
      , events = defaultEvents
      , subs   = [ arrowsSub GetArrows
                 , windowSub WindowCoords
                 ]
      }

-- | An image is determined just by its location.
type Image = FilePath

-- | All images preloaded.
-- We only need images for Mario's motion.
data Images = Images
  { imagesMotion :: Motion -> FilePath
  }

instance Eq Images where _ == _ = True

-- | Load a bunch of values and make an index function.
loadAll :: (Eq a, Applicative f) => (a -> f Image) -> [a] -> f (a -> Image)
loadAll load xs = indexEnum <$> sequenceA (map load xs)
  where
    indexEnum ps x = case lookup x (zip xs ps) of
      Nothing -> "//:0"   -- this basically means no src
      Just p  -> p

loadImages :: IO Images
loadImages = Images <$> loadAll loadMotionImage allMotions

loadMotionImage :: Motion -> IO Image
loadMotionImage Motion{..} = getDataFileName
  ("examples/mario/imgs/" <> show motionStyle <> "/" <> show motionDirection <> ".gif")

data Model = Model
    { x :: !Double
    , y :: !Double
    , vx :: !Double
    , vy :: !Double
    , dir :: !Direction
    , time :: !Double
    , delta :: !Double
    , arrows :: !Arrows
    , window :: !(Int,Int)
    } deriving (Eq)

data Direction
  = L
  | R
  deriving (Show, Eq, Enum, Bounded)

allDirections :: [Direction]
allDirections = [minBound..maxBound]

data MotionStyle
  = Jump
  | Walk
  | Stand
  deriving (Show, Eq, Enum, Bounded)

allMotionStyles :: [MotionStyle]
allMotionStyles = [minBound..maxBound]

data Motion = Motion
  { motionDirection :: Direction
  , motionStyle     :: MotionStyle
  } deriving (Show, Eq, Bounded)

allMotions :: [Motion]
allMotions = Motion
  <$> allDirections
  <*> allMotionStyles

mario :: Double -> Model
mario t = Model
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = R
    , time = t
    , delta = 0
    , arrows = Arrows 0 0
    , window = (0,0)
    }

updateMario :: Action -> Model -> Effect Model Action
updateMario (GetArrows arrs) m = step newModel
  where
    newModel = m { arrows = arrs }
updateMario (Time newTime) m = step newModel
  where
    newModel = m { delta = (newTime - time m) / 20
                 , time = newTime
                 }
updateMario (WindowCoords coords) m = step newModel
  where
    newModel = m { window = coords }

step :: Model -> Effect Model Action
step m@Model{..} = k <# do Time <$> now
  where
    k = m & gravity delta
          & jump arrows
          & walk arrows
          & physics delta

jump :: Arrows -> Model -> Model
jump Arrows{..} m@Model{..} =
    if arrowY > 0 && vy == 0
      then m { vy = 6 }
      else m

gravity :: Double -> Model -> Model
gravity dt m@Model{..} =
  m { vy = if y > 0 then vy - (dt / 4) else 0 }

physics :: Double -> Model -> Model
physics dt m@Model{..} =
  m { x = x + dt * vx
    , y = max 0 (y + dt * vy)
    }

walk :: Arrows -> Model -> Model
walk Arrows{..} m@Model{..} =
  m { vx = fromIntegral arrowX
    , dir = if | arrowX < 0 -> L
               | arrowX > 0 -> R
               | otherwise -> dir
    }

display :: Images -> Model -> View action
display Images{..} m@Model{..} = marioImage
  where
    (h,w) = window
    verb = if | y > 0 -> Jump
              | vx /= 0 -> Walk
              | otherwise -> Stand
    groundY = 62 - (fromIntegral (fst window) / 2)
    marioImage =
      div_ [ height_ $ pack (show h)
           , height_ $ pack (show w)
           ] [ img_ [ height_ "37"
                    , width_ "37"
                    , src_ (pack (imagesMotion (Motion dir verb)))
                    , style_ (marioStyle m groundY)
                    ] [] ]

marioStyle :: Model -> Double -> M.Map MisoString MisoString
marioStyle Model {..} gy =
  M.fromList [ ("transform", matrix x $ abs (y + gy) )
             , ("display", "block")
             ]

matrix :: Double -> Double -> MisoString
matrix x y =
  "matrix(1,0,0,1,"
     <> pack (show x)
     <> ","
     <> pack (show y)
     <> ")"
