{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (replicateM)
import Data.Bool (bool)
import Data.Foldable (foldl', traverse_)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

import Data.Char.Block (Row(..))
import Data.Char.Braille (Braille(..), braille)

import FRP.Rhine hiding (average)

import Data.AffineSpace
import Data.Point2
import Data.Vector2

import qualified Data.Vector as V


-- Core logic.

data Boid = Boid
  { boidPos :: Point2 Double
  , boidVel :: Vector2 Double
  } deriving (Show)

mkBoid :: Point2 Double -> Vector2 Double -> Boid
mkBoid pos vel = Boid { boidPos = pos, boidVel = vel }

boidDist :: Boid -> Boid -> Double
boidDist b1 b2 = distance (boidPos b1) (boidPos b2)

boidPosDiff :: Boid -> Boid -> Vector2 Double
boidPosDiff b1 b2 = boidPos b1 .-. boidPos b2

sumVectors :: (Foldable t, Num a, VectorSpace v a) => t v -> v
sumVectors = foldl' (^+^) zeroVector

averageVectors :: (Foldable t, Num a, VectorSpace v a) => t v -> Maybe v
averageVectors xs
  | null xs   = Nothing
  | otherwise = Just (sumVectors xs ^/ fromIntegral (length xs))

-- ... am i really doing this ...
averagePoints :: (Functor t, Foldable t, AffineSpace p v a) => t p -> Maybe p
averagePoints xs = (origin .+^) <$> averageVectors ((.-. origin) <$> xs)

isNeighbor :: Boid -> Boid -> Bool
isNeighbor b1 b2 = boidDist b1 b2 < neighborThresh
  where neighborThresh = 0.2

neighboringBoids :: Boid -> [Boid] -> [Boid]
neighboringBoids b bs = filter (isNeighbor b) bs

isTooClose :: Boid -> Boid -> Bool
isTooClose b1 b2 = boidDist b1 b2 < tooCloseThresh
  where tooCloseThresh = 0.05

tooCloseBoids :: Boid -> [Boid] -> [Boid]
tooCloseBoids b = filter (isTooClose b)

centerBoid :: [Boid] -> Boid -> Vector2 Double
centerBoid boids b = fromMaybe zeroVector ((centeringPower *^) <$> neighborDiff)
  where
    neighborDiff = (.-. boidPos b) <$> neighborsCenter
    neighborsCenter = averagePoints $ map boidPos (tooCloseBoids b boids)
    centeringPower = 0.1

avoidBoids :: [Boid] -> Boid -> Vector2 Double
avoidBoids boids b = avoidPower *^ sumVectors (map (boidPosDiff b) (tooCloseBoids b boids))
  where avoidPower = 0.03

matchVelocity :: [Boid] -> Boid -> Vector2 Double
matchVelocity boids b = fromMaybe zeroVector ((matchingPower *^) <$> neighborDiff)
  where
    neighborDiff = (^-^ boidVel b) <$> neighborsAvgVel
    neighborsAvgVel = averageVectors $ map boidVel (neighboringBoids b boids)
    matchingPower = 0.05

totalAccel :: [Boid] -> Boid -> Vector2 Double
totalAccel boids b = sumVectors $ map (\f -> f boids b) [centerBoid, avoidBoids, matchVelocity]

outOfBounds :: Point2 Double -> Bool
outOfBounds (Point2 x y) = x < -1.0 || x > 1.0 || y < -1.0 || y > 1.0

wrapPos :: Point2 Double -> Point2 Double
wrapPos (Point2 x y) = Point2 (w x) (w y)
  where w z
          | z < -1.0  = snd (properFraction @_ @Int z) + 1.0
          | z > 1.0   = snd (properFraction @_ @Int z) - 1.0
          | otherwise = z

inspectFor :: (Monad m) => (a -> Bool) -> ClSF (ExceptT a m) cl a a
inspectFor cond = proc a ->
  if cond a
  then throwS -< a
  else returnA -< a

wrappedIntegral :: (Monad m, Diff time ~ Double)
                => Point2 Double
                -> BehaviorF m time (Vector2 Double) (Point2 Double)
wrappedIntegral pos0 = safely (go pos0)
  where go initial = do
          pos <- try $ inspectFor outOfBounds <<< (initial .+^) ^<< integral
          go (wrapPos pos)

boidBehavior :: (Monad m, Diff time ~ Double) => Double -> Boid -> BehaviorF m time (Boid, [Boid]) Boid
boidBehavior timescale boid0 = proc (boid, boids) -> do
  let accel = totalAccel boids boid
  vel <- integralFrom (boidVel boid0) -< timescale *^ accel
  pos <- wrappedIntegral (boidPos boid0) -< timescale *^ vel
  returnA -< Boid { boidPos = wrapPos pos, boidVel = vel }

boidsBehavior :: (Monad m, Diff time ~ Double) => Double -> [Boid] -> BehaviorF m time () [Boid]
-- i love type inference for rank n types
boidsBehavior timescale boids0 = feedback boids0 $ proc ((), boids) -> do
  boids' <- traverse (\(i, b0) -> boidBehavior timescale b0 <<< arr (!! i) &&& arr id)
                     (zip [0..] boids0)
         -< boids
  returnA -< (boids', boids')


-- Rendering logic.

emptyRow :: Row Bool
emptyRow = Row { left = False, right = False }

updateRow :: Int -> a -> Row a -> Row a
updateRow 0 x (Row { .. }) = Row { left = x, right = right }
updateRow 1 x (Row { .. }) = Row { left = left, right = x }
updateRow i _            _ = error $ "index " ++ show i ++ " is out of bounds for a row!"

emptyBraille :: Braille Bool
emptyBraille = Braille { row1 = emptyRow, row2 = emptyRow, row3 = emptyRow, row4 = emptyRow }

updateBraille :: Int -> Int -> a -> Braille a -> Braille a
updateBraille 0 j x b@(Braille { .. }) = b { row1 = updateRow j x row1 }
updateBraille 1 j x b@(Braille { .. }) = b { row2 = updateRow j x row2 }
updateBraille 2 j x b@(Braille { .. }) = b { row3 = updateRow j x row3 }
updateBraille 3 j x b@(Braille { .. }) = b { row4 = updateRow j x row4 }
updateBraille i _ _                  _ = error $ "row index " ++ show i ++ " is out of bounds for a braille!"

type Screen = V.Vector (V.Vector (Braille Bool))

screenSize :: Int
screenSize = 200

blankScreen :: Screen
blankScreen = V.replicate (screenSize `div` 4) (V.replicate (screenSize `div` 2) emptyBraille)

updateAt :: (a -> a) -> Int -> V.Vector a -> V.Vector a
updateAt f i v = v V.// [(i, f (v V.! i))]

updateScreen :: Int -> Int -> Bool -> Screen -> Screen
updateScreen i j b = updateAt (updateAt (updateBraille (i `mod` 4) (j `mod` 2) b) (j `div` 2)) (i `div` 4)

printScreen :: Screen -> IO ()
printScreen = traverse_ (putStrLn . foldr ((:) . braille) "")

quantizePos :: Point2 Double -> Maybe (Int, Int)
quantizePos (Point2 x y) = (,) <$> q x <*> q y
  where q z
          | -1.0 < z && z < 1.0 = Just (round ((z + 1.0) / 2.0 * fromIntegral (screenSize - 1)))
          | otherwise           = Nothing

renderBoid :: Boid -> Screen -> Screen
renderBoid (Boid { .. })
  | Just (i, j) <- quantizePos boidPos = updateScreen i j True
  | otherwise                          = id

renderBoids :: [Boid] -> Screen
renderBoids = foldr renderBoid blankScreen

ansiClear :: IO ()
ansiClear = putStr "\x1b[2J\x1b[H" >> hFlush stdout

printBoids :: BehaviorF IO time [Boid] ()
printBoids = arrMCl $ \boids -> ansiClear >> printScreen (renderBoids boids)


-- Tie it together.

randomBoid :: IO Boid
randomBoid = mkBoid <$> (Point2 <$> randomRIO (-1.0, 1.0) <*> randomRIO (-1.0, 1.0))
                    <*> (vector2 <$> randomRIO (-0.03, 0.03) <*> randomRIO (-0.03, 0.03))

main :: IO ()
main = do
  initialBoids <- replicateM 500 randomBoid
  flow $
    (boidsBehavior 10 initialBoids @@ waitClock @25)
    >-- keepLast initialBoids
    --> (printBoids @@ waitClock @100)
