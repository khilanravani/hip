{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Hough Transform is used as a part of feature extraction in images. 
-- It is a tool that makes it far easier to identify straight lines in
-- the source image, whatever their orientation.
module Graphics.Image.Processing.Hough where

import Control.Monad (forM_, when)
import qualified Data.Foldable as F (maximum)
import Data.Array
import Data.Array.ST (newArray, writeArray, readArray, runSTArray)

import Prelude as P hiding (subtract)
import Graphics.Image
import Graphics.Image.Interface as I
import Graphics.Image.Types as IP

-- | Some helper functions :
-- | toImageY : Converts an image to Luma Image
toImageY :: (ToY cs e, IP.Array arr cs e, IP.Array arr Y Double) =>
            Image arr cs e
         -> Image arr Y Double
toImageY = I.map toPixelY

-- | Trivial function for subtracting co-ordinate pairs 
sub :: Num x => (x, x) -> (x, x) -> (x, x)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- | Compute the sum of squares or dot product of a given pair of co-ordinates
dotProduct :: Num x => (x, x) -> (x, x) -> x
dotProduct (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

-- | Conversion of pair fromIntegral
fromIntegralP :: (Integral x, Num y) => (x, x) -> (y, y)
fromIntegralP (x1, y1) = (fromIntegral x1, fromIntegral y1)

-- | Compute magnitude
mag :: Floating x => (x, x) -> x
mag x = sqrt (dotProduct x x)

-- | 'hough' computes the Linear Hough Transform and maps each point in the target image, ​ (ρ, θ) ​ 
-- to the average color of the pixels on  the corresponding line of the source image ​(x,y) ​- space,
-- where the line corresponds to points of the form ​(xcosθ + ysinθ = ρ(rho)). 
--
-- The idea is that where there is a straight line in the original image, it corresponds to a 
-- bright (or dark, depending on the color of the background field) spot; by applying a suitable 
-- filter to the results of the transform, it is possible to extract the locations of the lines in the original image.
--
-- <<images/frog_rbg.jpg>>
--
-- Usage : 
--	
-- >>> frog <- readImageRGB VU "frog_rbg.jpg"
-- >>> input1 <- getLine
-- >>> input2 <- getLine
-- >>> let thetaSz = (P.read input1 :: Int)
-- >>> let distSz = (P.read input2 :: Int)
-- >>> let houghImage :: Image VU RGB Double
-- >>>     houghImage = hough frog thetaSz distSz
-- >>> writeImage "test.png" houghImage
--
hough
  :: forall arr a.
     ( IP.Array arr RGB a, IP.Array arr RGB Word8
     , MArray arr Y Double, IP.Array arr Y Double
     , IP.Array arr RGB Double
     )
  => Image arr RGB a
  -> Int
  -> Int
  -> Image arr RGB Double
hough image thetaSz distSz = I.map (fmap toDouble) hImage
 where
   widthMax, xCtr, heightMax, yCtr :: Int
   widthMax = ((rows image) - 1)
   xCtr = (widthMax `div` 2)
   heightMax = ((cols image) - 1)
   yCtr = (heightMax `div` 2)

   luma :: Image arr Y Double
   luma = IP.toImageY image

   slope :: Int -> Int -> (Double, Double)
   slope x y =
     let PixelY orig = I.index luma (x, y)
         PixelY x' = I.index luma (min (x+1) widthMax, y)
         PixelY y' = I.index luma (x, min (y+1) heightMax)
     in (orig - x', orig - y')

   slopeMap :: [ ((Int, Int), (Double, Double)) ]
   slopeMap = [ ((x, y), slope x y) | x <- [0 .. widthMax], y <- [0 .. heightMax] ]

   distMax :: Double -- Compute Maximum distance
   distMax = (sqrt . fromIntegral $ (heightMax + 1) ^ (2 :: Int) + (widthMax + 1) ^ (2 :: Int)) / 2 

   accBin = runSTArray $   -- Core part of Algo begins here. Working in a safe way with a mutable array.
     do arr <- newArray ((0, 0), (thetaSz, distSz)) (0 :: Double) -- Build a new array, with every element initialised to the supplied value.
        forM_ slopeMap $ \((x, y), gradient) -> do
            let (x', y') = fromIntegralP $ (xCtr, yCtr) `sub` (x, y)
            when (mag gradient > 0) $
              forM_ [0 .. thetaSz] $ \theta -> do
                let theta_ =
                      fromIntegral theta * 360 / fromIntegral thetaSz / 180 *
                      pi :: Double
                    distance = cos theta_ * x' + sin theta_ * y'    -- (ρ(rho) = xcosθ + ysinθ)
                    distance_ = truncate $ distance * fromIntegral distSz / distMax -- returns the nearest integer
                    idx = (theta, distance_)
                when (distance_ >= 0 && distance_ < distSz) $
                  do old <- readArray arr idx      -- read an element at 'idx' from mutable array 'arr'
                     writeArray arr idx (old + 1)
        return arr

   maxAcc = F.maximum accBin  
   hTransform (x, y) =
        let l = 255 - truncate ((accBin ! (x, y)) / maxAcc * 255) -- pixel generating function
        in PixelRGB l l l

   hImage :: Image arr RGB Word8
   hImage = makeImage (thetaSz, distSz) hTransform


test :: IO ()
test = do
      frog <- readImageRGB VU "frog_rbg.jpg"
      input1 <- getLine
      input2 <- getLine
      let thetaSz = (P.read input1 :: Int)
      let distSz = (P.read input2 :: Int)
      writeImage "input.png" frog
      let houghImage :: Image VU RGB Double
          houghImage = hough frog thetaSz distSz
      writeImage "test.png" houghImage
