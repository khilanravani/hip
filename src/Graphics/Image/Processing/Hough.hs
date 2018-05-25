{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Image.Processing.Hough where

import Control.Applicative
import System.Environment (getArgs, getProgName)
import Control.Monad (forM_, when)
import Control.Monad.ST
import qualified Data.Foldable as F (maximum)
import Data.List

import Prelude as P hiding (subtract)
import Graphics.Image.Processing.Filter	
import Graphics.Image
import Graphics.Image.ColorSpace
import Graphics.Image.IO
import Graphics.Image.Interface as I 
import Graphics.Image.Types as IP

-- ####### Some trivial functions ########
toImageY :: (ToY cs e, Array arr cs e, Array arr Y Double) =>
            Image arr cs e
         -> Image arr Y Double
toImageY = I.map toPixelY

minLineLength :: Int
minLineLength = 100

sub :: Num x => (x, x) -> (x, x) -> (x, x)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

dotProduct :: Num x => (x, x) -> (x, x) -> x
dotProduct (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

mag :: Floating x => (x, x) -> x
mag x = sqrt (dotProduct x x)

fromIntegralP :: (Integral x, Num y) => (x, x) -> (y, y)
fromIntegralP (x1, y1) = (fromIntegral x1, fromIntegral y1)
   
maxLineGap :: Int
maxLineGap = 10

hough
  :: forall arr a b.
     ( Array arr RGB a, Array arr RGB b
     , Array arr Y Int, MArray arr Y Int
     , Array arr Y Double, MArray arr Y Double) => Image arr RGB a -> Image arr RGB b

hough image = hImage
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

   distMax :: Double
   distMax = (sqrt . fromIntegral $ (heightMax + 1) ^ (2 :: Int) + (widthMax + 1) ^ (2 :: Int)) / 2

   accBin :: Image arr Y Int
   accBin = runST $
     do arr <- new (widthMax, heightMax)
        forM_ slopeMap $ \((x, y), gradient) -> do
            let (x', y') = fromIntegralP ((xCtr, yCtr) `sub` (x, y))
            when ((mag gradient) > 127) $
              forM_ [0 .. widthMax] $ \theta -> do
                let theta_ =
                      fromIntegral theta * 360 / fromIntegral heightMax / 180 *
                      pi :: Double
                    distance = cos theta_ * x' + sin theta_ * y' * ( fromIntegral widthMax / distMax)
                    idx = (theta, round distance)
                when (distance>= 0 && distance < fromIntegral heightMax) $
                  do old <- I.read arr idx
                     write arr idx (old + 1)
        freeze arr

   maxAcc :: Int
   PixelY maxAcc = I.fold (\p1@(PixelY a) p2@(PixelY b) -> if a < b then p2 else p1) (PixelY 0) accBin

   hTransform :: (Int, Int) -> Pixel RGB b
   hTransform (x, y) =
       let PixelY acc_xy = I.index accBin (x, y)
           l = fromIntegral $ 255 - (acc_xy `div` 255) * maxAcc
       in PixelRGB l l l

   hImage = makeImage (widthMax, heightMax) hTransform
   
test :: IO ()
test = do
  let frog :: Image VU RGB Double
      frog = makeImageR VU (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j)) / 400) 
  let houghImage :: Image VU RGB Double
      houghImage = hough frog
  writeImage "test.png" houghImage
