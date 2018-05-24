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
--import Data.Array.MArray

import Prelude as P hiding (subtract)
import Graphics.Image.Processing.Filter	
import Graphics.Image
import Graphics.Image.ColorSpace
import Graphics.Image.IO
import Graphics.Image.Interface as I 
import Graphics.Image.Types as IP

toImageY :: (ToY cs e, Array arr cs e, Array arr Y Double) =>
            Image arr cs e
         -> Image arr Y Double
toImageY = I.map toPixelY

-- ####### Some trivial functions ########
sub :: Num x => (x, x) -> (x, x) -> (x, x)
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

dotProduct :: Num x => (x, x) -> (x, x) -> x
dotProduct (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

mag :: Floating x => (x, x) -> x
mag x = sqrt (dotProduct x x)

fromIntegralP :: (Integral x, Num y) => (x, x) -> (y, y)
fromIntegralP (x1, y1) = (fromIntegral x1, fromIntegral y1)

minLineLength :: Int
minLineLength = 100
   
maxLineGap :: Int
maxLineGap = 10

hough
  :: forall arr a.
     (Array arr RGB a, Elevator a, Fractional a, Array arr Y Double, MArray arr Y Double)
  => Image arr RGB a
  -> Image arr RGB a
hough image = hImage
 where
   widthMax = ((rows image) - 1) 
   xCtr = (widthMax `div` 2)
   heightMax = ((cols image) - 1)
   yCtr = (heightMax `div` 2)
   luma = IP.toImageY image  
   slope x y =
     let orig = I.index luma (xCtr, yCtr) 
         x_ = I.index luma (widthMax, y)		 
         y_ = I.index luma (x, heightMax)
     in (orig - x_, orig - y_)
   slopeMap = [ ((x, y), slope x y) | x <- [0 .. widthMax], y <- [0 .. heightMax] ]
   distMax :: Double
   distMax = (sqrt . fromIntegral $ (heightMax + 1) ^ 2 + (widthMax + 1) ^ 2) / 2
  
   accBin = runST $
     do arr <- new (widthMax, heightMax)
        forM_ slopeMap $ \((x, y), gradient) -> do
            let (x_, y_) = fromIntegralP ((xCtr, yCtr) `sub` (x, y))
            when ((mag gradient) > 127) $
              forM_ [0 .. widthMax] $ \theta -> do
                let theta_ =
                      fromIntegral theta * 360 / fromIntegral heightMax / 180 *
                      pi :: Double
                    distance = round (cos theta_ * x_ + sin theta_ * y_) * ( widthMax / fromIntegral distMax)
                    idx = (theta, distance)
                when (distance>= 0 && distance < heightMax) $
                  do old <- I.read arr idx
                     write arr idx (old + 1)
        return arr

   maxAcc = F.maximum accBin 
   hTransform x y =
       let l = 255 - round ((I.index accBin (x, y)) /255 ) * maxAcc
       in PixelRGB l l l
   hImage :: Image arr RGB Integer
   hImage = makeImage (widthMax, heightMax) hTransform
   --hImage = makeImage (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j)) / 400)

main :: IO()
main = do
  ans <- getArgs
  putStrLn "ans"
  let frog :: Image VU RGB Double
      frog = makeImageR VU (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j)) / 400) 
  writeImage "test.png" frog 
  --hough frog
