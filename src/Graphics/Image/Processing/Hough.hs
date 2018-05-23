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

hough
  :: forall arr a.
     (Array arr RGB a, Elevator a, Fractional a)
  => Image arr RGB a
  -> Image arr RGB a
hough image = hImage
 where
   widthMax = ((rows image) - 1) 
   xCtr = (widthMax / 2)
   heightMax = ((cols image) - 1)
   yCtr = (heightMax / 2)
   luma = IP.toImageY image  
   
   slope x y =
     let orig = I.index luma (xCtr, yCtr) 
         x_ = I.index luma (widthMax,y)		 
         y_ = I.index luma (x,heightMax)
     in fromIntegralP (orig - x_, orig - y_)
  
   gradMap =
     [ ((x, y), slope x y)
     | x <- [0 .. widthMax]
     , y <- [0 .. heightMax] ]
   


   hImage :: Image arr RGB a
   hImage = makeImage (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j)) / 400)

main :: IO()
main = do
  ans <- getArgs
  putStrLn "ans"
  let frog :: Image VU RGB Double
      frog = makeImageR VU (200, 200) (\(i, j) -> PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j)) / 400) 
  writeImage "test.png" frog 
  --hough frog






