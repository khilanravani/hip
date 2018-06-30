{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-} 

module Graphics.Image.Processing.Gabor where

import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.STRef 
import Data.Complex (Complex((:+)))

import Prelude as P hiding (subtract)
import Graphics.Image.Processing.Filter
import Graphics.Image.Processing.Geometric
import Graphics.Image.Interface as I
import Graphics.Image
import Graphics.Image.Types as IP
import Graphics.Image.ColorSpace (X)

gaborfn
  :: RealFloat p => p -> p -> p -> p -> p -> p -> p -> Complex p
gaborfn λ θ ψ σ γ x y = exp ( (-0.5) * ((x'^2 + γ^2*y'^2) / (σ^2)) :+ 0) * exp ( 0 :+ (2*pi*(x'/λ+ψ)) )
    where x' =  x * cos θ + y * sin θ
          y' = -x * sin θ + y * cos θ

gaborFilter :: (Array arr cs e, Array arr X e) => Direction -> Border (Pixel cs e) -> Filter arr cs e
gaborFilter dir !border =
  Filter (correlate border kernel)
  where
    widthMax, var1, heightMax, var2 :: Int
    var1 = ((rows ip) - 1)
    widthMax = ((rows ip) - 1)
    var2 = ((cols ip) - 1)
    heightMax = ((cols ip) - 1)
    γ = 0.5
    λ = 2
    θ = 0
    σ = 8
    ψ = 0
    u = 5
    v = 8
    m = 39
    n = 39
    kernel = runSTArray (Int, Int) Double $
      do gArray <- newArray ((0, 0), (u, v))    
         forM_ [0 .. u] $ \i -> do
           forM_ [0 .. v] $ \j -> do
             do arr <- newArray (m, n) 
             forM_ [0 .. m] $ \x -> do
               forM_ [0 .. n] $ \y -> do
                 let px = gaborfn (x, y, σ, θ, γ, λ, ψ)
                 writeArray arr (x, y) px
             let idx = (i, j)
             writeArray gArray idx arr


gaborfeatures
  :: forall arr e cs . ( MArray arr Y Double, IP.Array arr Y Double, IP.Array arr Y Word16, MArray arr Y Word16, Array arr X Double)
  => Image arr Y Double
  -> Array arr Y Double
  -> Int  
  -> Int   
  -> Image arr Y Word16

gaborfeatures image gArray thetaSz distSz = I.map (fmap toWord16) accBin
 where
   u = 5
   v = 8
   accBin :: Image arr Y Word16
   accBin = runST $            
     do gResult <- I.new (u, v)   
        forM_ [0 .. u] $ \i -> do
          forM_ [0 .. v] $ \j -> do
            let ip = applyFilter (gaborFilter I.index gArray (i,j)) image 
            writeArray gResult (i, j) ip  
   
   features :: Image arr Y Word16
   features = runST $            
     do featureVector <- I.new (u, v)   
        forM_ [0 .. u] $ \i -> do
          forM_ [0 .. v] $ \j -> do
            let gaborAbs = abs (I.index gResult (i, j))
                gaborAbsDs = downsample ((0 ==) . (`mod` 4)) gaborAbs
            let gaborAbsTr = transpose (gaborAbs)
                gaborAbsTrDs = downsample ((0 ==) . (`mod` 4)) gaborAbs
            let gaborFinal = (gaborAbsDs - mean(gaborAbsDs))/ stddev (gaborAbsDs)
            I.write featureVector (i, j) (PixelY (fromIntegral gaborFinal))
     
        freeze featureVector


