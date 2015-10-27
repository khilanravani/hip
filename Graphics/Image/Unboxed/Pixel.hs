{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TemplateHaskell,
TypeFamilies, UndecidableInstances #-}
-- |
-- Module      : Graphics.Image.Unboxed.Pixel
-- Copyright   : (c) Alexey Kuleshevich 2015
-- License     : MIT
--
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
-- This module contains all available pixel types at the moment.
module Graphics.Image.Unboxed.Pixel (
  -- * Pixel 
  Pixel, I.Inner,
  -- ** Grayscale
  module Graphics.Image.Interface.Pixel.Gray,
  -- ** Color
  module Graphics.Image.Interface.Pixel.RGB,
  rgbToHSI, rgbToGray,
  module Graphics.Image.Interface.Pixel.HSI,
  hsiToRGB, hsiToGray,
  -- ** Binary
  module Graphics.Image.Interface.Pixel.Binary,
  -- ** Complex
  module Graphics.Image.Interface.Pixel.Complex,
  ComplexInner,
  -- ** Alpha
  module Graphics.Image.Interface.Pixel.Alpha,
  AlphaInner
  ) where

import Data.Vector.Unboxed (Unbox)
import Data.Vector.Unboxed.Deriving
import Graphics.Image.Interface.Pixel (
  grayToRGB, grayToHSI, rgbToHSI, rgbToGray, hsiToRGB, hsiToGray, Inner)
import qualified Graphics.Image.Interface.Pixel as P (ComplexInner, AlphaInner)
import qualified Graphics.Image.Interface as I (Pixel(..))
import Graphics.Image.Interface.Pixel.Binary
import Graphics.Image.Interface.Pixel.Gray
import Graphics.Image.Interface.Pixel.RGB
import Graphics.Image.Interface.Pixel.HSI
import Graphics.Image.Interface.Pixel.Complex hiding (ComplexInner)
import Graphics.Image.Interface.Pixel.Alpha hiding (AlphaInner)

import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable


-- | Unboxed Vector can only work with 'I.Pixel's that implement 'Unbox'
class (Unbox px, I.Pixel px) => Pixel px where


-- | Unboxed Vector can only work with 'I.Pixel's that implement 'Unbox'. Also
-- Pixel that are instances of this class can be used with 'Complex' pixel.
class (Unbox px, Unbox (Inner px), P.ComplexInner px
      ) => ComplexInner px where

-- | Unboxed Vector can only work with 'I.Pixel's that implement 'Unbox'. Also
-- Pixel that are instances of this class can be used with 'Alpha' pixel.
class (Unbox px, Unbox (Inner px), P.AlphaInner px
      ) => AlphaInner px where
        

-- | Unboxed Pixel  
instance Pixel Binary where

-- | Unboxed Pixel  
instance Pixel Gray where

-- | Unboxed Pixel  
instance Pixel RGB where

-- | Unboxed Pixel  
instance Pixel HSI where
 
-- | Unboxed Pixel  
instance ComplexInner px => Pixel (Complex px) where

-- | Unboxed Pixel  
instance AlphaInner px => Pixel (Alpha px) where
  

-- | Unboxed Pixel  
instance ComplexInner Gray where

-- | Unboxed Pixel  
instance ComplexInner RGB where

-- | Unboxed Pixel  
instance ComplexInner HSI where

-- | Unboxed Pixel  
instance (Pixel (Alpha px), ComplexInner px, AlphaInner px) =>
         ComplexInner (Alpha px) where
  

-- | Unboxed Pixel  
instance AlphaInner Gray where

-- | Unboxed Pixel  
instance AlphaInner RGB where

-- | Unboxed Pixel  
instance AlphaInner HSI where

  
derivingUnbox "GrayPixel"
    [t| Gray -> Double |]
    [| \(Gray y) -> y  |]
    [| \y -> Gray y    |]


derivingUnbox "BinaryPixel"
    [t| Binary -> Bool             |]
    [| isOn                        |]
    [| \v -> if v then on else off |]


derivingUnbox "RGBPixel"
    [t| RGB -> (Double, Double, Double) |]
    [| \(RGB r g b) -> (r, g, b)        |]
    [| \(r, g, b) -> RGB r g b          |]


derivingUnbox "HSIPixel"
    [t| HSI -> (Double, Double, Double) |]
    [| \(HSI h s i) -> (h, s, i)        |]
    [| \(h, s, i) -> HSI h s i          |]


derivingUnbox "ComplexPixel"
    [t| ComplexInner px => Complex px -> (px, px) |]
    [| \(px1 :+: px2) -> (px1, px2)               |]
    [| \(px1, px2) -> px1 :+: px2                 |]


derivingUnbox "AlphaPixel"
    [t| AlphaInner px => Alpha px -> (Inner px, px) |]
    [| \(Alpha a px) -> (a, px)                     |]
    [| \(a, px) -> Alpha a px                       |]


