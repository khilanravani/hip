import Control.Applicative
import System.Environment (getArgs, getProgName)
import Control.Monad (forM_, when)
import Control.Monad.ST
import qualified Data.Foldable as F (maximum)
import Data.Massiv.Array.IO
import Data.List

import Codec.Picture.Types(dropTransparency)
import Codec.Picture
import Prelude as P
import Graphics.Image.ColorSpace
import Graphics.Image.IO
import Graphics.Image.Interface as I 
import Graphics.Image.Types as IP


-- ######### Read Image ##########
readImageRGB :: Array arr RGB Double => arr -> FilePath -> IO (Image arr RGB Double)
readImageRGB _ = readImage'

-- frog <- readImageRGB VU "images/frog.jpg"
-- writeImage "images/frog_eye_grid.png" $ pixelGrid 10 $ crop (51, 112) (20, 20) frog

-- makeImage :: Array arr cs e => (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image arr cs e 

-- ######## Convert to Luma #########

toPixelY :: Pixel cs e -> Pixel Y Double
toImageY :: (ToY cs e, Array arr cs e, Array arr Y Double) =>
            Image arr cs e
         -> Image arr Y Double
toImageY = I.map toPixelY

-- ####### Some trivial functions ########
subtract :: Num x => (x, x) -> (x, x) -> (x, x)
subtract (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

dotProduct :: Num x => (x, x) -> (x, x) -> x
dotProduct (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)
 
magnitude :: Floating x => (x, x) -> x
magnitude x = sqrt (dotProduct x x)

-- trying something new - Functor usage!
instance Functor P where
    fmap f (x :| y) = f x :| f y
 
fromIntegralP :: (Integral a, Num b) => P a -> P b
fromIntegralP = fmap fromIntegral

-- ######## Hough-T function begins

hough :: Image PixelRGB8 -> Int -> Int -> Image PixelRGB8
hough image thetaSize distSize = hImage
 where
 widthMax = (imageWidth image) - 1
 heightMax = (imageHeight image) - 1
 xCtr = widthMax / 2
 yCtr = heightMax / 2
 lumaImg = toPixelY image
{- or let arr = arrLightIx2 Par (600 :. 800) {Generated image} // image
      lumaImg = computeAs S $ fmap PixelY arr
-}
 slope x y =
     let orig = pixelAt lumaImg x y
         x_ = pixelAt lumaImg (min (x + 1) widthMax) y
         y_ = pixelAt lumaImg x (min (y + 1) heightMax)
     in fromIntegralP (orig - x_, orig - y_)
   -- List
   slopeMap = [ ((x, y), slope x y) | x <- [0 .. widthMax], y <- [0 .. heightMax] ]
  
   -- Type declaration
   distMax :: Double
   distMax = (sqrt . fromIntegral $ height ^ 2 + width ^ 2) / 2
   
   minLineLength :: Int
   minLineLength = 100
   
   maxLineGap :: Int
   maxLineGap = 10

   
   accBin = runST $
     do arr <- new ((0, 0), (thetaSize, distSize)) 0
        forM_ slopeMap $ \((x, y), gradient) -> do
            let (x_, y_) = fromIntegralP ((xCtr, yCtr) `subtract` (x, y))
            when (magnitude gradient > 127) $
              forM_ [0 .. thetaSize] $ \theta -> do
                let theta_ =
                      fromIntegral theta * 360 / fromIntegral thetaSize / 180 *
                      pi :: Double
                    distance = cos theta_ * x_ + sin theta_ * y_
                    distance_ = round  (distance * fromIntegral distSize )/ distMax
                    idx = (theta, distance_)
				-- optimization possible 
				-- minLineLength = 100 (pixels) and maxLineGap = 10 (pixels)
                when (distance_ >= 0 && distance_ < distSize) $
                  do old <- read arr idx
                     write arr idx (old + 1)
        return arr

     maxAcc = F.maximum accBin 
	   -- Generating function
	   hTransform x y =
       let l = 255 - round  ((accBin ! (x, y)) / maxAcc * 255)
       in PixelRGB8 l l l
     hImage = makeImage hTransform thetaSize distSize


houghIO :: FilePath -> FilePath -> Int -> Int -> IO ()
houghIO path outpath thetaSize distSize = do

[path, path'] <- getArgs
 eimg <- readImage path
 case eimg of
   Left err -> putStrLn ("Could not read image: " ++ err)
   Right (ImageRGB8 image_) -> doImage image_
   Right (ImageRGBA8 image_) -> doImage $ pixelMap dropTransparency image_
   _ -> putStrLn "Unexpected Pixel Format"
 where
   doImage image = do
     let houghImage = hough image thetaSize distSize
     writeImage outpath $ ImageRGB8 houghImage

{- ######### Helper functions #########
 transpose :: Array arr cs e => Image arr cs e -> Image arr cs e 
 index :: MArray arr cs e => Image arr cs e -> (Int, Int) -> Pixel cs e  -- Pixel at ith, jth
 dims :: BaseArray arr cs e => Image arr cs e -> (Int, Int)  -- get dimensions of image
 	>>> frog <- readImageRGB VU "images/frog.jpg"
	>>> frog
	<Image VectorUnboxed RGB (Double): 200x320>
	>>> dims frog
	(200,320)

 displayImage :: (Array VS cs e, Array arr cs e, Writable (Image VS cs e) TIF) => Image arr cs e -> IO ()
 writeImage :: (Array VS cs e, Array arr cs e, Writable (Image VS cs e) OutputFormat) => FilePath -> Image arr cs e	-> IO ()
 
 instance ToY RGB where
  toPixelY (PixelRGB r g b) = PixelY (0.299*r + 0.587*g + 0.114*b)
 
 toImageBinary :: (Array arr cs e, Array arr Binary Bit, Eq (Pixel cs e)) => Image arr cs e -> Image arr Binary Bit
  toImageBinary = I.map toPixelBinary

 makeImage :: Array arr cs e => (Int, Int) -> ((Int, Int) -> Pixel cs e) -> Image arr cs e  -- Given generating function and dimensions
 pixelAt :: Image a -> Int -> Int -> a 

-}


main :: IO ()
main = do 
 args <- getArgs
 prog <- getProgName
  case args of
   [path, outpath, thetaSize, distSize] ->
     houghIO path outpath (read thetaSize) (read distSize)
   _ ->
     putStrLn $
     "Usage: " ++ prog ++ " <image-file> <out-file.png> <width> <height>"


