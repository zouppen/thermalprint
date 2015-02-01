-- |Load monochrome image. Supported formats: PNG and BMP.
module Loader (loadMonochrome) where

import Codec.Picture
import Codec.Picture.Types
import Data.Array.Unboxed
import Common

-- |Loads monochrome image from file to an array of booleans.
loadMonochrome :: FilePath -> IO MonochromeImage
loadMonochrome path = readImage path >>= toMonochrome

-- |Converts decodeImage output to array of booleans.
toMonochrome :: Monad m
             => Either String DynamicImage
             -> m MonochromeImage
toMonochrome img = case img of
  Left m -> fail $ "Unsupported image format: " ++ m
  Right (ImageY8 x) -> monochromatic x
  Right (ImageYA8 x) -> monochromatic x
  Right (ImageRGB8 x) -> monochromatic x
  Right (ImageRGBA8 x) -> monochromatic x
  Right (ImageYCbCr8 _) -> fail "YCbCr8 colorspace is not supported"

monochromatic img = do
  xs <- mapM isBlack $ range geometry
  return $ listArray geometry xs
  where
    geometry = ((0,0), (imageHeight img-1, imageWidth img-1))
    isBlack (y,x) = case promotePixel (pixelAt img x y) of
          PixelRGBA8 0 0 0 255 -> return True
          PixelRGBA8 255 255 255 255 -> return False
          _ -> fail $ "Pixel not monochrome at position " ++
               show (x,y)
