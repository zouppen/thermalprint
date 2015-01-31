module Loader where

import Codec.Picture
import Codec.Picture.Types
import qualified Data.ByteString as B

toMonochrome :: Monad m => Either String DynamicImage -> m [[Bool]]
toMonochrome img = case img of
  Left m -> fail $ "Unsupported image format: " ++ m
  Right (ImageY8 x) -> monochromatic x
  Right (ImageYA8 x) -> monochromatic x
  Right (ImageRGB8 x) -> monochromatic x
  Right (ImageRGBA8 x) -> monochromatic x
  Right (ImageYCbCr8 _) -> fail "YCbCr8 colorspace is not supported"

monochromatic img = mapM row [0..imageHeight img - 1]
  where
    row y = mapM (isBlack y) [0..imageWidth img - 1]
    isBlack y x = case promotePixel (pixelAt img x y) of
          PixelRGBA8 0 0 0 255 -> return False
          PixelRGBA8 255 255 255 255 -> return True
          _ -> fail $ "Pixel not monochrome at position " ++
               show (x,y)
