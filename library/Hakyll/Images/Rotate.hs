
{-|
Module      : Hakyll.Images.Rotate
Description : Hakyll compiler to rotate images
Copyright   : (c) Laurent P René de Cotret, 2019
License     : BSD3
Maintainer  : laurent.decotret@outlook.com
Stability   : unstable
Portability : portable

This module defines one Hakyll compilers, 'rotateFromExifCompiler', which reads EXIF data
from the image and rotates it to an upright position.
-}
module Hakyll.Images.Rotate
    ( rotateFromExif
    , rotateFromExifCompiler
    ) where

import Prelude                  hiding (lookup)

import Codec.Picture            (convertRGBA8, decodeImageWithMetadata)
import Codec.Picture.Types      (DynamicImage(..))
import Codec.Picture.Extra      (rotateLeft90, rotateRight90, rotate180,
                                 flipHorizontally, flipVertically)
import Codec.Picture.Metadata   (Metadatas, Keys(..), lookup)
import Codec.Picture.Metadata.Exif

import Data.ByteString.Lazy     (toStrict)
import Data.ByteString          (ByteString)
import Data.Word                (Word16)

import Hakyll.Core.Item         (Item(..))
import Hakyll.Core.Compiler     (Compiler, getResourceLBS)

import Hakyll.Images.Common     (Image(..), mapImage)

-- | Rotate an image based on its EXIF data, if any.
rotateFromExif :: Maybe ExifData -> DynamicImage -> DynamicImage
rotateFromExif Nothing               = id
rotateFromExif (Just (ExifShort o')) = rotate o'
rotateFromExif (Just _)              = id

-- | Compiler that rotated images based on their EXIF data.
--
-- @
-- match "*.png" $ do
--     route idRoute
--     compile $ loadImage
--         >>= rotateFromExifCompiler
-- @
rotateFromExifCompiler :: Item Image -> Compiler (Item Image)
rotateFromExifCompiler item = do
  orientation <- fmap (lookup (Exif TagOrientation) . decodeExif . toStrict) <$> getResourceLBS
  return $ mapImage (rotateFromExif . itemBody $ orientation) <$> item

-- Decode EXIF from generic data
decodeExif :: ByteString -> Metadatas
decodeExif src = case decodeImageWithMetadata src of
    Left msg -> error msg
    Right img -> snd img

-- Rotate image based on EXIF data
rotate :: Word16 -> (DynamicImage -> DynamicImage)
rotate 2 = ImageRGBA8 .                             flipHorizontally . convertRGBA8
rotate 3 = ImageRGBA8 . rotate180 .                                    convertRGBA8
rotate 4 = ImageRGBA8 .                             flipVertically   . convertRGBA8
rotate 5 = ImageRGBA8 . rotate180 . rotateRight90 . flipHorizontally . convertRGBA8
rotate 6 = ImageRGBA8 .             rotateRight90                    . convertRGBA8
rotate 7 = ImageRGBA8 .             rotateRight90 . flipHorizontally . convertRGBA8
rotate 8 = ImageRGBA8 . rotate180 . rotateRight90                    . convertRGBA8
rotate _ = id
