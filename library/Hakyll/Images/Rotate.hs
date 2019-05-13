
-- |
-- Module      : Hakyll.Images.Rotate
-- Description : Hakyll compiler to rotate images
-- Copyright   : (c) Simon Sigurdhsson, 2025
-- License     : BSD3
-- Maintainer  : Sigurdhsson@gmail.com
-- Stability   : unstable
-- Portability : portable
--
-- This module defines one Hakyll compiler, 'rotateFromExifCompiler', which reads EXIF data
-- from the image and rotates it to an upright position.
--
-- @
--     import Hakyll
--     import Hakyll.Images        ( loadImage
--                                 , rotateFromExifCompiler
--                                 )
--
--     hakyll $ do
--
--         -- Rotate images based on their EXIF
--         match "images/**" $ do
--             route idRoute
--             compile $ loadImage >>= rotateFromExifCompiler 600 400
--
--         (... omitted ...)
-- @
module Hakyll.Images.Rotate
  ( rotateFromExif,
    rotateFromExifCompiler,
  )
where

import Prelude hiding (lookup)
import Codec.Picture (convertRGBA8)
import Codec.Picture.Extra (rotateRight90, rotateLeft90, rotate180, flipHorizontally, flipVertically)
import Codec.Picture.Metadata (Keys(..), lookup, delete)
import Codec.Picture.Metadata.Exif
import qualified Codec.Picture.Types as T
import Data.Word (Word16)
import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.Item (Item(..))
import Hakyll.Images.Common (Image(..), ImageContent, WithMetadata (..), encode, withImageContent)

-- | Rotate an image based on its EXIF data, if any.
rotateFromExif :: ImageContent -> ImageContent
rotateFromExif (MkWithMetadata img meta) =
    MkWithMetadata (rotate' orientation img) (delete (Exif TagOrientation) meta)
  where
    orientation = lookup (Exif TagOrientation) meta
    rotate' :: Maybe ExifData -> (T.DynamicImage -> T.DynamicImage)
    rotate' Nothing               = id
    rotate' (Just (ExifShort o')) = T.ImageRGBA8 . rotate o' . convertRGBA8
    rotate' (Just _)              = id

-- | Compiler that rotates images based on their EXIF data.
--
-- @
-- match "*.png" $ do
--     route idRoute
--     compile $ loadImage
--         >>= rotateFromExifCompiler
-- @
--
-- Note that in the rotating process, images will be converted to RGBA8.
rotateFromExifCompiler :: Item Image -> Compiler (Item Image)
rotateFromExifCompiler = return . fmap (withImageContent rotateFromExif encode)

-- Rotate image based on EXIF data
rotate :: T.Pixel a => Word16 -> (T.Image a -> T.Image a)
rotate 2 =                 flipHorizontally
rotate 3 = rotate180
rotate 4 =                 flipVertically
rotate 5 = rotateLeft90  . flipHorizontally
rotate 6 = rotateRight90
rotate 7 = rotateRight90 . flipHorizontally
rotate 8 = rotateLeft90
rotate _ = id
