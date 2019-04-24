{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-|
Module      : Hakyll.Images.Common
Description : Types and utilities for Hakyll.Images
Copyright   : (c) Laurent P René de Cotret, 2019
License     : BSD3
Maintainer  : laurent.decotret@outlook.com
Stability   : unstable
Portability : portable
-}

module Hakyll.Images.Common ( Image(..)
                            , ImageFormat(..)
                            , loadImage
                            , mapImage
                            , encode
                            , decode
                            ) where

import Prelude                          hiding (readFile)
import Control.Monad                    (liftM2)
import System.FilePath                  (takeExtension)

import Codec.Picture                    (decodeImage)
import Codec.Picture.Types              (DynamicImage)
import Codec.Picture.Saving

import Data.Binary                      (Binary(..))
import Data.ByteString.Lazy             (toStrict)
import Data.ByteString                  (ByteString)
import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)

import Hakyll.Core.Compiler             (Compiler, getResourceLBS)
import Hakyll.Core.Item                 (Item(..))
import Hakyll.Core.Writable             (Writable(..))

-- Supported (i.e. encodable) image formats
data ImageFormat
    = Jpeg 
    | Png
    | Bitmap
    | Tiff
    deriving (Eq, Generic)

-- Polymorphic type only to get an instance of functor.
-- Do not use this type.
data Image = Image { quality :: Int
                   , image :: DynamicImage
                   }
    deriving (Typeable)

-- When writing to disk, we deduce the image format.
instance Writable Image where
    write fp item = write fp (encode (fromExt . takeExtension $ fp) <$> item)

-- Binary instance looks similar to the binary instance for a Hakyll Item
instance Binary Image where
    put (Image quality' data') = put quality' >> (put . imageToTiff) data'
    get                        = Image <$> get <*> (decode <$> get)

-- | Load an image from a file.
-- This function can be combined with other compilers.
--
-- @
-- match "*.jpg" $ do
--     route idRoute
--     compile $ loadImage
--         >>= compressJpgCompiler 50
-- @
loadImage :: Compiler (Item Image)
loadImage = do
    content <- fmap (decode . toStrict) <$> getResourceLBS
    return $ (Image 100) <$> content

-- | Translation between file extensions and image formats.
-- It is important to keep track of image formats because Hakyll
-- compilers provides raw bytestrings and filenames
fromExt :: String -> ImageFormat
fromExt ".jpeg" = Jpeg
fromExt ".jpg"  = Jpeg
fromExt ".png"  = Png
fromExt ".bmp"  = Bitmap
fromExt ".tif"  = Tiff
fromExt ".tiff" = Tiff
fromExt ext     = error $ "Unsupported format: " <> ext

-- Encode images based on file extension
encode :: ImageFormat -> Image -> ByteString
encode Jpeg   = toStrict . liftM2 imageToJpg quality image
encode Png    = toStrict . imageToPng . image
encode Bitmap = toStrict . imageToBitmap . image
encode Tiff   = toStrict . imageToTiff . image

-- Decode images from generic data
decode :: ByteString -> DynamicImage
decode src = case decodeImage src of
    Left msg -> error msg
    Right img -> img

-- Map the image contents
mapImage :: (DynamicImage -> DynamicImage) -> Image -> Image
mapImage f (Image quality' data') = Image quality' $ f data'
