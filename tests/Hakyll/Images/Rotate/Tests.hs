{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Images.Rotate.Tests
  ( tests,
  )
where

import Prelude hiding (lookup)
import Codec.Picture
  ( convertRGBA8,
    imageHeight,
    imageWidth,
  )
import Codec.Picture.Metadata.Exif
import qualified Data.ByteString as B
import Hakyll.Images
import Hakyll.Images.Internal (ImageContent, WithMetadata (..), decodeContent)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Text.Printf (printf)

-- Original test image "piccolo.jpg" has shape 1170 x 647px
testJpg :: IO ImageContent
testJpg = decodeContent <$> B.readFile "tests/data/piccolo.jpg"

-- | Map over the metadata of an `Image`, decoded into an `ImageContent`.
withMetadatas :: (Metadatas -> Metadatas) -> ImageContent -> ImageContent
withMetadatas f (MkWithMetadata img meta) = MkWithMetadata img (f meta)

fromAssertions ::
  -- | Name
  String ->
  -- | Cases
  [Assertion] ->
  -- | Result tests
  [TestTree]
fromAssertions name =
  zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

-- Test that not rotating does not affect the dimensions
testRotateIdentity :: Assertion
testRotateIdentity = do
  image <- testJpg
  let initialWidth  = imageWidth  $ (convertRGBA8 . getData) image
      initialHeight = imageHeight $ (convertRGBA8 . getData) image
      rotated       = rotateFromExif (withMetadatas (insert (Exif TagOrientation) (ExifShort 1)) image)
      MkWithMetadata converted meta = fmap convertRGBA8 rotated
      (width, height) =
        ( imageWidth converted,
          imageHeight converted
        )
  assertEqual "Image width was not rotated properly"  width  initialWidth
  assertEqual "Image height was not rotated properly" height initialHeight
  assertEqual "Image orientation was not stripped" (lookup (Exif TagOrientation) meta) Nothing

-- Test that flipping does not affect the dimensions
testFlipHorizontally :: Assertion
testFlipHorizontally = do
  image <- testJpg
  let initialWidth  = imageWidth  $ (convertRGBA8 . getData) image
      initialHeight = imageHeight $ (convertRGBA8 . getData) image
      rotated       = rotateFromExif (withMetadatas (insert (Exif TagOrientation) (ExifShort 2)) image)
      MkWithMetadata converted meta = fmap convertRGBA8 rotated
      (width, height) =
        ( imageWidth converted,
          imageHeight converted
        )
  assertEqual "Image width was not rotated properly"  width  initialWidth
  assertEqual "Image height was not rotated properly" height initialHeight
  assertEqual "Image orientation was not stripped" (lookup (Exif TagOrientation) meta) Nothing

-- Test that rotating does affect the dimensions
testRotate90Degrees :: Assertion
testRotate90Degrees = do
  image <- testJpg
  let initialWidth  = imageWidth  $ (convertRGBA8 . getData) image
      initialHeight = imageHeight $ (convertRGBA8 . getData) image
      rotated       = rotateFromExif (withMetadatas (insert (Exif TagOrientation) (ExifShort 8)) image)
      MkWithMetadata converted meta = fmap convertRGBA8 rotated
      (width, height) =
        ( imageWidth converted,
          imageHeight converted
        )
  assertEqual "Image width was not rotated properly"  width  initialHeight
  assertEqual "Image height was not rotated properly" height initialWidth
  assertEqual "Image orientation was not stripped" (lookup (Exif TagOrientation) meta) Nothing


--------------------------------------------------------------------------------
tests :: TestTree
tests =
  testGroup "Hakyll.Images.Rotate.Tests" $
    fromAssertions "rotate"
      [ testRotateIdentity,
        testFlipHorizontally,
        testRotate90Degrees
      ]
