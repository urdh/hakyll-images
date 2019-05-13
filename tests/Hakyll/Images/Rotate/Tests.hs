--------------------------------------------------------------------------------
module Hakyll.Images.Rotate.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (Assertion, assertEqual, testCase)


--------------------------------------------------------------------------------
import           Hakyll.Images
import qualified Data.ByteString        as B

import           Codec.Picture
import           Codec.Picture.Metadata.Exif

import           Text.Printf            (printf)

-- Original test image "piccolo.jpg" has shape 1170 x 647px
testJpg :: IO DynamicImage
testJpg = do
    img <- decodeJpeg <$> B.readFile "tests/data/piccolo.jpg"
    case img of
        Left _ -> error "Could not decode test picture piccolo.jpg"
        Right im -> return im

fromAssertions :: String       -- ^ Name
               -> [Assertion]  -- ^ Cases
               -> [TestTree]   -- ^ Result tests
fromAssertions name =
    zipWith testCase [printf "[%2d] %s" n name | n <- [1 :: Int ..]]

-- Test that not rotating does not affect the dimensions
testRotateIdentity :: Assertion
testRotateIdentity = do
    image <- testJpg
    let initialWidth  = (imageWidth  $ convertRGBA8 image)
        initialHeight = (imageHeight $ convertRGBA8 image)
        rotated       = convertRGBA8 $ rotateFromExif (Just (ExifShort 1)) image
        (width, height) = ( imageWidth rotated
                          , imageHeight rotated
                          )
    assertEqual "Image width was not resized properly"  width  initialWidth
    assertEqual "Image height was not resized properly" height initialHeight

-- Test that flipping does not affect the dimensions
testFlipHorizontally :: Assertion
testFlipHorizontally = do
    image <- testJpg
    let initialWidth  = (imageWidth  $ convertRGBA8 image)
        initialHeight = (imageHeight $ convertRGBA8 image)
        rotated       = convertRGBA8 $ rotateFromExif (Just (ExifShort 2)) image
        (width, height) = ( imageWidth rotated
                          , imageHeight rotated
                          )
    assertEqual "Image width was not resized properly"  width  initialWidth
    assertEqual "Image height was not resized properly" height initialHeight

-- Test that rotating does affect the dimensions
testRotate90Degrees :: Assertion
testRotate90Degrees = do
    image <- testJpg
    let initialWidth  = (imageWidth  $ convertRGBA8 image)
        initialHeight = (imageHeight $ convertRGBA8 image)
        rotated       = convertRGBA8 $ rotateFromExif (Just (ExifShort 8)) image
        (width, height) = ( imageWidth rotated
                          , imageHeight rotated
                          )
    assertEqual "Image width was not resized properly"  width  initialHeight
    assertEqual "Image height was not resized properly" height initialWidth


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Images.Rotate.Tests" $ concat
   [ fromAssertions "rotate"
        [ testRotateIdentity
        , testFlipHorizontally
        , testRotate90Degrees
        ]
    ]
