--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty                           (defaultMain, testGroup)


--------------------------------------------------------------------------------
import qualified Hakyll.Images.CompressJpg.Tests

import qualified Hakyll.Images.Rescale.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Hakyll"
    [ Hakyll.Images.CompressJpg.Tests.tests
    , Hakyll.Images.Rescale.Tests.tests 
    ]