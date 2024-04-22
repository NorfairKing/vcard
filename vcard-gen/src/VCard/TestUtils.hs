module VCard.TestUtils
  ( vcardScenarioDirRecur,
  )
where

import Control.Monad
import Path
import Test.Syd
import VCard

vcardScenarioDirRecur :: FilePath -> (Path Rel File -> Spec) -> Spec
vcardScenarioDirRecur dir func =
  scenarioDirRecur dir $ \cardFile -> do
    relCardFile <- runIO $ parseRelFile cardFile
    let mExt = fileExtension relCardFile
    when (maybe False isVCardExtension mExt) $
      func relCardFile
