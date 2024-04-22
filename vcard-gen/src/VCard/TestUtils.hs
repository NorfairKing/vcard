module VCard.TestUtils
  ( vcardScenarioDirRecur,
  )
where

import Control.Monad
import Path
import Test.Syd

vcardScenarioDirRecur :: FilePath -> (Path Rel File -> Spec) -> Spec
vcardScenarioDirRecur dir func =
  scenarioDirRecur dir $ \cardFile -> do
    relCardFile <- runIO $ parseRelFile cardFile
    let mExt = fileExtension relCardFile
    when (mExt == Just ".vcf" || mExt == Just ".vcard") $
      func relCardFile
