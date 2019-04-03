module Seal.Core.Metrics.Constants (
      sealNamespace
    , withSealNamespace
                                        ) where

import           Universum

sealNamespace :: Text
sealNamespace = "seal"

withSealNamespace :: Text -> Text
withSealNamespace label = sealNamespace <> "." <> label
