module Spec
    ( spec
    ) where

import           Prelude
import           Test.Hspec
import qualified Test.Seal.Infra.Diffusion.Subscription.StatusSpec (spec)
import qualified Test.Seal.Infra.Diffusion.Subscription.SubscriptionSpec (spec)

spec :: Spec
spec = describe "Subscription" $ do
    Test.Seal.Infra.Diffusion.Subscription.StatusSpec.spec
    Test.Seal.Infra.Diffusion.Subscription.SubscriptionSpec.spec
