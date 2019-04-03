-- | Re-exports of SSC modules.
--
-- We implement SSC as a coin tossing protocol with guaranteed output
-- delivery. Nodes exchange commitments, openings, and shares, and in the
-- end arrive at a shared seed.
--
-- See https://eprint.iacr.org/2016/889.pdf (“A Provably Secure
-- Proof-of-Stake Blockchain Protocol”), section 4 for more details.

module Seal.Chain.Ssc
       ( module X
       ) where

import           Seal.Chain.Ssc.Base as X
import           Seal.Chain.Ssc.Behavior as X
import           Seal.Chain.Ssc.Commitment as X
import           Seal.Chain.Ssc.CommitmentAndOpening as X
import           Seal.Chain.Ssc.CommitmentsMap as X
import           Seal.Chain.Ssc.Configuration as X
import           Seal.Chain.Ssc.Error as X
import           Seal.Chain.Ssc.Functions as X
import           Seal.Chain.Ssc.Mem as X
import           Seal.Chain.Ssc.Message as X
import           Seal.Chain.Ssc.Opening as X
import           Seal.Chain.Ssc.OpeningsMap as X
import           Seal.Chain.Ssc.Payload as X
import           Seal.Chain.Ssc.Proof as X
import           Seal.Chain.Ssc.Seed as X
import           Seal.Chain.Ssc.Shares as X (getOurShares)
import           Seal.Chain.Ssc.SharesDistribution as X
import           Seal.Chain.Ssc.SharesMap as X
import           Seal.Chain.Ssc.Toss as X
import           Seal.Chain.Ssc.Types as X
import           Seal.Chain.Ssc.VssCertData as X
import           Seal.Chain.Ssc.VssCertificate as X
import           Seal.Chain.Ssc.VssCertificatesMap as X
