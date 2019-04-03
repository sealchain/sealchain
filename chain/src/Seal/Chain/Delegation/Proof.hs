module Seal.Chain.Delegation.Proof
       ( DlgProof
       , mkDlgProof
       ) where

import           Seal.Crypto (Hash, hash)

import           Seal.Chain.Delegation.Payload

-- | Proof of delegation payload.
type DlgProof = Hash DlgPayload

-- | Creates 'DlgProof' out of delegation payload.
mkDlgProof :: DlgPayload -> DlgProof
mkDlgProof = hash
