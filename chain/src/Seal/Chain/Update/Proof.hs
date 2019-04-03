module Seal.Chain.Update.Proof
       ( UpdateProof
       , mkUpdateProof
       ) where

import           Seal.Crypto (Hash, hash)

import           Seal.Chain.Update.Payload

-- | Proof that body of update message contains 'UpdatePayload'.
type UpdateProof = Hash UpdatePayload

mkUpdateProof :: UpdatePayload -> UpdateProof
mkUpdateProof = hash
