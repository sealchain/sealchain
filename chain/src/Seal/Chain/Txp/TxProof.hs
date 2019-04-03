{-# LANGUAGE RecordWildCards #-}

module Seal.Chain.Txp.TxProof
       ( TxProof (..)
       , mkTxProof
       ) where

import           Universum

import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Fmt (genericF)
import qualified Formatting.Buildable as Buildable

import           Seal.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Seal.Core.Merkle (MerkleRoot, mkMerkleTree, mtRoot)
import           Seal.Crypto (Hash, hash)

import           Seal.Chain.Txp.Tx
import           Seal.Chain.Txp.TxPayload
import           Seal.Chain.Txp.TxWitness

data TxProof = TxProof
    { txpNumber        :: !Word32
    , txpRoot          :: !(MerkleRoot Tx)
    , txpWitnessesHash :: !(Hash [TxWitness])
    } deriving (Show, Eq, Generic)

instance Buildable TxProof where
    build = genericF

instance Bi TxProof where
    encode proof =  encodeListLen 3
                 <> encode (txpNumber proof)
                 <> encode (txpRoot proof)
                 <> encode (txpWitnessesHash proof)
    decode = do
        enforceSize "TxProof" 3
        TxProof <$> decode <*>
                    decode <*>
                    decode

instance NFData TxProof

-- | Construct 'TxProof' which proves given 'TxPayload'.
-- This will construct a merkle tree, which can be very expensive. Use with
-- care. Bi constraints arise because we need to hash these things.
mkTxProof :: TxPayload -> TxProof
mkTxProof UnsafeTxPayload {..} =
    TxProof
    { txpNumber = fromIntegral (length _txpTxs)
    , txpRoot = mtRoot (mkMerkleTree _txpTxs)
    , txpWitnessesHash = hash _txpWitnesses
    }

deriveSafeCopySimple 0 'base ''TxProof
