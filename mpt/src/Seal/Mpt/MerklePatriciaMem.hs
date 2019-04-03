
module Seal.Mpt.MerklePatriciaMem 
 ( putKeyValMem
 , getKeyValMem
 , deleteKeyMem
 , keyExistsMem
 , initializeBlankMem
 , MPMem (..)
  ) where

import           Universum hiding (head)
import           Data.List hiding (null)
import           Data.ByteArray (convert)
import           Crypto.Hash as Crypto
import qualified Data.Map as Map
import           Data.Maybe (isJust)

import           Seal.Binary.Class (serialize')
import           Seal.Mpt.MerklePatricia.InternalMem
import           Seal.Mpt.MerklePatricia.StateRoot

putKeyValMem :: (Monad m) => MPMem
            -> MPKey
            -> MPVal
            -> m MPMem
putKeyValMem db = unsafePutKeyValMem db . keyToSafeKeyMem


getKeyValMem :: (Monad m) => MPMem
         -> MPKey
         -> m (Maybe MPVal)
getKeyValMem db key = do
  vals <- unsafeGetKeyValsMem db (keyToSafeKeyMem key)
  return $
    if not (null vals)
    then Just $ snd (head vals)
         -- Since we hash the keys, it's impossible
         -- for vals to have more than one item
    else Nothing

deleteKeyMem :: (Monad m) => MPMem
             -> MPKey
             -> m MPMem
deleteKeyMem db = unsafeDeleteKeyMem db . keyToSafeKeyMem

keyExistsMem :: (Monad m) => MPMem
         -> MPKey
         -> m Bool
keyExistsMem db key = isJust <$> getKeyValMem db key


initializeBlankMem :: MPMem
initializeBlankMem =
    let bytes = serialize' (0::Integer)
        key = convert $ (Crypto.hash bytes :: Crypto.Digest Crypto.Keccak_256)
    in
      MPMem {
        mpMap = Map.insert key bytes Map.empty,
        mpStateRoot = StateRoot bytes
      }


