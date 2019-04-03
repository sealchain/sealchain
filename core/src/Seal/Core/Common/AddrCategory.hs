module Seal.Core.Common.AddrCategory
       ( AddrCategory (..)
       ) where

import           Universum hiding (id)

import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Formatting.Buildable as Buildable
import           Formatting (bprint, (%), int)

import           Seal.Binary.Class (Bi, decode, encode)

-- | Stake distribution associated with an address.
data AddrCategory
    = ACMobile
    | ACUserAccount
    | ACContractAccount
    | ACUnknown !Word8
    deriving (Eq, Ord, Show, Generic, Typeable)

instance Buildable AddrCategory where
    build =
        \case
            ACMobile -> "Mobile category"
            ACUserAccount -> "User account category"
            ACContractAccount -> "Contract account category"
            ACUnknown tag ->
                bprint ("Unknown address category ("%int%")") tag

instance NFData AddrCategory

instance Bi AddrCategory where
    encode =
        encode @Word8 . \case
            ACMobile          -> 1
            ACUserAccount     -> 2
            ACContractAccount -> 3
            ACUnknown tag     -> tag
    decode =
        decode @Word8 <&> \case
            1   -> ACMobile
            2   -> ACUserAccount
            3   -> ACContractAccount
            tag -> ACUnknown tag

deriveSafeCopySimple 0 'base ''AddrCategory