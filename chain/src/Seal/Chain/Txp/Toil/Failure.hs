{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards #-}

-- | Toil failures.

module Seal.Chain.Txp.Toil.Failure
       ( ToilVerFailure (..)
       , WitnessVerFailure (..)
       , TxOutVerFailure (..)
       ) where

import           Universum

import           Formatting (bprint, build, int, ords, shown, stext, (%))
import qualified Formatting.Buildable
import           GHC.TypeLits (TypeError)
import           Serokell.Data.Memory.Units (Byte, memory)
import           Serokell.Util (listJson)

import           Seal.Chain.Block.Header (HeaderHash)
import           Seal.Chain.Txp.Toil.Types (TxFee)
import           Seal.Chain.Txp.Tx (TxIn, TxOut (..), AccountIn (..))
import           Seal.Chain.Txp.TxWitness (TxInWitness)
import           Seal.Core (Address, TxFeePolicy, AccountState (..),
                     Account (..), addressDetailedF, addressF)
import           Seal.Core.Attributes (UnparsedFields)
import           Seal.Util (DisallowException)

----------------------------------------------------------------------------
-- ToilVerFailure
----------------------------------------------------------------------------

-- | Result of transaction processing
data ToilVerFailure
    = ToilKnown -- ^ Transaction is already in the storage (cache)
    -- | ToilTipsMismatch oldTip newTip
    | ToilTipsMismatch !HeaderHash !HeaderHash
    | ToilSlotUnknown
    | ToilOverwhelmed !Int -- ^ Local transaction storage is full --
                            -- can't accept more txs. Current limit is attached.
    | ToilNotUnspent !TxIn -- ^ Tx input is not a known unspent input.
    -- | ToilOutGreaterThanIn inputSum outputSum
    | ToilOutGreaterThanIn !Integer !Integer
    | ToilInconsistentTxAux !Text
    | ToilInvalidOutput !Word32 !TxOutVerFailure
    | ToilUnknownInput !Word32 !TxIn
    -- | The witness can't be used to justify spending an output – either
    --     * it has a wrong type, e.g. PKWitness for a script address, or
    --     * it has the right type but doesn't match the address, e.g. the
    --       hash of key in PKWitness is not equal to the address.
    | ToilWitnessDoesntMatchUtxo !Word32 !TxIn !Address !TxInWitness
    -- | The witness could in theory justify spending an output, but it
    -- simply isn't valid (the signature doesn't pass validation, the
    -- validator–redeemer pair produces 'False' when executed, etc).
    | ToilInvalidWitness !Word32 !TxInWitness !WitnessVerFailure
    -- | ToilTooLargeTx acutalSize limit
    | ToilTooLargeTx !Byte !Byte
    | ToilInvalidMinFee !TxFeePolicy !Text !Byte
    -- | ToilInsufficientFee policy actualFee minFee size
    | ToilInsufficientFee !TxFeePolicy !TxFee !TxFee !Byte
    | ToilUnknownAttributes !UnparsedFields
    | ToilNonBootstrapDistr !(NonEmpty Address)
    | ToilRepeatedInput
    | ToilEmptyAfterFilter
    | ToilMissingSealCoins -- when tx does not contains seal coin within inputs
    | ToilLockedInput !TxIn !TxOut
    | ToilInvalidAccountIn !AccountIn !(Maybe AccountState)  -- when account input don't have enough balance.
    | ToilPactEvalFailure !String -- ^ Error happens while evaling cmds
    | ToilWitnessDoesntMatchAcc !Word32 !AccountIn !Account !TxInWitness
    | ToilUnexpectedWitnessType
    deriving (Show, Eq)

instance TypeError (DisallowException ToilVerFailure) =>
         Exception ToilVerFailure

instance Buildable ToilVerFailure where
    build ToilKnown =
        "transaction already is in the mem pool"
    build (ToilTipsMismatch dbTip localTip) =
        bprint ("Something is bad with this node, tips mismatch, "%
                "tip from DB is "%build%", local tip is "%build)
        dbTip localTip
    build ToilSlotUnknown =
        "can't process, current slot is unknown"
    build (ToilOverwhelmed limit) =
        bprint ("max size of the mem pool is reached which is "%shown) limit
    build (ToilNotUnspent txId) =
        bprint ("input is not a known unspent input: "%build) txId
    build (ToilOutGreaterThanIn tInputSum tOutputSum) =
        bprint ("sum of outputs is greater than sum of inputs ("%int%" < "%int%")")
        tInputSum tOutputSum
    build (ToilInconsistentTxAux msg) =
        bprint ("TxAux is inconsistent: "%stext) msg
    build (ToilInvalidOutput n reason) =
        bprint (ords%" output is invalid:\n'"%
                " reason: "%build)
            n reason
    build (ToilWitnessDoesntMatchUtxo i txIn addr witness) =
        bprint ("input #"%int%"'s witness doesn't match address "%
                "of corresponding output:\n"%
                "  input: "%build%"\n"%
                "  address details: "%addressDetailedF%"\n"%
                "  witness: "%build)
            i txIn addr witness
    build (ToilInvalidWitness i witness reason) =
        bprint ("input #"%int%"'s witness doesn't pass verification:\n"%
                "  witness: "%build%"\n"%
                "  reason: "%build)
            i witness reason
    build (ToilTooLargeTx ttltSize ttltLimit) =
        bprint ("transaction's size exceeds limit "%
                "("%memory%" > "%memory%")") ttltSize ttltLimit
    build (ToilInvalidMinFee timfPolicy timfReason timfSize) =
        bprint (build%" generates invalid minimal fee on a "%
                "transaction of size "%memory%", reason: "%stext)
            timfPolicy
            timfSize
            timfReason
    build (ToilInsufficientFee tifPolicy tifFee tifMinFee tifSize) =
        bprint ("transaction of size "%memory%" does not adhere to "%
                build%"; it has fee "%build%" but needs "%build)
            tifSize
            tifPolicy
            tifFee
            tifMinFee
    build (ToilUnknownAttributes uf) =
        bprint ("transaction has unknown attributes: "%shown) uf
    build (ToilNonBootstrapDistr addresses) =
        bprint ("we are in bootstrap era, but some addresses have distribution"%
                " which is not 'BootstrapEraDistr': "%listJson) addresses
    build ToilRepeatedInput =
        "transaction tries to spend an unspent input more than once"
    build (ToilUnknownInput inpId txIn) =
       bprint ("vtcVerifyAllIsKnown is True, but the input #"%int%" "%build%" is unknown") inpId txIn

    build ToilEmptyAfterFilter =
       "transaction list is empty after filtering out asset-locked source addresses"

    build ToilMissingSealCoins =
        "missing seal coins in inputs"
    build (ToilLockedInput txIn txOut) =
        bprint ( "input is locked:\n"%
                "  input: "%build%"\n"%
                "  output spent by this input: "%build%"\n") txIn txOut
    build (ToilInvalidAccountIn ai as) =
        bprint ("account redeem input invalid, account input: "%build%", account state:"%build) ai as
    build (ToilPactEvalFailure msg) =
        bprint ("error happens while evaling commands, error message: "%build) msg
    build (ToilWitnessDoesntMatchAcc i acctIn Account{..} witness) =
        bprint ("input #"%int%"'s witness doesn't match account "%
                "of corresponding output:\n"%
                "  input: "%build%"\n"%
                "  address details: "%addressDetailedF%"\n"%
                "  witness: "%build)
            i acctIn getAccount witness
    build ToilUnexpectedWitnessType =
        "Unexpected witness type"


----------------------------------------------------------------------------
-- WitnessVerFailure
----------------------------------------------------------------------------

-- | Result of checking a witness.
data WitnessVerFailure
    -- | The signature of a 'PKWitness' doesn't pass validation
    = WitnessWrongSignature
    -- | Don't know how to handle this witness type
    | WitnessUnknownType Word8
    deriving (Show, Eq, Generic, NFData)

instance Buildable WitnessVerFailure where
    build WitnessWrongSignature =
        bprint "the signature in the witness doesn't pass validation"
    build (WitnessUnknownType t) =
        bprint ("unknown witness type: "%build) t

----------------------------------------------------------------------------
-- TxOutVerFailure
----------------------------------------------------------------------------

-- | Result of checking transaction output.
data TxOutVerFailure
    -- | Not all attributes for the output are known
    = TxOutUnknownAttributes Address
    -- | Can't send to an address with unknown type
    | TxOutUnknownAddressType Address
    -- | Can't send to a redeem address
    | TxOutRedeemAddressProhibited Address
    -- | NetworkMagic's must match
    | TxOutAddressBadNetworkMagic Address
    -- | Wrong address type of address in account
    | TxOutAccountAddressType Address
    | TxOutAccountAddrCategory Address
    deriving (Show, Eq, Generic, NFData)

instance Buildable TxOutVerFailure where
    build (TxOutUnknownAttributes addr) =
        bprint ("address "%addressF%" has unknown attributes") addr
    build (TxOutUnknownAddressType addr) =
        bprint ("sends money to an addresss with unknown type ("
                %addressF%"), this is prohibited") addr
    build (TxOutRedeemAddressProhibited addr) =
        bprint ("sends money to a redeem address ("
                %addressF%"), this is prohibited") addr
    build (TxOutAddressBadNetworkMagic addr) =
        bprint ("sends money to an address with mismatched \
                \NetworkMagic ("%addressF%"), this is prohibited")
               addr
    build (TxOutAccountAddressType addr) =
        bprint ("sends money to an account who's address is not ATPubKey type ("
                %addressF%")") addr
    build (TxOutAccountAddrCategory addr) =
        bprint ("sends money to an account who's address category \
                \is not ACUserAccount type (" %addressF%")") addr
