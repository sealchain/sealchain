{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types used for pure transaction processing (aka Toil).

module Seal.Chain.Txp.Toil.Types
       ( AccountModifier
       , Utxo
       , UtxoLookup
       , UtxoModifier
       , formatUtxo
       , utxoF
       , utxoToModifier
       , utxoToLookup
       , CurrencyBalances (..)
       , applyUtxoModToCurrencyBalances
       , sealUtxo
       , goldUtxo
       , dollarUtxo
       , currencyUtxo
       , sealUtxoModifier
       , goldUtxoModifier
       , dollarUtxoModifier
       , getGoldCoinStateOnly
       , getGoldDollarStateOnly
       , utxoToAddrBalanceMaps

       , StakesView (..)
       , svStakes
       , svTotal

       , TxFee(..)
       , MemPool (..)
       , mpLocalTxs
       , mpSize
       , TxMap
       , UndoMap
       , AddrCoinMap
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Data.Default (Default, def)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M (lookup, member, toList, filter, elems)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (Format, later)
import           Serokell.Util.Text (mapBuilderJson)

import           Seal.Chain.Txp.Tx (TxId, TxIn, TxOut (..),
                     isSealTxOut, isGoldTxOut, isDollarTxOut, isCurrencyTxOut,
                     isGoldCoinStateTxOut, isGoldDollarStateTxOut)
import           Seal.Chain.Txp.TxAux (TxAux)
import           Seal.Chain.Txp.TxOutAux (TxOutAux (..))
import           Seal.Chain.Txp.Undo (TxUndo)
import           Seal.Core (Address, Coin, StakeholderId,
                     AccountModifier, Currency (..), GoldCoin, GoldDollar)
import qualified Seal.Util.Modifier as MM

----------------------------------------------------------------------------
-- UTXO
----------------------------------------------------------------------------

-- | Unspent transaction outputs.
--
-- Transaction inputs are identified by (transaction ID, index in list of
-- output) pairs.
type Utxo = Map TxIn TxOutAux

-- | Type of function to look up an entry in 'Utxo'.
type UtxoLookup = TxIn -> Maybe TxOutAux

-- | All modifications (additions and deletions) to be applied to 'Utxo'.
type UtxoModifier = MM.MapModifier TxIn TxOutAux

-- | Format 'Utxo' map for showing
formatUtxo :: Utxo -> Builder
formatUtxo = mapBuilderJson . M.toList

-- | Specialized formatter for 'Utxo'.
utxoF :: Format r (Utxo -> r)
utxoF = later formatUtxo

utxoToModifier :: Utxo -> UtxoModifier
utxoToModifier = foldl' (flip $ uncurry MM.insert) mempty . M.toList

utxoToLookup :: Utxo -> UtxoLookup
utxoToLookup = flip M.lookup

----------------------------------------------------------------------------
-- Fee
----------------------------------------------------------------------------

-- | tx.fee = sum(tx.in) - sum (tx.out)
newtype TxFee = TxFee Coin
    deriving (Show, Eq, Ord, Generic, Buildable)

----------------------------------------------------------------------------
-- StakesView
----------------------------------------------------------------------------

data StakesView = StakesView
    { _svStakes :: !(HashMap StakeholderId Coin)
    , _svTotal  :: !(Maybe Coin)
    }

makeLenses ''StakesView

instance Default StakesView where
    def = StakesView mempty Nothing

----------------------------------------------------------------------------
-- MemPool
----------------------------------------------------------------------------

type TxMap = HashMap TxId TxAux

data MemPool = MemPool
    { _mpLocalTxs :: !TxMap
      -- | Number of transactions in the memory pool.
    , _mpSize     :: !Int
    }

makeLenses ''MemPool

instance Default MemPool where
    def =
        MemPool
        { _mpLocalTxs = mempty
        , _mpSize     = 0
        }

----------------------------------------------------------------------------
-- UndoMap and AddrCoinsMap
----------------------------------------------------------------------------

type UndoMap = HashMap TxId TxUndo
type AddrCoinMap = HashMap Address Coin

-- | Takes utxo modifier and address-coin map with correspodning utxo
-- and applies utxo modifier to map.
-- Works for O(size of modifier * log (size of map)).
applyUtxoModToAddrCoinMap
    :: UtxoModifier
    -> (AddrCoinMap, Utxo)
    -> AddrCoinMap
applyUtxoModToAddrCoinMap modifier (addrCoins, utxo) = result
  where
    outToPair :: TxOutAux -> (Address, Coin)
    outToPair TxOutAux{..} = (txOutAddress toaOut, txOutSeal toaOut)

    -- Resolve TxOut for every TxIn and convert TxOuts
    -- to pairs (Address, Coin)
    resolvedAddrs :: [(Address, Coin)]
    resolvedAddrs =
        mapMaybe (fmap outToPair . flip M.lookup utxo)
                 (MM.deletions modifier)

    -- subAddress and updateHM are used to do
    -- hashMap[address] = hashMap[address] - coins
    subAddress :: Coin -> Coin -> Maybe Coin
    subAddress r c = if r < c then Just (c `unsafeSubMoney` r) else Nothing

    updateHM :: HashMap Address Coin -> (Address, Coin) -> HashMap Address Coin
    updateHM hm (ad, coins) = HM.update (subAddress coins) ad hm

    -- Substract coins from current balances
    addrCoinsRest :: HashMap Address Coin
    addrCoinsRest = foldl' updateHM addrCoins resolvedAddrs

    -- Remove such TxIns which are already in wallet utxo.
    insertionsNotInUtxo :: [(TxIn, TxOutAux)]
    insertionsNotInUtxo = filter (not . flip M.member utxo . fst) (MM.insertions modifier)

    -- Convert TxOuts of insertionsNotInUtxo to [(Address, Coin)]
    addrCoinsAdditions :: [(Address, Coin)]
    addrCoinsAdditions = map (outToPair . snd) insertionsNotInUtxo

    -- Add coins to balances
    result :: HashMap Address Coin
    result = foldl' (flip $ uncurry $ HM.insertWith unsafeAddMoney) addrCoinsRest addrCoinsAdditions

----------------------------------------------------------------------------
-- AddrGoldCoinMap and AddrGoldDollarMap
----------------------------------------------------------------------------

type AddrGoldCoinMap = HashMap Address GoldCoin
type AddrGoldDollarMap = HashMap Address GoldDollar

data CurrencyBalances = CurrencyBalances
    { cbSeals   :: AddrCoinMap
    , cbGolds   :: AddrGoldCoinMap
    , cbDollars :: AddrGoldDollarMap
    } deriving (Eq)

applyUtxoModToAddrGoldCoinMap
    :: UtxoModifier
    -> (AddrGoldCoinMap, Utxo)
    -> AddrGoldCoinMap
applyUtxoModToAddrGoldCoinMap modifier (addrGolds, utxo) = result
  where
    outToPair :: TxOutAux -> (Address, GoldCoin)
    outToPair TxOutAux{..} = (txOutAddress toaOut, txOutGold toaOut)

    -- Resolve TxOut for every TxIn and convert TxOuts
    -- to pairs (Address, GoldCoin)
    resolvedAddrs :: [(Address, GoldCoin)]
    resolvedAddrs =
        mapMaybe (fmap outToPair . flip M.lookup utxo)
                 (MM.deletions modifier)

    -- subAddress and updateHM are used to do
    -- hashMap[address] = hashMap[address] - coins
    subAddress :: GoldCoin -> GoldCoin -> Maybe GoldCoin
    subAddress r c = if r < c then Just (c `unsafeSubMoney` r) else Nothing

    updateHM :: HashMap Address GoldCoin -> (Address, GoldCoin) -> HashMap Address GoldCoin
    updateHM hm (ad, golds) = HM.update (subAddress golds) ad hm

    -- Substract coins from current balances
    addrGoldCoinsRest :: HashMap Address GoldCoin
    addrGoldCoinsRest = foldl' updateHM addrGolds resolvedAddrs

    -- Remove such TxIns which are already in wallet utxo.
    insertionsNotInUtxo :: [(TxIn, TxOutAux)]
    insertionsNotInUtxo = filter (not . flip M.member utxo . fst) (MM.insertions modifier)

    -- Convert TxOuts of insertionsNotInUtxo to [(Address, GoldCoin)]
    addrGoldCoinsAdditions :: [(Address, GoldCoin)]
    addrGoldCoinsAdditions = map (outToPair . snd) insertionsNotInUtxo

    -- Add coins to balances
    result :: HashMap Address GoldCoin
    result = foldl' (flip $ uncurry $ HM.insertWith unsafeAddMoney) addrGoldCoinsRest addrGoldCoinsAdditions

applyUtxoModToAddrGoldDollarMap
    :: UtxoModifier
    -> (AddrGoldDollarMap, Utxo)
    -> AddrGoldDollarMap
applyUtxoModToAddrGoldDollarMap modifier (addrDollars, utxo) = result
  where
    outToPair :: TxOutAux -> (Address, GoldDollar)
    outToPair TxOutAux{..} = (txOutAddress toaOut, txOutDollar toaOut)

    -- Resolve TxOut for every TxIn and convert TxOuts
    -- to pairs (Address, GoldDollar)
    resolvedAddrs :: [(Address, GoldDollar)]
    resolvedAddrs =
        mapMaybe (fmap outToPair . flip M.lookup utxo)
                 (MM.deletions modifier)

    -- subAddress and updateHM are used to do
    -- hashMap[address] = hashMap[address] - coins
    subAddress :: GoldDollar -> GoldDollar -> Maybe GoldDollar
    subAddress r c = if r < c then Just (c `unsafeSubMoney` r) else Nothing

    updateHM :: HashMap Address GoldDollar -> (Address, GoldDollar) -> HashMap Address GoldDollar
    updateHM hm (ad, golds) = HM.update (subAddress golds) ad hm

    -- Substract coins from current balances
    addrCoinsRest :: HashMap Address GoldDollar
    addrCoinsRest = foldl' updateHM addrDollars resolvedAddrs

    -- Remove such TxIns which are already in wallet utxo.
    insertionsNotInUtxo :: [(TxIn, TxOutAux)]
    insertionsNotInUtxo = filter (not . flip M.member utxo . fst) (MM.insertions modifier)

    -- Convert TxOuts of insertionsNotInUtxo to [(Address, GoldDollar)]
    addrCoinsAdditions :: [(Address, GoldDollar)]
    addrCoinsAdditions = map (outToPair . snd) insertionsNotInUtxo

    -- Add coins to balances
    result :: HashMap Address GoldDollar
    result = foldl' (flip $ uncurry $ HM.insertWith unsafeAddMoney) addrCoinsRest addrCoinsAdditions

applyUtxoModToCurrencyBalances
    :: UtxoModifier
    -> (CurrencyBalances, Utxo)
    -> CurrencyBalances
applyUtxoModToCurrencyBalances modifier (CurrencyBalances{..}, utxo) = 
        CurrencyBalances sealBals goldBals dollarBals
    where
        sealBals = applyUtxoModToAddrCoinMap (sealUtxoModifier modifier) (cbSeals, sealUtxo utxo)
        goldBals = applyUtxoModToAddrGoldCoinMap (goldUtxoModifier modifier) (cbGolds, goldUtxo utxo)
        dollarBals = applyUtxoModToAddrGoldDollarMap (dollarUtxoModifier modifier) (cbDollars, dollarUtxo utxo)

sealUtxo :: Utxo -> Utxo
sealUtxo = M.filter (isSealTxOut . toaOut)

goldUtxo :: Utxo -> Utxo
goldUtxo = M.filter (isGoldTxOut . toaOut)

dollarUtxo :: Utxo -> Utxo
dollarUtxo = M.filter (isDollarTxOut . toaOut)

currencyUtxo :: Utxo -> Utxo
currencyUtxo = M.filter (isCurrencyTxOut . toaOut)

sealUtxoModifier :: UtxoModifier -> UtxoModifier
sealUtxoModifier utxoModifier = MM.filter (maybe True isSealTxOut . fmap toaOut) utxoModifier  -- reserve all deletions

goldUtxoModifier :: UtxoModifier -> UtxoModifier
goldUtxoModifier utxoModifier = MM.filter (maybe True isGoldTxOut . fmap toaOut) utxoModifier  -- reserve all deletions

dollarUtxoModifier :: UtxoModifier -> UtxoModifier
dollarUtxoModifier utxoModifier = MM.filter (maybe True isDollarTxOut . fmap toaOut) utxoModifier  -- reserve all deletions

utxoToAddrBalanceMaps :: Utxo -> (AddrCoinMap, AddrGoldCoinMap, AddrGoldDollarMap)
utxoToAddrBalanceMaps utxo = 
        (addrCoinMap, addrGoldCoinMap, addrDollarCoinMap)
    where 
        addrSealPairs = map (\TxOutAux{..} -> (txOutAddress toaOut, txOutSeal toaOut)) $ M.elems (sealUtxo utxo)
        addrGoldPairs = map (\TxOutAux{..} -> (txOutAddress toaOut, txOutGold toaOut)) $ M.elems (goldUtxo utxo)
        addrDollarPairs = map (\TxOutAux{..} -> (txOutAddress toaOut, txOutDollar toaOut)) $ M.elems (dollarUtxo utxo)

        addrCoinMap = HM.fromListWith unsafeAddMoney addrSealPairs
        addrGoldCoinMap = HM.fromListWith unsafeAddMoney addrGoldPairs
        addrDollarCoinMap = HM.fromListWith unsafeAddMoney addrDollarPairs

getGoldCoinStateOnly :: Utxo -> Either Text (TxIn, TxOutAux)
getGoldCoinStateOnly utxo = 
    case (M.toList $ M.filter (isGoldCoinStateTxOut . toaOut) utxo) of
        [(txIn, txOutAux)] -> Right (txIn, txOutAux)
        _                  -> Left "Gold coin state not found or found more than one!"

getGoldDollarStateOnly :: Utxo -> Either Text (TxIn, TxOutAux)
getGoldDollarStateOnly utxo = 
    case (M.toList $ M.filter (isGoldDollarStateTxOut . toaOut) utxo) of
        [(txIn, txOutAux)] -> Right (txIn, txOutAux)
        _                  -> Left "Gold dollar state not found or found more than one!"
