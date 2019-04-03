{-# LANGUAGE RecordWildCards #-}
-- | Utility functions working on Utxo.

module Seal.Chain.Txp.Toil.Util
       ( filterUtxoByAddr
       , filterUtxoByAddrs
       , getTotalCoinsInUtxo
       , utxoToStakes
       , utxoToAddressCoinPairs
       , utxoToAddressCoinMap
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M

import           Seal.Chain.Genesis (GenesisWStakeholders)
import           Seal.Chain.Txp.Base (addrBelongsTo, addrBelongsToSet,
                     txOutStake)
import           Seal.Chain.Txp.Toil.Types (Utxo)
import           Seal.Chain.Txp.Tx (TxOut (..))
import           Seal.Chain.Txp.TxOutAux (TxOutAux (..))
import           Seal.Core (Address, Currency (..), StakesMap,
                     CoinGroup (..), sumCoinGroups, unsafeAddCoinGroup)

-- | Select only TxOuts for given address
filterUtxoByAddr :: Address -> Utxo -> Utxo
filterUtxoByAddr addr = M.filter (`addrBelongsTo` addr)

-- | Select only TxOuts for given addresses
filterUtxoByAddrs :: [Address] -> Utxo -> Utxo
filterUtxoByAddrs addrs =
    let addrSet = HS.fromList addrs
    in  M.filter (`addrBelongsToSet` addrSet)

-- | Get total amount of coins in given Utxo
getTotalCoinsInUtxo :: Utxo -> CoinGroup
getTotalCoinsInUtxo =
    sumCoinGroups . map (getCoinGroup . toaOut) . toList

getCoinGroup :: TxOut -> CoinGroup
getCoinGroup TxOutSeal{..} = CoinGroup txOutSeal (mkMoney 0) (mkMoney 0)
getCoinGroup TxOutGold{..} = CoinGroup (mkMoney 0) txOutGold (mkMoney 0)
getCoinGroup TxOutDollar{..} = CoinGroup (mkMoney 0) (mkMoney 0) txOutDollar
getCoinGroup _ = CoinGroup (mkMoney 0) (mkMoney 0) (mkMoney 0)

-- | Convert 'Utxo' to 'StakesMap'.
utxoToStakes :: GenesisWStakeholders -> Utxo -> StakesMap
utxoToStakes bootStakeholders = foldl' putDistr mempty . M.toList
  where
    plusAt hm (key, val) = HM.insertWith unsafeAddMoney key val hm
    putDistr hm (_, TxOutAux txOut) =
        foldl' plusAt hm (txOutStake bootStakeholders txOut)

utxoToAddressCoinPairs :: Utxo -> [(Address, CoinGroup)]
utxoToAddressCoinPairs utxo = combineWith unsafeAddCoinGroup txOuts
  where
    combineWith :: (Eq a, Hashable a) => (b -> b -> b) -> [(a, b)] -> [(a, b)]
    combineWith func = HM.toList . HM.fromListWith func

    txOuts :: [(Address, CoinGroup)]
    txOuts = map processTxOutAux utxoElems

    utxoElems :: [TxOutAux]
    utxoElems = M.elems utxo

    processTxOutAux :: TxOutAux -> (Address, CoinGroup)
    processTxOutAux TxOutAux{..} = (txOutAddress toaOut, getCoinGroup toaOut)

utxoToAddressCoinMap :: Utxo -> HashMap Address CoinGroup
utxoToAddressCoinMap = HM.fromList . utxoToAddressCoinPairs
