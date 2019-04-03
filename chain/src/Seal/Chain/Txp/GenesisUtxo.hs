-- | Runtime propagation of genesis data (stakes & utxo).

module Seal.Chain.Txp.GenesisUtxo
       ( genesisUtxo
       , genesisStakes
       ) where

import           Universum

import           Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

import           Seal.Chain.Genesis (GenesisData (..), Issuers (..),
                     GenesisProtocolConstants (..), getGenesisAvvmBalances,
                     getGenesisNonAvvmBalances)
import           Seal.Chain.Txp.Toil (Utxo, utxoToStakes)
import           Seal.Chain.Txp.Tx (TxIn (..), TxOut (..), 
                     IssueCert (..), IssueState (..))
import           Seal.Chain.Txp.TxOutAux (TxOutAux (..))
import           Seal.Core (Address, Coin, StakesMap, makeRedeemAddress)
import           Seal.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Seal.Crypto (unsafeHash)


genesisStakes :: GenesisData -> StakesMap
genesisStakes genesisData =
    utxoToStakes (gdBootStakeholders genesisData) $ genesisUtxo genesisData

genesisUtxo :: GenesisData -> Utxo
genesisUtxo genesisData =
    let
        networkMagic :: NetworkMagic
        networkMagic = makeNetworkMagic $
                       gpcProtocolMagic $
                       gdProtocolConsts genesisData

        preUtxo :: [(Address, Coin)]
        preUtxo =
            (first (makeRedeemAddress networkMagic) <$> HM.toList
                    (getGenesisAvvmBalances $ gdAvvmDistr genesisData)
                )
                <> (HM.toList $ getGenesisNonAvvmBalances $ gdNonAvvmBalances
                       genesisData
                   )

        utxoEntry :: (Address, Coin) -> (TxIn, TxOutAux)
        utxoEntry (addr, coin) =
            (TxInUtxo (unsafeHash addr) 0, TxOutAux (TxOutSeal addr coin Nothing))
        
        toStateUtxo :: Address -> [(TxIn, TxOutAux)]
        toStateUtxo addr =
            let goldState = 
                    ( TxInUtxo (unsafeHash addr) (maxBound - 1)
                    , TxOutAux $ TxOutState addr  (GoldCoinIssueCert def "Bang") (GoldCoinState def)
                    )
                dollarState = 
                    ( TxInUtxo (unsafeHash addr) maxBound
                    , TxOutAux $ TxOutState addr (GoldDollarIssueCert def def "Bang") (GoldDollarState def def)
                    )
            in [goldState, dollarState]

        stateUtxos :: [(TxIn, TxOutAux)]
        stateUtxos = foldl' (<>) [] $ map toStateUtxo (getIssuers $ gdIssuers genesisData)

    in Map.fromList $ (utxoEntry <$> preUtxo) <> stateUtxos
