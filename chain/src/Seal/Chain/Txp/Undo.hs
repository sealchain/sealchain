module Seal.Chain.Txp.Undo
       ( TxpUndo
       , TxUndo
       ) where

import           Universum

import           Seal.Chain.Txp.TxOutAux

type TxpUndo = [TxUndo]

-- | Particular undo needed for transactions
-- Just means we know transaction input, hence know TxOutAux corresponding to it,
-- Nothing otherwise.
type TxUndo =  [Maybe TxOutAux]
