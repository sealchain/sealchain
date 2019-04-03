{-# LANGUAGE RecordWildCards #-}

module Seal.Chain.Txp.Tx
       ( Tx (..)
       , txUtxoInputs
       , txUtxoOutputs
       , txAccountInputs
       , txAccountOutputs
       , txCmds
       , txAttributes
       , checkTx
       , txF

       , TxId

       , TxIn (..)
       , isTxInUnknown

       , IssueCert (..)
       , IssueState (..)
       , TxOut (..)
       , isSealTxOut
       , isGoldTxOut
       , isDollarTxOut
       , isCurrencyTxOut
       , isGoldCoinStateTxOut
       , isGoldDollarStateTxOut
       , isStateTxOut

       , Nonce
       , AccountIn (..)
       , AccountOut (..)

       , isGoldIssueTx
       , isDollarIssueTx
       ) where

import           Universum

import qualified Codec.CBOR.Encoding as E
import           Codec.CBOR.Decoding as D
import           Control.Lens (makeLenses)
import           Control.Monad.Except (MonadError (throwError))
import           Data.Aeson (FromJSON (..), FromJSONKey (..),
                     FromJSONKeyFunction (..), ToJSON (toJSON), ToJSONKey (..),
                     object, withObject, (.:), (.=))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (toJSONKeyText)
import qualified Data.ByteString.Lazy as LBS
import           Data.Default (def)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Data.Text as T
import           Formatting (Format, bprint, build, builder, int, sformat, (%), stext)
import qualified Formatting.Buildable as Buildable
import qualified Serokell.Util.Base16 as B16
import           Serokell.Util.Text (listJson)
import           Serokell.Util.Verify (VerificationRes (..), verResSingleF,
                     verifyGeneric)

import           Seal.Binary.Class (Bi (..), Case (..), Cons (..), Field (..),
                     decodeKnownCborDataItem, decodeUnknownCborDataItem, cborError,
                     deriveIndexedBi, encodeKnownCborDataItem, encodeListLen,
                     encodeUnknownCborDataItem, enforceSize, matchSize,
                     knownCborDataItemSizeExpr, szCases)
import           Seal.Chain.Txp.TxAttributes
import           Seal.Core.Attributes (areAttributesKnown)
import           Seal.Core.Common (Address (..), Coin (..), Currency (..), GoldCoin (..), 
                     GoldDollar (..), Account (..), CoinGroup (..), Cmd,
                     decodeTextAddress, anyGreatThanCoinGroup, allLittleEqualCoinGroup)
import           Seal.Core.Slotting (SlotId)
import           Seal.Core.Util.LogSafe (SecureLog (..))
import           Seal.Crypto (Hash, decodeAbstractHash, hash, hashHexF,
                     shortHashF)
import           Seal.Util.Util (toAesonError)

----------------------------------------------------------------------------
-- Tx
----------------------------------------------------------------------------

-- | Transaction.
--
-- NB: transaction witnesses are stored separately.
data Tx = UnsafeTx
    { _txUtxoInputs     :: ![TxIn]       -- ^ Utxo Inputs of transaction.
    , _txUtxoOutputs    :: ![TxOut]      -- ^ Utxo Outputs of transaction.
    , _txAccountInputs  :: ![AccountIn]  -- ^ Account Inputs of transaction.
    , _txAccountOutputs :: ![AccountOut] -- ^ Account Outputs of transaction.
    , _txCmds           :: ![Cmd]        -- ^ Commands of calling smart contracts.
    , _txAttributes     :: !TxAttributes -- ^ Attributes of transaction
    } deriving (Eq, Ord, Generic, Show, Typeable)

--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

instance Buildable (SecureLog TxId) where
    build _ = "<txid>"

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------

-- | Transaction arbitrary input.
data TxIn
      -- | TxId = Which transaction's output is used
      -- | Word32 = Index of the output in transaction's outputs
    = TxInUtxo TxId Word32
    | TxInUnknown !Word8 !ByteString
    deriving (Eq, Ord, Generic, Show, Typeable)

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

data IssueCert
    = GoldCoinIssueCert 
    { issuedGolds     :: GoldCoin
    , reason          :: Text
    }
    | GoldDollarIssueCert 
    { issuedDollars    :: GoldDollar
    , lockedGolds      :: GoldCoin
    , reason           :: Text
    }
    | GoldDollarDestroyCert 
    { destroyedDollars :: GoldDollar
    , unlockedGolds    :: GoldCoin
    , reason           :: Text
    } deriving (Eq, Ord, Generic, Show, Typeable)

data IssueState
    = GoldCoinState 
    { totalGolds       :: GoldCoin
    }
    | GoldDollarState 
    { totalDollars     :: GoldDollar
    , totalLockedGolds :: GoldCoin
    } deriving (Eq, Ord, Generic, Show, Typeable)


-- | Transaction utxo output.
data TxOut 
    = TxOutSeal
    { txOutAddress   :: !Address
    , txOutSeal      :: !Coin
    , txOutLockTime  :: !(Maybe SlotId)
    }   
    | TxOutGold   
    { txOutAddress   :: !Address
    , txOutGold      :: !GoldCoin
    }   
    | TxOutDollar   
    { txOutAddress   :: !Address
    , txOutDollar    :: !GoldDollar
    }
    | TxOutState
    { txOutAddress   :: !Address
    , txOutCert      :: !IssueCert
    , txOutState     :: !IssueState
    } deriving (Eq, Ord, Generic, Show, Typeable)

--------------------------------------------------------------------------------
-- AccountIn
--------------------------------------------------------------------------------

type Nonce = Word64

data AccountIn 
    = RedeemInput
    { riAccount :: Account
    , riNonce   :: Nonce
    , riValue   :: CoinGroup
    } deriving (Eq, Ord, Generic, Show, Typeable)

--------------------------------------------------------------------------------
-- AccountOut
--------------------------------------------------------------------------------

data AccountOut 
    = DepositOutput
    { doAccount :: Account
    , doValue   :: CoinGroup
    } deriving (Eq, Ord, Generic, Show, Typeable)

----------------------------------------------------------------------------
-- Tx instances and utils
----------------------------------------------------------------------------

instance Hashable Tx
instance NFData Tx

instance Buildable Tx where
    build tx@(UnsafeTx{..}) =
        bprint
            ("Tx "%build%" with"
            %" utxo inputs "%listJson
            %",utxo outputs: "%listJson
            %",account inputs: "%listJson 
            %",account outputs: "%listJson
            %",cmds: "%listJson
             %builder)
            (hash tx) 
            _txUtxoInputs
            _txUtxoOutputs
            _txAccountInputs
            _txAccountOutputs
            _txCmds
            attrsBuilder
      where
        attrs = _txAttributes
        attrsBuilder | areAttributesKnown attrs = mempty
                     | otherwise = bprint (", attributes: "%build) attrs

instance Bi Tx where
    encode UnsafeTx{..}
        | isPureUtxoTx = do
            encodeListLen 3
            <> encode _txUtxoInputs
            <> encode _txUtxoOutputs
            <> encode _txAttributes
        | otherwise = do
            E.encodeMapLen (len + 1)
            <> encode (0 :: Word8) <> encode _txAttributes
            <> encoder
      where
          isPureUtxoTx
              =  null _txAccountInputs
              && null _txAccountOutputs
              && null _txCmds
              && (not (null _txUtxoInputs))
              && (not (null _txUtxoOutputs))

          encodeItem tag item
              | null item = (Sum 0, mempty)
              | otherwise = (Sum 1, encode tag <> encode item)
          (Sum len, encoder) = encodeItem (1 :: Word8) _txUtxoInputs
                            <> encodeItem (2 :: Word8) _txUtxoOutputs
                            <> encodeItem (3 :: Word8) _txAccountInputs
                            <> encodeItem (4 :: Word8) _txAccountOutputs
                            <> encodeItem (5 :: Word8) _txCmds

    decode = do
        tokenType <- D.peekTokenType
        case tokenType of 
            D.TypeMapLen -> do
                mapLen <- D.decodeMapLen
                go initValue mapLen

            _            -> do
                enforceSize "Tx" 3
                UnsafeTx <$> decode <*> decode <*> pure [] <*> pure [] <*> pure [] <*> decode

      where
          initValue =
              UnsafeTx
              { _txUtxoInputs = []
              , _txUtxoOutputs = []
              , _txAccountInputs = []
              , _txAccountOutputs = []
              , _txCmds = []
              , _txAttributes = def
              }

          updater n tx =
              case n of
                  0 -> do
                    attr <- decode 
                    pure $ Just tx {_txAttributes = attr}
                  1 -> do 
                    utxoInputs <- decode 
                    pure $ Just tx {_txUtxoInputs = utxoInputs}
                  2 -> do
                    utxoOutputs <- decode 
                    pure $ Just tx {_txUtxoOutputs = utxoOutputs}
                  3 -> do
                    accountInputs <- decode 
                    pure $ Just tx {_txAccountInputs = accountInputs}
                  4 -> do
                    accountOutputs <- decode 
                    pure $ Just tx {_txAccountOutputs = accountOutputs}
                  5 -> do
                    cmds <- decode 
                    pure $ Just tx {_txCmds = cmds}
                  _ -> pure Nothing

          go :: Tx -> Int -> Decoder s Tx
          go tx len = do
              if | len == 0 -> do
                    pure tx
                 | otherwise -> do
                     flag <- decode @Word8
                     updaterData <- updater flag tx
                     case updaterData of
                         Nothing        -> cborError "Found unknown field while getting Tx"
                         Just newTx -> go newTx (len - 1)

    encodedSizeExpr size pxy = 1
        + size (_txUtxoInputs     <$> pxy)
        + size (_txUtxoOutputs    <$> pxy)
        + size (_txAccountInputs  <$> pxy)
        + size (_txAccountOutputs <$> pxy)
        + size (_txAttributes     <$> pxy)

deriveSafeCopySimple 0 'base ''Tx

deriveJSON defaultOptions ''Tx

makeLenses ''Tx

-- | Specialized formatter for 'Tx'.
txF :: Format r (Tx -> r)
txF = build

-- | Verify inputs and outputs are non empty; have enough coins.
-- | todo xl add check for account
checkTx
    :: MonadError Text m
    => Tx
    -> m ()
checkTx it =
    case verRes of
        VerSuccess -> pure ()
        failure    -> throwError $ verResSingleF failure
  where
    verRes =
        verifyGeneric $
        concat $ 
        zipWith utxoOutputPredicates [0 ..] (_txUtxoOutputs it) <>
        zipWith accountInputPredicates [0 ..] (_txAccountInputs it) <>
        zipWith accountOutputPredicates [0 ..] (_txAccountOutputs it)

    utxoOutputPredicates (i :: Word) TxOutSeal{..} =
        [ ( txOutSeal > Coin 0
          , sformat
                ("output #"%int%" has non-positive value: "%formatter)
                i txOutSeal
          )
        , ( isRight (checkMoney txOutSeal)
          , sformat
                ("output #"%int%" has invalid coin")
                i
          )
        ]
    utxoOutputPredicates (i :: Word) TxOutGold{..} =
        [ ( txOutGold > GoldCoin 0
          , sformat
                ("output #"%int%" has non-positive value: "%formatter)
                i txOutGold
          )
        , ( isRight (checkMoney txOutGold)
          , sformat
                ("output #"%int%" has invalid coin")
                i
          )
        ]
    utxoOutputPredicates (i :: Word) TxOutDollar{..} =
        [ ( txOutDollar > GoldDollar 0
          , sformat
                ("output #"%int%" has non-positive value: "%formatter)
                i txOutDollar
          )
        , ( isRight (checkMoney txOutDollar)
          , sformat
                ("output #"%int%" has invalid coin")
                i
          )
        ]
    utxoOutputPredicates _ _ = []

    accountInputPredicates (i :: Word) RedeemInput{..} =
        [ ( riValue `anyGreatThanCoinGroup` minBound
          , sformat
                ("input #"%int%" has non-positive value: "%build)
                i riValue
          )
        , ( riValue `allLittleEqualCoinGroup` maxBound
          , sformat
                ("input #"%int%" has invalid coin")
                i
          )
        ]

    accountOutputPredicates (i :: Word) DepositOutput{..} =
        [ ( doValue `anyGreatThanCoinGroup` minBound
          , sformat
                ("output #"%int%" has non-positive value: "%build)
                i doValue
          )
        , ( doValue `allLittleEqualCoinGroup` maxBound
          , sformat
                ("output #"%int%" has invalid coin")
                i
          )
        ]

--------------------------------------------------------------------------------
-- TxIn instances and utils
--------------------------------------------------------------------------------

txInFromText :: Text -> Either Text TxIn
txInFromText t = case T.splitOn "_" t of
    ["TxInUtxo", h, idx]     -> TxInUtxo <$> decodeAbstractHash h <*> readEither idx
    ["TxInUnknown", tag, bs] -> TxInUnknown <$> readEither tag <*> B16.decode bs
    _                        -> Left $ "Invalid TxIn " <> t

txInToText :: TxIn -> Text
txInToText (TxInUtxo txInHash txInIndex) =
    sformat ("TxIn_"%hashHexF%"_"%int) txInHash txInIndex
txInToText (TxInUnknown tag bs) =
    sformat ("TxInUnknown_"%int%"_"%B16.base16F) tag bs

instance FromJSON TxIn where
    parseJSON v = toAesonError =<< txInFromText <$> parseJSON v

instance ToJSON TxIn where
    toJSON = toJSON . txInToText

instance FromJSONKey TxIn where
    fromJSONKey = FromJSONKeyTextParser (toAesonError . txInFromText)

instance ToJSONKey TxIn where
    toJSONKey = toJSONKeyText txInToText

instance Hashable TxIn

instance Buildable TxIn where
    build (TxInUtxo txInHash txInIndex) =
        bprint ("TxInUtxo "%shortHashF%" #"%int) txInHash txInIndex
    build (TxInUnknown tag bs) =
        bprint ("TxInUnknown "%int%" "%B16.base16F) tag bs

instance Bi TxIn where
    encode (TxInUtxo txInHash txInIndex) =
        encodeListLen 2 <>
        encode (0 :: Word8) <>
        encodeKnownCborDataItem (txInHash, txInIndex)
    encode (TxInUnknown tag bs) =
        encodeListLen 2 <>
        encode tag <>
        encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        enforceSize "TxIn" 2
        tag <- decode @Word8
        case tag of
            0 -> uncurry TxInUtxo <$> decodeKnownCborDataItem
            _ -> TxInUnknown tag  <$> decodeUnknownCborDataItem
    encodedSizeExpr size _ = 2 + (knownCborDataItemSizeExpr $
        szCases [ let TxInUtxo txInHash txInIndex = error "unused"
                  in  Case "TxIn" (size ((,) <$> pure txInHash <*> pure txInIndex))
                ])

instance NFData TxIn

deriveSafeCopySimple 0 'base ''TxIn

isTxInUnknown :: TxIn -> Bool
isTxInUnknown (TxInUnknown _ _) = True
isTxInUnknown _                 = False

--------------------------------------------------------------------------------
-- TxOut instances and utils
--------------------------------------------------------------------------------

instance Hashable IssueCert
instance Hashable IssueState

instance NFData IssueCert
instance NFData IssueState

instance Buildable IssueCert where
    build (GoldCoinIssueCert{..}) = 
        bprint ("GoldCoinIssueCert "%formatter%", reason: "%stext) issuedGolds reason
    build (GoldDollarIssueCert{..} ) = 
        bprint ("GoldDollarIssueCert "%formatter%"GoldDollar : "%formatter%stext) issuedDollars lockedGolds reason
    build (GoldDollarDestroyCert{..} ) = 
        bprint ("GoldDollarDestroyCert "%formatter%" : "%formatter%stext) destroyedDollars unlockedGolds reason

instance Buildable IssueState where
    build (GoldCoinState{..}) = 
        bprint ("GoldCoinState "%formatter) totalGolds
    build (GoldDollarState{..}) = 
        bprint ("GoldDollarState "%formatter%" locked GoldCoin"%formatter) totalDollars totalLockedGolds

deriveIndexedBi ''IssueCert [
    Cons 'GoldCoinIssueCert [
        Field [| 0 :: GoldCoin|],
        Field [| 1 :: Text|]
    ],
    Cons 'GoldDollarIssueCert [
        Field [| 0 :: GoldDollar|],
        Field [| 1 :: GoldCoin|],
        Field [| 2 :: Text|]
    ],
    Cons 'GoldDollarDestroyCert [
        Field [| 0 :: GoldDollar|],
        Field [| 1 :: GoldCoin|],
        Field [| 2 :: Text|]
    ]]

deriveIndexedBi ''IssueState [
    Cons 'GoldCoinState [
        Field [| 0 :: GoldCoin|]
    ],
    Cons 'GoldDollarState [
        Field [| 0 :: GoldDollar|],
        Field [| 1 :: GoldCoin|]
    ]]

instance FromJSON TxOut where
    parseJSON = withObject "TxOutSeal" $ \o -> do
        txOutSeal     <- toAesonError . integerToMoney =<< o .: "coin"
        txOutAddress  <- toAesonError . decodeTextAddress =<< o .: "address"
        txOutLockTime <- toAesonError =<< o .: "lockTime"
        return $ TxOutSeal {..}
    -- todo xl add other data constructors

instance ToJSON TxOut where
    toJSON TxOutSeal{..} = object [
        "coin"     .= moneyToInteger txOutSeal,
        "address"  .= sformat build txOutAddress,
        "lockTime" .= sformat build txOutLockTime ]
    toJSON TxOutGold{..} = object [
        "gold"     .= moneyToInteger txOutGold,
        "address"  .= sformat build txOutAddress ]
    toJSON TxOutDollar{..} = object [
        "dollar"   .= moneyToInteger txOutDollar,
        "address"  .= sformat build txOutAddress ]
    toJSON TxOutState{..} = object [
        "cert"     .= sformat build txOutCert,
        "state"    .= sformat build txOutState,
        "address"  .= sformat build txOutAddress ]

instance Hashable TxOut

instance Buildable TxOut where
    build TxOutSeal {txOutAddress=addr, txOutSeal=value, txOutLockTime=Nothing} =
        bprint ("TxOutSeal "%formatter%" -> "%build) value addr
    build TxOutSeal {txOutAddress=addr, txOutSeal=value, txOutLockTime=(Just lockTime)} =
        bprint ("TxOutSeal "%formatter%" -> "%build%"(is locked before "%build) value addr lockTime
    build TxOutGold {..} =
        bprint ("TxOutGold "%formatter%" -> "%build) txOutGold txOutAddress
    build TxOutDollar {..} =
        bprint ("TxOutDollar "%formatter%" -> "%build) txOutDollar txOutAddress
    build TxOutState {..} =
        bprint ("TxOutState "%build%" -> "%build%" -> "%build) txOutCert txOutState txOutAddress

instance NFData TxOut

instance Bi TxOut where
    encode (TxOutSeal addr value Nothing) = 
        encodeListLen 3 <> encode (0 :: Word8) <> encode addr <> encode value
    encode (TxOutSeal addr value (Just lockTime)) = 
        encodeListLen 4 <> encode (8 :: Word8) <> encode addr <> encode value <> encode lockTime
    encode (TxOutGold addr value) = 
        encodeListLen 3 <> encode (1 :: Word8) <> encode addr <> encode value
    encode (TxOutDollar addr value) = 
        encodeListLen 3 <> encode (2 :: Word8) <> encode addr <> encode value
    encode (TxOutState addr cert ss) = 
        encodeListLen 4 <> encode (3 :: Word8) <> encode addr <> encode cert <> encode ss
    
    decode = do
        len <- decodeListLenCanonical
        tag <- decode @Word8
        case tag of 
            0 -> do
               matchSize 3 "TxOutSeal without locktime" len
               TxOutSeal <$> decode <*> decode <*> pure Nothing
            1 -> do
               matchSize 3 "TxOutGold" len
               TxOutGold <$> decode <*> decode
            2 -> do
               matchSize 3 "TxOutDollar" len
               TxOutDollar <$> decode <*> decode
            3 -> do
               matchSize 4 "TxOutState" len
               TxOutState <$> decode <*> decode <*> decode
            8 -> do
               matchSize 4 "TxOutSeal with locktime" len
               TxOutSeal <$> decode <*> decode <*> (Just <$> decode)
            _ -> cborError "Found invalid tag while getting TxOut"


deriveSafeCopySimple 0 'base ''IssueCert
deriveSafeCopySimple 0 'base ''IssueState
deriveSafeCopySimple 0 'base ''TxOut

isSealTxOut :: TxOut -> Bool
isSealTxOut TxOutSeal{..} = True
isSealTxOut _ = False

isGoldTxOut :: TxOut -> Bool
isGoldTxOut TxOutGold{..} = True
isGoldTxOut _ = False

isDollarTxOut :: TxOut -> Bool
isDollarTxOut TxOutDollar{..} = True
isDollarTxOut _ = False

isCurrencyTxOut :: TxOut -> Bool
isCurrencyTxOut txOut = 
    (isSealTxOut txOut) ||
    (isGoldTxOut txOut) ||
    (isDollarTxOut txOut)

isStateTxOut :: TxOut -> Bool
isStateTxOut TxOutState{..} = True
isStateTxOut _ = False

isGoldCoinStateTxOut :: TxOut -> Bool
isGoldCoinStateTxOut TxOutState{txOutState = GoldCoinState{}} = True
isGoldCoinStateTxOut _ = False

isGoldDollarStateTxOut :: TxOut -> Bool
isGoldDollarStateTxOut TxOutState{txOutState = GoldDollarState{}} = True
isGoldDollarStateTxOut _ = False

--------------------------------------------------------------------------------
-- AccountIn instances and utils
--------------------------------------------------------------------------------

instance Hashable AccountIn

instance Bi AccountIn where
    encode RedeemInput{..} =
        encodeListLen 4 <>
        encode (0 :: Word8) <>
        encode riAccount <>
        encode riNonce <>
        encode riValue

    decode = do
        enforceSize "AccountIn" 4
        tag <- decode @Word8
        case tag of
            0 -> RedeemInput <$> decode <*> decode <*> decode
            _ -> cborError "Found invalid tag while getting AccountIn"

instance Buildable AccountIn where
    build RedeemInput {..} =
        bprint 
            ("RedeemInput " %build% " with"%
             " nonce "%build%
             ", amount "%build)
            riAccount 
            riNonce 
            riValue

instance NFData AccountIn

deriveSafeCopySimple 0 'base ''AccountIn
deriveJSON defaultOptions ''AccountIn

--------------------------------------------------------------------------------
-- AccountOut instances and utils
--------------------------------------------------------------------------------

instance Hashable AccountOut

instance Bi AccountOut where
    encode DepositOutput{..} =
        encodeListLen 3 <>
        encode (0 :: Word8) <>
        encode doAccount <>
        encode doValue

    decode = do
        enforceSize "AccountOut" 3
        tag <- decode @Word8
        case tag of
            0 -> DepositOutput <$> decode <*> decode
            _ -> cborError "Found invalid tag while getting AccountOut"

instance Buildable AccountOut where
    build DepositOutput {..} =
        bprint 
            ("DepositOutput " %build% " with"%
             ", amount "%build)
            doAccount 
            doValue

instance NFData AccountOut

deriveSafeCopySimple 0 'base ''AccountOut
deriveJSON defaultOptions ''AccountOut

--------------------------------------------------------------------------------
-- utils
--------------------------------------------------------------------------------

-- | is GoldCoin issueing or destroying 
isGoldIssueTx :: Tx -> Bool
isGoldIssueTx UnsafeTx{..} = (length $ filter isGoldCoinStateTxOut _txUtxoOutputs) /= 0

-- | is GoldDollar issueing or destroying 
isDollarIssueTx :: Tx -> Bool
isDollarIssueTx UnsafeTx{..} = (length $ filter isGoldDollarStateTxOut _txUtxoOutputs) /= 0
