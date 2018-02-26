{-# LANGUAGE TypeFamilies #-}

-- | Extra type classes used for explorer's toil.

module Pos.Explorer.Txp.Toil.Monadic
       (
         TxExtraLookup (..)
       , TxExtraM

       , getTxExtra
       , getAddrHistory
       , getAddrBalance
       , getUtxoSum

       , putTxExtra
       , delTxExtra
       , updateAddrHistory
       , putAddrBalance
       , delAddrBalance
       , putUtxoSum

       , ELocalToilM
       , txExtraMToELocalToilM
       ) where

import           Universum

import           Control.Lens (at, magnify, zoom, (%=), (.=))
import           Control.Monad.Reader (mapReaderT)
import           System.Wlog (NamedPureLogger)

import           Pos.Core (Address, Coin, TxId)
import           Pos.Explorer.Core (AddrHistory, TxExtra)
import           Pos.Explorer.Txp.Toil.Types (ExplorerExtraModifier, eemAddrBalances,
                                              eemAddrHistories, eemLocalTxsExtra, eemNewUtxoSum)
import           Pos.Txp.Toil (LocalToilState, UtxoLookup)
import qualified Pos.Util.Modifier as MM

----------------------------------------------------------------------------
-- Monadic actions with extra txp data.
----------------------------------------------------------------------------

data TxExtraLookup = TxExtraLookup
    { telGetTxExtra     :: TxId -> Maybe TxExtra
    , telGetAddrHistory :: Address -> AddrHistory
    , telGetAddrBalance :: Address -> Maybe Coin
    , telGetUtxoSum     :: Integer
    }

-- | Utility monad which allows to lookup extra values related to txp and modify them.
type TxExtraM
     = ReaderT TxExtraLookup (StateT ExplorerExtraModifier (NamedPureLogger Identity))

getTxExtra :: TxId -> TxExtraM (Maybe TxExtra)
getTxExtra txId = do
    baseLookup <- telGetTxExtra <$> ask
    MM.lookup baseLookup txId <$> use eemLocalTxsExtra

getAddrHistory :: Address -> TxExtraM AddrHistory
getAddrHistory addr = do
    use (eemAddrHistories . at addr) >>= \case
        Nothing -> telGetAddrHistory <$> ask <*> pure addr
        Just hist -> pure hist

getAddrBalance :: Address -> TxExtraM (Maybe Coin)
getAddrBalance addr = do
    baseLookup <- telGetAddrBalance <$> ask
    MM.lookup baseLookup addr <$> use eemAddrBalances

getUtxoSum :: TxExtraM Integer
getUtxoSum = fromMaybe <$> (telGetUtxoSum <$> ask) <*> use eemNewUtxoSum

putTxExtra :: TxId -> TxExtra -> TxExtraM ()
putTxExtra txId extra = eemLocalTxsExtra %= MM.insert txId extra

delTxExtra :: TxId -> TxExtraM ()
delTxExtra txId = eemLocalTxsExtra %= MM.delete txId

updateAddrHistory :: Address -> AddrHistory -> TxExtraM ()
updateAddrHistory addr hist = eemAddrHistories . at addr .= Just hist

putAddrBalance :: Address -> Coin -> TxExtraM ()
putAddrBalance addr coin = eemAddrBalances %= MM.insert addr coin

delAddrBalance :: Address -> TxExtraM ()
delAddrBalance addr = eemAddrBalances %= MM.delete addr

putUtxoSum :: Integer -> TxExtraM ()
putUtxoSum utxoSum = eemNewUtxoSum .= Just utxoSum

----------------------------------------------------------------------------
-- Monad used for local Toil in Explorer.
----------------------------------------------------------------------------

type ELocalToilM
     = ReaderT (UtxoLookup, TxExtraLookup) (
       StateT  (LocalToilState, ExplorerExtraModifier) (
       NamedPureLogger
       Identity))

txExtraMToELocalToilM :: TxExtraM a -> ELocalToilM a
txExtraMToELocalToilM = mapReaderT (zoom _2) . magnify _2
