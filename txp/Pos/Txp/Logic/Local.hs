{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Logic for local processing of transactions.
-- Local transaction is a transaction which has not yet been added to the blockchain.

module Pos.Txp.Logic.Local
       ( TxpProcessTransactionMode
       , TxpNormalizeMempoolMode
       , txProcessTransaction
       , txProcessTransactionNoLock
       , txNormalize
       , txGetPayload

       -- Utils to processing and nomralization tx
       , buildProccessTxContext
       , TxProcessingMode
       , txProcessTransactionAbstract
       -- , txNormalizeAbstract
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Control.Monad.Except (mapExceptT, runExceptT, throwError)
import           Control.Monad.Reader (mapReaderT)
import           Control.Monad.State.Strict (mapStateT)
import           Data.Default (Default (def))
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M (fromList)
import           Formatting (build, sformat, (%))
import           System.Wlog (NamedPureLogger, WithLogger, logDebug, logError, logWarning)

import           Pos.Core (BlockVersionData, EpochIndex, HasConfiguration, HeaderHash, siEpoch)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, TxUndo)
import           Pos.Crypto (WithHash (..))
import           Pos.DB.Class (MonadDBRead, MonadGState (..))
import qualified Pos.DB.GState.Common as GS
import           Pos.Reporting (reportError)
import           Pos.Slotting (MonadSlots (..))
import           Pos.StateLock (Priority (..), StateLock, StateLockMetrics, withStateLock)
import           Pos.Txp.MemState (GenericTxpLocalData (..), GenericTxpLocalDataPure, MempoolExt,
                                   MonadTxpMem, TxpLocalWorkMode, askTxpMem, getLocalTxsMap,
                                   getUtxoModifier, modifyTxpLocalData, setTxpLocalData)
import           Pos.Txp.Toil (LocalToilState (..), ToilVerFailure (..), Utxo, UtxoLookup,
                               mpLocalTxs, normalizeToil, processTx)
import           Pos.Txp.Topsort (topsortTxs)
import           Pos.Util.Util (HasLens (..), HasLens')

type TxpProcessTransactionMode ctx m =
    ( TxpLocalWorkMode ctx m
    , MonadReader ctx m
    , HasLens' ctx StateLock
    , HasLens' ctx StateLockMetrics
    , MonadMask m
    , MempoolExt m ~ ()
    )

-- | Process transaction. 'TxId' is expected to be the hash of
-- transaction in 'TxAux'. Separation is supported for optimization
-- only.
txProcessTransaction
    :: TxpProcessTransactionMode ctx m
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
txProcessTransaction itw =
    withStateLock LowPriority "txProcessTransaction" $ \__tip -> txProcessTransactionNoLock itw

-- | Unsafe version of 'txProcessTransaction' which doesn't take a
-- lock. Can be used in tests.
txProcessTransactionNoLock
    :: ( TxpLocalWorkMode ctx m
       , MempoolExt m ~ ()
       )
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
txProcessTransactionNoLock =
    txProcessTransactionAbstract buildProccessTxContext processTxHoisted
  where
    processTxHoisted ::
           BlockVersionData
        -> EpochIndex
        -> (TxId, TxAux)
        -> TxProcessingMode TxUndo
    processTxHoisted bvd epoch tx =
        mapExceptT (mapReaderT (mapStateT lift)) $ processTx bvd epoch tx

type TxProcessingMode =
    ExceptT ToilVerFailure (
        ReaderT UtxoLookup (
            StateT LocalToilState (
                NamedPureLogger Identity
    )))

txProcessTransactionAbstract
    :: forall ext ctx m a .
       ( TxpLocalWorkMode ctx m
       , MempoolExt m ~ ext
       )
    => (TxAux -> m UtxoLookup)
    -> (BlockVersionData -> EpochIndex -> (TxId, TxAux) -> TxProcessingMode a)
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
txProcessTransactionAbstract buildPTxContext txAction itw@(txId, txAux) = reportTipMismatch $ runExceptT $ do
    -- Note: we need to read tip from the DB and check that it's the
    -- same as the one in mempool. That's because mempool state is
    -- valid only with respect to the tip stored there. Normally tips
    -- will match, because whenever we apply/rollback blocks we
    -- normalize mempool. However, there is a corner case when we
    -- receive an unexpected exception after modifying GState and
    -- before normalization. In this case normalization can fail and
    -- tips will differ. Rejecting transactions in this case should be
    -- fine, because the fact that we receive exceptions likely
    -- indicates that something is bad and we have more serious issues.
    --
    -- Also note that we don't need to use a snapshot here and can be
    -- sure that GState won't change, because changing it requires
    -- 'StateLock' which we own inside this function.
    tipDB <- lift GS.getTip
    epoch <- siEpoch <$> (note ToilSlotUnknown =<< getCurrentSlot)
    utxo <- lift $ buildPTxContext txAux
    bvd <- gsAdoptedBVData
    pRes <-
        lift $
        modifyTxpLocalData $
        processTransactionPure bvd epoch utxo tipDB itw
    -- We report 'ToilTipsMismatch' as an error, because usually it
    -- should't happen. If it happens, it's better to look at logs.
    case pRes of
        Left er -> do
            logDebug $ sformat ("Transaction processing failed: " %build) txId
            throwError er
        Right _ ->
            logDebug
                (sformat ("Transaction is processed successfully: " %build) txId)
  where
    processTransactionPure
        :: BlockVersionData
        -> EpochIndex
        -> UtxoLookup
        -> HeaderHash
        -> (TxId, TxAux)
        -> GenericTxpLocalDataPure ext
        -> NamedPureLogger Identity (Either ToilVerFailure (), GenericTxpLocalDataPure ext)
    processTransactionPure bvd curEpoch utxo tipDB tx txld@(um, mp, undo, tip, extra)
        | tipDB /= tip = pure (Left $ ToilTipsMismatch tipDB tip, txld)
        | otherwise = do
            let initialState = LocalToilState { _ltsMemPool = mp
                                              , _ltsUtxoModifier = um
                                              , _ltsUndos = undo
                                              }
            res :: (Either ToilVerFailure a, LocalToilState) <-
                    usingStateT initialState $
                    usingReaderT utxo $
                    runExceptT $
                    txAction bvd curEpoch tx
            case res of
                (Left er, _) -> pure (Left er, txld)
                (Right _, LocalToilState {..}) -> pure
                    ( Right ()
                    , (_ltsUtxoModifier, _ltsMemPool, _ltsUndos, tip, undefined))
    -- REPORT:ERROR Tips mismatch in txp.
    reportTipMismatch action = do
        res <- action
        res <$ case res of
            (Left err@(ToilTipsMismatch {})) -> reportError (pretty err)
            _                                -> pass

buildProccessTxContext
    :: forall m ctx.
       ( MonadIO m
       , MonadDBRead m
       , MonadTxpMem (MempoolExt m) ctx m
       )
    => TxAux -> m UtxoLookup
buildProccessTxContext txAux = undefined -- do
    -- let UnsafeTx {..} = taTx txAux
    -- localUM <- getUtxoModifier @(MempoolExt m)
    -- let runUM um = runToilTLocal um def mempty
    -- (resolvedOuts, _) <- runDBToil $ runUM localUM $ mapM utxoGet _txInputs
    -- -- Resolved are unspent transaction outputs corresponding to input
    -- -- of given transaction.
    -- let resolved =
    --         M.fromList $
    --         catMaybes $
    --         toList $ NE.zipWith (liftM2 (,) . Just) _txInputs resolvedOuts
    -- pure $
    --     ProcessTxContext
    --     { _ptcAdoptedBVData = bvd
    --     , _ptcUtxoBase = resolved
    --     , _ptcExtra    = ()
    --     }

type TxpNormalizeMempoolMode ctx m =
    ( TxpLocalWorkMode ctx m
    , MonadSlots ctx m
    , MempoolExt m ~ ()
    )

-- | 1. Recompute UtxoView by current MemPool
-- | 2. Remove invalid transactions from MemPool
-- | 3. Set new tip to txp local data
txNormalize
    :: TxpNormalizeMempoolMode ctx m
    => m ()
txNormalize = undefined -- txNormalizeAbstract (\e txs -> normalizeToil e (HM.toList txs))

-- txNormalizeAbstract
--     :: ( TxpLocalWorkMode ctx m
--        , MonadSlots ctx m
--        , Default (MempoolExt m)
--        )
--     => (EpochIndex -> HashMap TxId TxAux -> ToilT (MempoolExt m) (DBToil m) ())
--     -> m ()
-- txNormalizeAbstract normalizeAction = getCurrentSlot >>= \case
--     Nothing -> do
--         tip <- GS.getTip
--         -- Clear and update tip
--         setTxpLocalData (mempty, def, mempty, tip, def)
--     Just (siEpoch -> epoch) -> do
--         utxoTip <- GS.getTip
--         localTxs <- getLocalTxsMap
--         ToilModifier {..} <-
--             runDBToil $
--             snd <$> runToilTLocalExtra mempty def mempty def
--             (normalizeAction epoch localTxs)
--         setTxpLocalData (_tmUtxo, _tmMemPool, _tmUndos, utxoTip, _tmExtra)

-- | Get 'TxPayload' from mempool to include into a new block which
-- will be based on the given tip. In something goes wrong, empty
-- payload is returned. That's because we would sooner create an empty
-- block to maintain decent chain quality than skip block creation.
--
-- We need to explicitly check that tip matches, even though we do
-- mempool normalization whenever we apply/rollback a block. That's
-- because we can't make them both atomically, i. e. can't guarantee
-- that either none or both of them will be done.
txGetPayload :: (MonadIO m, MonadTxpMem ext ctx m, WithLogger m) => HeaderHash -> m [TxAux]
txGetPayload neededTip = do
    TxpLocalData {..} <- askTxpMem
    (view mpLocalTxs -> memPool, memPoolTip) <-
        atomically $ (,) <$> readTVar txpMemPool <*> readTVar txpTip
    let tipMismatchMsg =
            sformat
                ("txGetPayload: tip mismatch (in DB: )"%build%
                 ", (in mempool: "%build%")")
                neededTip memPoolTip
    let topsortFailMsg = "txGetPayload: topsort failed!"
    let convertTx (txId, txAux) = WithHash (taTx txAux) txId
    case (memPoolTip == neededTip, topsortTxs convertTx $ HM.toList memPool) of
        (False, _)       -> [] <$ logWarning tipMismatchMsg
        (True, Nothing)  -> [] <$ logError topsortFailMsg
        (True, Just res) -> return $ map snd res
