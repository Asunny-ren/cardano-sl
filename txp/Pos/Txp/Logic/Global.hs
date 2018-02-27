{-# LANGUAGE TypeOperators #-}

-- | Logic for global processing of transactions.  Global transaction
-- is a transaction which has already been added to the blockchain.

module Pos.Txp.Logic.Global
       ( txpGlobalSettings

       -- * Helpers
       , ApplyBlocksSettings (..)
       -- , applyBlocksWith
       , blundToAuxNUndo
       -- , genericGlobalToilStateToBatch
       -- , runToilAction
       ) where

import           Universum

import           Control.Monad.Except (throwError)
import           Data.Default (Default)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import           Formatting (build, sformat, (%))

import           Pos.Core.Block.Union (ComponentBlock (..))
import           Pos.Core.Class (epochIndexL)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Txp (TxAux, TxUndo, TxpUndo)
import           Pos.DB (MonadDBRead, SomeBatchOp (..))
import           Pos.DB.Class (gsAdoptedBVData)
import qualified Pos.DB.GState.Stakes as DB
import           Pos.Exception (assertionFailed)
import           Pos.Txp.Base (flattenTxPayload)
import qualified Pos.Txp.DB as DB
import           Pos.Txp.Logic.Common (buildUtxo)
import           Pos.Txp.Settings.Global (TxpBlock, TxpBlund, TxpGlobalApplyMode,
                                          TxpGlobalRollbackMode, TxpGlobalSettings (..),
                                          TxpGlobalVerifyMode)
import           Pos.Txp.Toil (GlobalToilEnv (..), GlobalToilM, GlobalToilState (..),
                               StakesView (..), ToilVerFailure, UtxoM, UtxoModifier, applyToil,
                               defGlobalToilState, gtsUtxoModifier, rollbackToil, runGlobalToilM,
                               runUtxoM, utxoToLookup, verifyToil)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..))
import qualified Pos.Util.Modifier as MM

-- | Settings used for global transactions data processing used by a
-- simple full node.
txpGlobalSettings :: TxpGlobalSettings
txpGlobalSettings =
    TxpGlobalSettings
    { tgsVerifyBlocks = verifyBlocks
    , tgsApplyBlocks = undefined -- applyBlocksWith applyBlocksSettings
    , tgsRollbackBlocks = rollbackBlocks
    }

verifyBlocks ::
       forall m. TxpGlobalVerifyMode m
    => Bool
    -> OldestFirst NE TxpBlock
    -> m $ Either ToilVerFailure $ OldestFirst NE TxpUndo
verifyBlocks verifyAllIsKnown newChain = runExceptT $ do
    bvd <- gsAdoptedBVData
    let verifyPure :: [TxAux] -> UtxoM (Either ToilVerFailure TxpUndo)
        verifyPure = runExceptT . verifyToil bvd epoch verifyAllIsKnown
        foldStep ::
               (UtxoModifier, [TxpUndo])
            -> TxpBlock
            -> ExceptT ToilVerFailure m (UtxoModifier, [TxpUndo])
        foldStep (modifier, undos) (convertPayload -> txAuxes) = do
            baseUtxo <- utxoToLookup <$> buildUtxo modifier txAuxes
            case runUtxoM modifier baseUtxo (verifyPure txAuxes) of
                (Left err, _) -> throwError err
                (Right txpUndo, newModifier) ->
                    return (newModifier, txpUndo : undos)
        -- 'NE.fromList' is safe here, because there will be at least
        -- one 'foldStep' (since 'newChain' is not empty) and it will
        -- either fail (and then 'convertRes' will not be called) or
        -- will prepend something to the result.
        convertRes :: (UtxoModifier, [TxpUndo]) -> OldestFirst NE TxpUndo
        convertRes = OldestFirst . NE.fromList . reverse . snd
    convertRes <$> foldM foldStep mempty newChain
  where
    epoch = NE.last (getOldestFirst newChain) ^. epochIndexL
    convertPayload :: TxpBlock -> [TxAux]
    convertPayload (ComponentBlockMain _ payload) = flattenTxPayload payload
    convertPayload (ComponentBlockGenesis _)      = []

data ApplyBlocksSettings extra m = ApplyBlocksSettings
    { absApplySingle     :: TxpBlund -> m ()
    , absExtraOperations :: extra -> SomeBatchOp
    }

-- applyBlocksSettings
--     :: GlobalApplyToilMode m
--     => ApplyBlocksSettings () m
-- applyBlocksSettings =
--     ApplyBlocksSettings
--     { absApplySingle = applyToil . blundToAuxNUndo
--     , absExtraOperations = const mempty
--     }

-- applyBlocksWith
--     :: (TxpGlobalApplyMode ctx m, Default extra)
--     => ApplyBlocksSettings extra (ToilT extra (DBToil m))
--     -> OldestFirst NE TxpBlund
--     -> m SomeBatchOp
-- applyBlocksWith ApplyBlocksSettings {..} blunds = do
--     let blocks = map fst blunds
--     inAssertMode $ do
--         verdict <- verifyBlocks False blocks
--         whenLeft verdict $
--             assertionFailed .
--             sformat ("we are trying to apply txp blocks which we fail to verify: "%build)
--     genericToilModifierToBatch absExtraOperations . snd <$>
--         runToilAction (mapM absApplySingle blunds)

rollbackBlocks ::
       forall m. TxpGlobalRollbackMode m
    => NewestFirst NE TxpBlund
    -> m SomeBatchOp
rollbackBlocks blunds =
    globalToilStateToBatch <$> foldM rollbackStep defGlobalToilState blunds
  where
    rollbackStep :: GlobalToilState -> TxpBlund -> m GlobalToilState
    rollbackStep gts txpBlund = do
        let txAuxesAndUndos = blundToAuxNUndo txpBlund
            txAuxes = fst <$> txAuxesAndUndos
        totalStake <- DB.getRealTotalStake
        baseUtxo <- utxoToLookup <$> buildUtxo (gts ^. gtsUtxoModifier) txAuxes
        let env =
                GlobalToilEnv
                { _gteUtxo = baseUtxo
                , _gteTotalStake = totalStake
                }
        snd <$> runGlobalToilM env gts DB.getRealStake (rollbackToil txAuxesAndUndos)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Convert 'GlobalToilState' to batch of database operations.
globalToilStateToBatch :: HasConfiguration => GlobalToilState -> SomeBatchOp
globalToilStateToBatch GlobalToilState {..} =
    SomeBatchOp [SomeBatchOp utxoOps, SomeBatchOp stakesOps]
  where
    StakesView (HM.toList -> stakes) total = _gtsStakesView
    utxoOps =
        map DB.DelTxIn (MM.deletions _gtsUtxoModifier) ++
        map (uncurry DB.AddTxOut) (MM.insertions _gtsUtxoModifier)
    stakesOps = addTotalStakeOp $ map (uncurry DB.PutFtsStake) stakes
    addTotalStakeOp =
        case total of
            Nothing -> identity
            Just x  -> (DB.PutTotalStake x :)

-- Zip block's TxAuxes and corresponding TxUndos.
blundToAuxNUndo :: TxpBlund -> [(TxAux, TxUndo)]
blundToAuxNUndo (ComponentBlockGenesis _ , _)        = []
blundToAuxNUndo (ComponentBlockMain _ payload, undo) = zip (flattenTxPayload payload) undo
