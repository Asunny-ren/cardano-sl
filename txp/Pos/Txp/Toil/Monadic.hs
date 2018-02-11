-- | Some monads used in Toil and primitive actions.

module Pos.Txp.Toil.Monadic
       (
         -- * Monadic Utxo
         UtxoM
       , utxoGet
       , utxoPut
       , utxoDel

         -- * Monadic local Toil
       , LocalToilState (..)
       , ltsMemPool
       , ltsUtxoModifier
       , ltsUndos
       , LocalToilM
       , hasTx
       , memPoolSize
       , putTxWithUndo

         -- * Monadic global Toil
       , GlobalToilState (..)
       , GlobalToilM
       , getStake
       , getTotalStake
       , setStake
       , setTotalStake

         -- * Conversions
       , utxoMToLocalToilM
       , utxoMToGlobalToilM
       ) where

import           Universum

import           Control.Lens (at, magnify, makeLenses, zoom, (%=), (+=), (.=))
import           Control.Monad.Reader (mapReaderT)
import           Fmt ((+|), (|+))
import           System.Wlog (NamedPureLogger)

import           Pos.Core.Common (Coin, StakeholderId)
import           Pos.Core.Txp (TxAux, TxId, TxIn, TxOutAux, TxUndo)
import           Pos.Txp.Toil.Types (MemPool, StakesLookup, StakesView, UndoMap, UtxoLookup,
                                     UtxoModifier, mpLocalTxs, mpSize, svStakes, svTotal)
import qualified Pos.Util.Modifier as MM

----------------------------------------------------------------------------
-- Monadic actions with Utxo.
----------------------------------------------------------------------------

-- | Utility monad which allows to lookup values in UTXO and modify it.
type UtxoM = ReaderT UtxoLookup (State UtxoModifier)

-- | Look up an entry in 'Utxo' considering 'UtxoModifier' stored
-- inside 'State'.
utxoGet :: TxIn -> UtxoM (Maybe TxOutAux)
utxoGet txIn = do
    utxoLookup <- ask
    MM.lookup utxoLookup txIn <$> use identity

-- | Add an unspent output to UTXO. If it's already there, throw an 'error'.
utxoPut :: TxIn -> TxOutAux -> UtxoM ()
utxoPut id txOut = utxoGet id >>= \case
    Nothing -> identity %= MM.insert id txOut
    Just _  ->
        -- TODO [CSL-2173]: Comment
        error ("utxoPut: "+|id|+" is already in utxo")

-- | Delete an unspent input from UTXO. If it's not there, throw an 'error'.
utxoDel :: TxIn -> UtxoM ()
utxoDel id = utxoGet id >>= \case
    Just _  -> identity %= MM.delete id
    Nothing ->
        -- TODO [CSL-2173]: Comment
        error ("utxoDel: "+|id|+" is not in the utxo")

----------------------------------------------------------------------------
-- Monad used for local Toil and some actions.
----------------------------------------------------------------------------

data LocalToilState = LocalToilState
    { _ltsMemPool      :: !MemPool
    , _ltsUtxoModifier :: !UtxoModifier
    , _ltsUndos        :: !UndoMap
    }

makeLenses ''LocalToilState

type LocalToilM = ReaderT UtxoLookup (State LocalToilState)

-- | Check whether Tx with given identifier is stored in the pool.
hasTx :: TxId -> LocalToilM Bool
hasTx id = isJust <$> use (ltsMemPool . mpLocalTxs . at id)

-- | Put a transaction with corresponding 'TxUndo' into MemPool.
-- Transaction must not be in MemPool (but it's checked anyway).
putTxWithUndo :: TxId -> TxAux -> TxUndo -> LocalToilM ()
putTxWithUndo id tx undo =
    unlessM (hasTx id) $ do
        ltsMemPool . mpLocalTxs . at id .= Just tx
        ltsMemPool . mpSize += 1
        ltsUndos . at id .= Just undo

-- | Return the number of transactions contained in the pool.
memPoolSize :: LocalToilM Int
memPoolSize = use $ ltsMemPool . mpSize

----------------------------------------------------------------------------
-- Monad used for global Toil and some actions.
----------------------------------------------------------------------------

data GlobalToilState = GlobalToilState
    { _gtsUtxoModifier :: !UtxoModifier
    , _gtsStakesView   :: !StakesView
    }

makeLenses ''GlobalToilState

data GlobalToilEnv = GlobalToilEnv
    { _gteUtxo       :: !UtxoLookup
    , _gteStakes     :: !StakesLookup
    , _gteTotalStake :: !Coin
    }

makeLenses ''GlobalToilEnv

type GlobalToilM
     = NamedPureLogger (ReaderT GlobalToilEnv (State GlobalToilState))

getStake :: StakeholderId -> GlobalToilM (Maybe Coin)
getStake id = lift $ do
    stakesLookup <- view gteStakes
    (<|> stakesLookup id) <$> (use (gtsStakesView . svStakes . at id))

getTotalStake :: GlobalToilM Coin
getTotalStake =
    lift $ maybe (view gteTotalStake) pure =<< use (gtsStakesView . svTotal)

setStake :: StakeholderId -> Coin -> GlobalToilM ()
setStake id c = lift $ gtsStakesView . svStakes . at id .= Just c

setTotalStake :: Coin -> GlobalToilM ()
setTotalStake c = lift $ gtsStakesView . svTotal .= Just c

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

-- | Lift 'UtxoM' action to 'LocalToilM'.
utxoMToLocalToilM :: UtxoM a -> LocalToilM a
utxoMToLocalToilM = mapReaderT f
  where
    f :: forall a. State UtxoModifier a -> State LocalToilState a
    f = zoom ltsUtxoModifier

-- | Lift 'UtxoM' action to 'GlobalToilM'.
utxoMToGlobalToilM :: UtxoM a -> GlobalToilM a
utxoMToGlobalToilM = lift . magnify gteUtxo . mapReaderT f
  where
    f :: forall a. State UtxoModifier a -> State GlobalToilState a
    f = zoom gtsUtxoModifier






----------------------------------------------------------------------------
-- Obsolete
----------------------------------------------------------------------------

-- runToilTGlobal
--     :: (Default ext, Functor m)
--     => ToilT ext m a -> m (a, GenericToilModifier ext)
-- runToilTGlobal txpt = Ether.runStateT' txpt def

-- -- | Run ToilT using empty stakes modifier. Should be used for local
-- -- transaction processing.
-- runToilTLocal
--     :: (Functor m)
--     => UtxoModifier
--     -> MemPool
--     -> UndoMap
--     -> ToilT () m a
--     -> m (a, ToilModifier)
-- runToilTLocal um mp undo txpt =
--     Ether.runStateT' txpt (def {_tmUtxo = um, _tmMemPool = mp, _tmUndos = undo})

-- evalToilTEmpty
--     :: Monad m
--     => ToilT () m a
--     -> m a
-- evalToilTEmpty txpt = Ether.evalStateT txpt def

-- -- | Execute ToilT using empty stakes modifier. Should be used for
-- -- local transaction processing.
-- execToilTLocal
--     :: (Functor m)
--     => UtxoModifier
--     -> MemPool
--     -> UndoMap
--     -> ToilT () m a
--     -> m ToilModifier
-- execToilTLocal um mp undo = fmap snd . runToilTLocal um mp undo

-- -- | Like 'runToilTLocal', but takes extra data as argument.
-- runToilTLocalExtra
--     :: (Functor m)
--     => UtxoModifier
--     -> MemPool
--     -> UndoMap
--     -> extra
--     -> ToilT extra m a
--     -> m (a, GenericToilModifier extra)
-- runToilTLocalExtra um mp undo e =
--     flip Ether.runStateT' $
--         ToilModifier
--         { _tmUtxo = um
--         , _tmStakes = def
--         , _tmMemPool = mp
--         , _tmUndos = undo
--         , _tmExtra = e
--         }
