module TeenyTT.Core.Compute
  ( CmpM
  , runCmp
  , MonadCmp(..)
  , getGlobal
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import TeenyTT.Core.Env (Env, Level)
import TeenyTT.Core.Env qualified as Env
import TeenyTT.Core.Error (Error)

import TeenyTT.Core.Domain qualified as D

-- | The Semantic Manipulation Monad.
-- This provides the "least amount of structure" we need to perform any sort
-- of manipulation of semantic values (IE: instantiating closures, etc).
newtype CmpM a = CmpM { unCmpM :: ReaderT (Env (D.Value, D.Type)) (Except Error) a }
    deriving (Functor, Applicative, Monad)

runCmp :: Env (D.Value, D.Type) -> CmpM a -> Either Error a
runCmp globals (CmpM m) = runExcept $ runReaderT m globals

class (Monad m) => MonadCmp m where
    -- | Lift a 'CmpM' into the monad 'm'.
    liftCmp :: CmpM a -> m a

    -- | Abort the current computation with an error.
    -- This has a default implementation, but we allow for overloading
    -- so that other monads can add extra information regarding the failure
    -- if possible.
    failure :: Error -> m a
    failure err = liftCmp $ CmpM $ throwError err

instance MonadCmp CmpM where
  liftCmp m = m

instance (MonadCmp m) => MonadCmp (ReaderT r m) where
    liftCmp m = ReaderT $ \_ -> liftCmp m

getGlobal :: (MonadCmp m) => Level -> m (D.Value, D.Type)
getGlobal lvl = liftCmp $ CmpM $ asks (Env.level lvl)
