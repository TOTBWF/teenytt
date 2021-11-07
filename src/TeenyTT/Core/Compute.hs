module TeenyTT.Core.Compute
  ( Compute
  , runCompute
  , MonadCompute(..)
  , getGlobal
  , emit
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import TeenyTT.Core.Pretty

import TeenyTT.Core.Env (Env, Level)
import TeenyTT.Core.Env qualified as Env
import TeenyTT.Core.Error (Error)

import TeenyTT.Core.Domain qualified as D

-- | The Semantic Manipulation Monad.
-- This provides the "least amount of structure" we need to perform any sort
-- of manipulation of semantic values (IE: instantiating closures, etc).
newtype Compute a = Compute { unCompute :: ReaderT (Env (D.Value, D.Type)) (ExceptT Error IO) a }
    deriving (Functor, Applicative, Monad)

runCompute :: Env (D.Value, D.Type) -> Compute a -> IO (Either Error a)
runCompute globals (Compute m) = runExceptT $ runReaderT m globals

class (Monad m) => MonadCompute m where
    -- | Lift a 'CmpM' into the monad 'm'.
    liftCompute :: Compute a -> m a

    -- | Abort the current computation with an error.
    -- This has a default implementation, but we allow for overloading
    -- so that other monads can add extra information regarding the failure
    -- if possible.
    failure :: Error -> m a
    failure err = liftCompute $ Compute $ throwError err

instance MonadCompute Compute where
  liftCompute m = m

instance (MonadCompute m) => MonadCompute (ReaderT r m) where
    liftCompute m = ReaderT $ \_ -> liftCompute m

getGlobal :: (MonadCompute m) => Level -> m (D.Value, D.Type)
getGlobal lvl = liftCompute $ Compute $ asks (Env.level lvl)

emit :: (MonadCompute m) => Doc ann -> m ()
emit doc = liftCompute $ Compute $ liftIO $ do
    putDoc doc
    putStrLn ""
