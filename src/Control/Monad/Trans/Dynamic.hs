module Control.Monad.Trans.Dynamic where


class (forall m. Monad m => Monad (t m)) => MonadTrans t where
    lift :: Monad m => m a -> t m a
