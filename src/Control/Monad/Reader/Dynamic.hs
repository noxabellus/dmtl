module Control.Monad.Reader.Dynamic
    ( module X
    , ReaderT(..)
    , runReaderT, runReaderT'
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Zip
import Control.Monad.IO.Class
import Control.Monad.Trans.Dynamic
import Control.Monad.Reader.Dynamic.Class as X
import Control.Monad.State.Dynamic.Class
import Control.Monad.Writer.Dynamic.Class
import Control.Monad.Error.Dynamic.Class

import Data.Functor.Contravariant



newtype ReaderT r m a = ReaderT (r -> m a)

instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT g) = ReaderT (fmap f . g)

instance Applicative m => Applicative (ReaderT r m) where
    pure a = ReaderT \_ -> pure a
    ReaderT mf <*> ReaderT ma = ReaderT \r -> mf r <*> ma r

instance Monad m => Monad (ReaderT r m) where
    ReaderT ma >>= f = ReaderT \r -> do
        a <- ma r
        runReaderT (f a) r

instance MonadTrans (ReaderT r) where
    lift ma = ReaderT (const ma)

instance MonadFail m => MonadFail (ReaderT r m) where
    fail = lift . fail

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

instance MonadPlus m => MonadPlus (ReaderT r m)
instance Alternative m => Alternative (ReaderT r m) where
    empty = ReaderT (const empty)
    ReaderT ma <|> ReaderT mb = ReaderT \r -> ma r <|> mb r

instance (MonadFix m) => MonadFix (ReaderT r m) where
    mfix f = ReaderT \r -> mfix \a -> runReaderT (f a) r

instance MonadZip m => MonadZip (ReaderT r m) where
    mzipWith f (ReaderT m) (ReaderT n) = ReaderT \ a ->
        mzipWith f (m a) (n a)

instance Contravariant m => Contravariant (ReaderT r m) where
    contramap f = ReaderT . fmap (contramap f) . runReaderT

instance MonadState s m => MonadState s (ReaderT r m) where
    state = lift . state

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
    listen (ReaderT m) = ReaderT (listen . m)
    pass w (ReaderT m) = ReaderT (pass w . m)

instance MonadError e m => MonadError e (ReaderT r m) where
    throwError = lift . throwError
    catchError (ReaderT m) f = ReaderT \r ->
        catchError (m r) \e -> runReaderT (f e) r

instance Monad m => MonadReader r (ReaderT r m) where
    ask = ReaderT pure
    local f (ReaderT m) = ReaderT (m . f)

instance {-# OVERLAPPABLE #-} (MonadReader r m)
    => MonadReader r (ReaderT r' m) where
        ask = lift ask
        local f (ReaderT m) = ReaderT (local f . m)


runReaderT :: ReaderT r m a -> r -> m a
runReaderT (ReaderT m) = m

runReaderT' :: r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT
