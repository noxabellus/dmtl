module Control.Monad.Error.Dynamic
    ( module X
    , ErrorT(..)
    , runErrorT
    ) where




import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Control.Monad.IO.Class
import Control.Monad.Trans.Dynamic
import Control.Monad.Error.Dynamic.Class as X
import Control.Monad.Reader.Dynamic.Class
import Control.Monad.State.Dynamic.Class
import Control.Monad.Writer.Dynamic.Class


newtype ErrorT e m a = ErrorT (m (Either e a))

instance Functor m => Functor (ErrorT e m) where
    fmap f (ErrorT m) = ErrorT (fmap (fmap f) m)

instance Monad m => Applicative (ErrorT e m) where
    pure a = ErrorT (pure (Right a))
    ErrorT mf <*> ErrorT ma = ErrorT do
        mf >>= \case
            Left e -> pure (Left e)
            Right f ->
                ma >>= \case
                    Left e -> pure (Left e)
                    Right a -> pure (Right (f a))

instance Monad m => Monad (ErrorT e m) where
    ErrorT ma >>= f = ErrorT do
        ma >>= \case
            Left e -> pure (Left e)
            Right a -> runErrorT (f a)

instance MonadTrans (ErrorT e) where
    lift ma = ErrorT (fmap Right ma)

instance {-# OVERLAPPABLE #-} MonadFail m => MonadFail (ErrorT e m) where
    fail = lift . fail

instance MonadIO m => MonadIO (ErrorT e m) where
    liftIO = lift . liftIO

instance {-# OVERLAPPABLE #-} (Monoid e, Monad m) => MonadPlus (ErrorT e m)
instance {-# OVERLAPPABLE #-} (Monoid e, Monad m)
    => Alternative (ErrorT e m) where
        empty = ErrorT (pure (Left mempty))
        ErrorT ma <|> ErrorT mb = ErrorT do
            ma >>= \case
                Left _ -> mb
                Right a -> pure (Right a)

instance MonadZip m => MonadZip (ErrorT e m) where
    mzipWith f (ErrorT m) (ErrorT n) = ErrorT do
        mzipWith zipWithEither m n where
        zipWithEither (Left e) _ = Left e
        zipWithEither _ (Left e) = Left e
        zipWithEither (Right a) (Right b) = Right (f a b)

instance MonadReader r m => MonadReader r (ErrorT e m) where
    ask = lift ask
    local f (ErrorT m) = ErrorT (local f m)

instance MonadState s m => MonadState s (ErrorT e m) where
    state = lift . state

instance MonadWriter w m => MonadWriter w (ErrorT e m) where
    listen (ErrorT m) = ErrorT do
        (ea, w) <- listen m
        pure (ea >>= \a -> pure (a, w))
    pass f (ErrorT m) = ErrorT (pass f m)

instance Monad m => MonadError e (ErrorT e m) where
    throwError e = ErrorT (pure (Left e))
    catchError (ErrorT m) f = ErrorT do
        m >>= \case
            Left e -> runErrorT (f e)
            Right a -> pure (Right a)

instance {-# OVERLAPPABLE #-} (MonadError e m)
    => MonadError e (ErrorT e' m) where
        throwError = lift . throwError
        catchError (ErrorT m) f = ErrorT (catchError m (runErrorT . f))

runErrorT :: ErrorT e m a -> m (Either e a)
runErrorT (ErrorT m) = m
