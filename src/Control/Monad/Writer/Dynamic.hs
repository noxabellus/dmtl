module Control.Monad.Writer.Dynamic
    ( module X
    , WriterT(..)
    , runWriterT
    , execWriterT
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Dynamic
import Control.Monad.Writer.Dynamic.Class as X
import Control.Monad.Reader.Dynamic.Class
import Control.Monad.State.Dynamic.Class
import Control.Monad.Error.Dynamic.Class

import Data.Functor.Contravariant

import Data.Bifunctor


newtype WriterT w m a = WriterT (m (a, w))


instance Functor m => Functor (WriterT w m) where
    fmap f (WriterT m) = WriterT (fmap (first f) m)

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
    pure a = WriterT $ pure (a, mempty)
    WriterT mf <*> WriterT ma = WriterT do
        (f, w) <- mf
        (a, w') <- ma
        pure (f a, w <> w')

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    WriterT ma >>= f = WriterT do
        (a, w) <- ma
        (b, w') <- runWriterT (f a)
        pure (b, w <> w')

instance Monoid w => MonadTrans (WriterT w) where
    lift ma = WriterT do
        a <- ma
        pure (a, mempty)

instance (Monoid w, MonadFail m) => MonadFail (WriterT w m) where
    fail = lift . fail

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
    liftIO = lift . liftIO

instance (Monoid w, Monad m, Alternative m) => MonadPlus (WriterT w m)
instance (Monoid w, Monad m, Alternative m) => Alternative (WriterT w m) where
    empty = WriterT empty
    WriterT ma <|> WriterT mb = WriterT (ma <|> mb)

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
    mfix m = WriterT do
        mfix \ ~(a, _) -> runWriterT (m a)

instance Contravariant m => Contravariant (WriterT w m) where
    contramap f = WriterT . contramap (first f) . runWriterT

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
    state = lift . state

instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
    ask = lift ask
    local f (WriterT m) = WriterT (local f m)

instance (Monoid w, MonadError e m) => MonadError e (WriterT w m) where
    throwError = lift . throwError
    catchError (WriterT m) f = WriterT (catchError m (runWriterT . f))

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
    listen (WriterT m) = WriterT do
        (a, w) <- m
        pure ((a, w), w)
    pass f (WriterT m) = WriterT (second f <$> m)

instance {-# OVERLAPPABLE #-} (Monoid w', MonadWriter w m)
    => MonadWriter w (WriterT w' m) where
        listen (WriterT m) = WriterT do
            ((a, w'), w) <- listen @w m
            pure ((a, w), w')
        pass f (WriterT m) = WriterT do
            (a, w') <- pass @w f m
            pure (a, w')

runWriterT :: WriterT w m a -> m (a, w)
runWriterT (WriterT m) = m

execWriterT :: Functor m => WriterT w m a -> m w
execWriterT = fmap snd . runWriterT
