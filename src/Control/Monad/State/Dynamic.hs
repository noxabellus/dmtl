module Control.Monad.State.Dynamic
    ( module X
    , StateT(..)
    , runStateT, runStateT'
    , evalStateT, evalStateT'
    , execStateT, execStateT'
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Dynamic
import Control.Monad.State.Dynamic.Class as X
import Control.Monad.Reader.Dynamic.Class
import Control.Monad.Writer.Dynamic.Class
import Control.Monad.Error.Dynamic.Class

import Data.Functor.Contravariant
import Data.Bifunctor



newtype StateT s m a = StateT (s -> m (a, s))

instance Functor m => Functor (StateT s m) where
    fmap f (StateT g) = StateT (fmap (first f) . g)

instance Monad m => Applicative (StateT s m) where
    pure a = StateT \s -> pure (a, s)
    StateT mf <*> StateT ma = StateT \s -> do
        (f, s') <- mf s
        (a, s'') <- ma s'
        pure (f a, s'')

instance Monad m => Monad (StateT s m) where
    StateT ma >>= f = StateT \s -> do
        (a, s') <- ma s
        runStateT (f a) s'

instance MonadTrans (StateT s) where
    lift ma = StateT \s -> do
        a <- ma
        pure (a, s)

instance MonadFail m => MonadFail (StateT s m) where
    fail = lift . fail

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO

instance (Monad m, Alternative m) => MonadPlus (StateT s m)
instance (Monad m, Alternative m) => Alternative (StateT s m) where
    empty = StateT (const empty)
    StateT ma <|> StateT mb = StateT \s -> ma s <|> mb s

instance MonadFix m => MonadFix (StateT s m) where
    mfix f = StateT \s -> mfix \ ~(a, _) -> runStateT (f a) s

instance Contravariant m => Contravariant (StateT s m) where
    contramap f m = StateT do
      contramap (first f) . runStateT m

instance MonadReader r m => MonadReader r (StateT s m) where
    ask = lift ask
    local f (StateT m) = StateT (local f . m)

instance MonadWriter w m => MonadWriter w (StateT s m) where
    listen (StateT m) = StateT \s -> do
        ((a, s'), w) <- listen (m s)
        pure ((a, w), s')
    pass f (StateT m) = StateT (pass f . m)

instance MonadError e m => MonadError e (StateT s m) where
    throwError = lift . throwError
    catchError (StateT m) f = StateT \s ->
        catchError (m s) \e -> runStateT (f e) s

instance Monad m => MonadState s (StateT s m) where
    state f = StateT (pure . f)

instance {-# OVERLAPPABLE #-} MonadState s m => MonadState s (StateT s' m) where
    state f = lift (state f)


runStateT :: StateT s m a -> s -> m (a, s)
runStateT (StateT f) = f

runStateT' :: s -> StateT s m a -> m (a, s)
runStateT' = flip runStateT

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT m s = fst <$> runStateT m s

evalStateT' :: Functor m => s -> StateT s m a -> m a
evalStateT' = flip evalStateT

execStateT :: Functor m => StateT s m a -> s -> m s
execStateT m s = snd <$> runStateT m s

execStateT' :: Functor m => s -> StateT s m a -> m s
execStateT' = flip execStateT
