module Control.Monad.Writer.Dynamic.Class where

import Control.Has



data Wr w

type instance Has m (Wr w ': xs) = (MonadWriter w m, Has m xs)


class (Monad m, Monoid w) => MonadWriter w m where
    listen :: m a -> m (a, w)
    pass :: (w -> w) -> m a -> m a



tell :: MonadWriter w m => w -> m ()
tell w = pass (<> w) (pure ())
