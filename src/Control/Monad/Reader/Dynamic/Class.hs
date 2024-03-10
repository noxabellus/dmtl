module Control.Monad.Reader.Dynamic.Class where

import Control.Has



data Rd r

type instance Has m (Rd r ': xs) = (MonadReader r m, Has m xs)


class Monad m => MonadReader r m where
    ask :: m r
    local :: (r -> r) -> m a -> m a

asks :: MonadReader r m => (r -> a) -> m a
asks = (<$> ask)
