module Control.Monad.Error.Dynamic.Class where

import Control.Has



data Err e

type instance Has m (Err e ': xs) = (MonadError e m, Has m xs)


class Monad m => MonadError e m where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a


liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError pure
