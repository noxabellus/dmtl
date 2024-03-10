module Control.Monad.State.Dynamic.Class where

import Control.Has



data St s

type instance Has m (St s ': xs) = (MonadState s m, Has m xs)


class Monad m => MonadState s m where
    state :: (s -> (a, s)) -> m a

get :: MonadState s m => m s
get = state \s -> (s, s)

gets :: MonadState s m => (s -> a) -> m a
gets f = state \s -> (f s, s)

put :: MonadState s m => s -> m ()
put s = state (const ((), s))

modify :: MonadState s m => (s -> s) -> m ()
modify f = state \s -> ((), f s)
