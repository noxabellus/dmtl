module Control.Has where

import Data.Kind
import Control.Applicative
import Control.Monad



type family Has (m :: Type -> Type) (xs :: [Type]) :: Constraint

type instance Has m '[] = ()


data Alt

type instance Has m (Alt ': xs) = (Alternative m, MonadPlus m, Has m xs)


data Fail

type instance Has m (Fail ': xs) = (MonadFail m, Has m xs)


data With (c :: [Constraint])

type instance Has m (With (c ': cs) ': xs) = (c, Has m (With cs ': xs))
type instance Has m (With '[] ': xs) = Has m xs
