# dmtl

`mtl` without the functional dependencies; more complete than `mtl-unleashed`

Other differences to mtl:
+ `Has` for constructing elaborate monadic contexts
+ `ExceptT` -> `ErrorT`
+ `pass :: m (a, w -> w) -> m a` -> `pass :: (w -> w) -> m a -> m a`
+ `fooTransformerT'` = `flip fooTransformerT`
+ `using :: MonadReader r m => r -> m a -> m a`
+ No `Lazy` versions of transformers
+ Overlappable lifting instances
+ Overlappable `Alternative ErrorT`
+ Some missing boilerplate functions
+ No `Identity`-wrapping aliases/functions
+ Does not re-export anything but own class files
