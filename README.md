# dmtl

`mtl` without the functional dependencies; more complete than `mtl-unleashed`

Other differences to mtl:
+ `ExceptT` -> `ErrorT`
+ `pass :: m (a, w -> w) -> m a` -> `pass :: (w -> w) -> m a -> m a`
+ `fooTransformerT'` = `flip fooTransformerT`
+ No `Lazy` versions of transformers
+ Overlappable lifting instances
+ Overlappable `Alternative ErrorT`
+ Some missing boilerplate functions
