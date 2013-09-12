module InjFun (
    -- * Inject function
    InjFun(cfapply)
  , inject
    -- * Sequencing, exploding and merging
  , (|->)
  , explode
  , merge
  ) where

-- |Function able to be injected parameters in.
-- `i` represents its input, `c` is the injected control parameters, `m` is the resulting monad
-- and `o` is the output.
newtype InjFun i c m o = InjFun {
  cfapply :: i   -- ^ Regular parameters of the function
          -> c   -- ^ Injected control parameters
          -> m o -- ^ Output after injection and wrapped in a `Monad` 
  }

-- |Create an inject function.
inject :: (i -> c -> m o) -> InjFun i c m o
inject f = InjFun f

-- |Sequencing operator. It’s a helper function that composes with `>>=` the two `InjFun`, respecting
-- the order.
(|->) :: (Monad m) => InjFun i c m o       -- ^ First function
                   -> InjFun o c' m o'     -- ^ Second function
                   -> InjFun i (c,c') m o' -- ^ Resulting sequencing function
f |-> g = InjFun $ \i (c,c') -> do
  r0 <- cfapply f i c
  cfapply g r0 c'

-- |Explode an `InjFun` that outputs two values into two other `InjFun`.
explode :: (Monad m) => InjFun i c m (o0,o1)              -- ^ Function to explode
                     -> (InjFun i c m o0,InjFun i c m o1) -- ^ Exploded functions
explode f = (f',f'')
  where f'  = cf fst
        f'' = cf snd
        cf sel = InjFun $ \i c -> cfapply f i c >>= return . sel

-- |Merge two `InjFun` into one.
merge :: (Monad m) => InjFun i c m o                -- ^ First function
                   -> InjFun i' c' m o'             -- ^ Second function
                   -> InjFun (i,i') (c,c') m (o,o') -- ^ Merged function
merge f g = fg
  where fg = InjFun $ \(i,i') (c,c') -> do
                r0 <- cfapply f i c
                r1 <- cfapply g i' c'
                return (r0,r1)
