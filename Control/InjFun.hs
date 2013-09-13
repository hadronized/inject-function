module Control.InjFun (
    -- * Inject function
    InjFun
  , cfapply
  , inject
    -- * Sequencing, exploding and merging
  , (|->)
  , (||->)
  , explode
  , merge
  ) where

-- |Function able to be injected parameters in.
-- `c` is the injected control parameters, `i` represents its input, `m` is the resulting monad
-- and `o` is the output.
newtype InjFun c i m o = InjFun {
  -- |Feed a `InjFun` with its regular parameters and injected parameters.
  cfapply :: c -> i -> m o
  }

-- |Create an inject function.
inject :: (c -> i -> m o) -> InjFun c i m o
inject f = InjFun f

-- |Sequencing operator. It’s a helper function that composes with `>>=` the two `InjFun`, respecting
-- the order. That version (with a single `|`) means that both the two injected parameters are considered
-- the same; then they’re shared as a single `c`.
(|->) :: (Monad m) => InjFun c i m o  -- ^ First function
                   -> InjFun c o m o' -- ^ Second function
                   -> InjFun c i m o' -- ^ Resulting sequencing function
f |-> g = InjFun $ \c i -> cfapply f c i >>= cfapply g c

-- |Sequencing operator. It’s a helper function that composes with `>>=` the two `InjFun`, respecting
-- the order. That version (with double `|`) means that the two injected parameters are considered
-- different.
(||->) :: (Monad m) => InjFun c i m o       -- ^ First function
                    -> InjFun c' o m o'     -- ^ Second function
                    -> InjFun (c,c') i m o' -- ^ Resulting sequencing function
f ||-> g = InjFun $ \(c,c') i -> cfapply f c i >>= cfapply g c'

-- |Explode an `InjFun` that outputs two values into two other `InjFun`.
explode :: (Monad m) => InjFun c i m (o0,o1)              -- ^ Function to explode
                     -> (InjFun c i m o0,InjFun c i m o1) -- ^ Exploded functions
explode f = (f',f'')
  where f'  = cf fst
        f'' = cf snd
        cf sel = InjFun $ \c i -> cfapply f c i >>= return . sel

-- |Merge two `InjFun` into one.
merge :: (Monad m) => InjFun c i m o                -- ^ First function
                   -> InjFun c' i' m o'             -- ^ Second function
                   -> InjFun (c,c') (i,i') m (o,o') -- ^ Merged function
merge f g = fg
  where fg = InjFun $ \(c,c') (i,i') -> do
                r0 <- cfapply f c i
                r1 <- cfapply g c' i'
                return (r0,r1)
