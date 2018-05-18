{-# LANGUAGE BangPatterns, DeriveFoldable, DeriveTraversable, RankNTypes #-}

{- |
   Module      : LambdaJAM.Streaming.Exercise1
   Description : Exercise 0
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   This exercise focuses on how Stream differs from Pipe and Conduit.

 -}
module LambdaJAM.Streaming.Exercise1 where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Foldable             as F

-- For Task 3
import           Data.Conduit      (ConduitM, await, runConduit, transPipe,
                                    (.|))
import qualified Data.Conduit.List as CL

--------------------------------------------------------------------------------

{-

This is an inlined variant of @Stream (Of a) m r@ with a subset of the
functionality made available.

-}

data IStream a m r = IStep (a, IStream a m r)
                   | IEffect (m (IStream a m r))
                   | IReturn r

instance (Monad m) => Functor (IStream a m) where
  fmap f = go
    where
      go stream = case stream of
                    IStep g   -> IStep (fmap go g)
                    IEffect m -> IEffect (m >>= return . go)
                    IReturn r -> IReturn (f r)

instance (Monad m) => Applicative (IStream a m) where
  pure = IReturn

  streamf <*> streamx = do
    f <- streamf
    x <- streamx
    return (f x)

instance (Monad m) => Monad (IStream a m) where
  stream >>= f = go stream
    where
      go str = case str of
                 IStep g   -> IStep (fmap go g)
                 IEffect m -> IEffect (fmap go m)
                 IReturn r -> f r

  fail = lift . fail

instance MonadTrans (IStream a) where
  lift = IEffect . fmap IReturn

instance (MonadIO m) => MonadIO (IStream a m) where
  liftIO = lift . liftIO

inlineFold :: (Monad m) => (x -> a -> x) -> x -> (x -> b) -> IStream a m r -> m (b, r)
inlineFold step begin done str = go str begin
  where
    go stream !x = case stream of
                     IReturn r       -> return (done x, r)
                     IEffect m       -> m >>= \str' -> go str' x
                     IStep (a, rest) -> go rest $! step x a

inlineToList :: (Monad m) => IStream a m r -> m ([a], r)
inlineToList = inlineFold (\diff a ls -> diff (a:ls)) id ($[])

listToInline :: (Monad m, F.Foldable f) => f a -> IStream a m ()
listToInline = F.foldr (\a p -> IStep (a, p)) (IReturn ())

iMapM_ :: (Monad m) => (a -> m b) -> IStream a m r -> m r
iMapM_ f = go
  where
    go stream = case stream of
                  IStep (a,str') -> f a >> go str'
                  IEffect m      -> m >>= go
                  IReturn r      -> return r

printIStream :: (Show a, MonadIO m) => IStream a m r -> m r
printIStream = iMapM_ (liftIO . print)

--------------------------------------------------------------------------------


{-

Task 1:

When dealing with lists, a common operation is to split the list at a specified index:

@

  splitAt :: Int -> [a] -> ([a], [a])

@

such that the length of the first returned list is equal (or less than
if the length of the input list is less than) the specified @Int@
parameter:

@

  splitAt 3  [1..10] = ([1,2,3],[4,5,6,7,8,9,10])

  splitAt 11 [1..10] = ([1,2,3,4,5,6,7,8,9,10],[])

@

Define such a function on 'IStream's.

Consider: what would be the appropriate return type? (Remember that
@IStream a m r@ is roughly analogous to @m ([a], r)@!).

Test it out!  'printIStream' should come in handy.

What are the consequences of the type you chose?

-}

--------------------------------------------------------------------------------

{-

Motivating discussion:

Rather than just taking /a/ list of @n@ elements from the front of the
list, we often want to split an entire list into sub-lists of size no
greater than @n@ (allowing for the last sub-list to be shorter).

How can we express this using 'IStream'?

One solution may be to have a function with a type like:

@
  chunksOf :: (Monad m) => Int -> IStream a m r -> IStream [a] m r
@

but this is clearly sub-optimal: we are no longer able to capture the
streaming nature of the original data, and must periodically wait to
collect enough values to construct a list.

One attempt to resolve this might be this slight variation:

@
  chunksOf :: (Monad m) => Int -> IStream a m r -> IStream (IStream a m r) m r
@

Except... where do we get value for the @r@ returned by each
sub-stream?  If we get the final value of @r@ then there's no point in
streaming; we can't create one as there's no @Monoid@ or similar
constraint on it.

First of all, let's realise that it doesn't /have/ to be of type @r@!
This type makes just as much sense:

@
  chunksOf :: (Monad m) => Int -> IStream a m r -> IStream (IStream a m v) m r
@

We still can't actually create these values... so let's denote that:

@
  -- Type alias for clarity
  type SubIStream = IStream

  chunksOf :: (Monad m) => Int -> IStream a m r -> IStream (forall v. SubIStream a m v) m r
@

Now, every time we have a new @SubIStream@, the actual value of the outer @IStream@ is:

@
  IStep (forall v. SubIStream a m v, IStream b m r)

  -- b ~ (forall v. SubIStream a m v)
@

In actual usage, that inner @IStream@ /really/ shouldn't be accessed
or used until we've evaluated that @SubIStream@.  So why not kill two
birds with one stone: ensure we can't access the rest of the @IStream@
by having it as the return value of the @SubIStream@!

@
  SubIStream a m (IStream b m r)

  = SubIStream a m (IStream (SubIStream a m (IStream b m r) m r)

  = SubIStream a m (IStream (SubIStream a m (IStream (SubIStream a m (IStream b m r) m r) m r) m r)
@

We obviously can't keep doing this indefinitely if we expect a usable type out of this.

Some of you may recognise this: it's reminiscent of the free monad transformer:

@
  -- The "free monad transformer" for a functor @f@
  newtype FreeT f m a = FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }

  -- The base functor for a free monad.
  data FreeF f a b = Pure a | Free (f b)
@

So we should be able to define a FreeT-inspired Stream datatype, and
to actually carry a payload, we can use the left-strict pair.

-}

data FStream f m r = FStep (f (FStream f m r))
                   | FEffect (m (FStream f m r))
                   | FReturn r

-- | A left-strict pair; the base functor for streams of individual elements.
data Of a b = !a :> b
    deriving (Eq, Foldable, Ord,
              Read, Show, Traversable)

infixr 5 :>

instance Functor (Of a) where
  fmap f (a :> x) = a :> f x

fYield :: a -> FStream (Of a) m ()
fYield a = FStep (a :> FReturn ())

fCons :: a -> FStream (Of a) m r -> FStream (Of a) m r
fCons a str = FStep (a :> str)

{-

(Actually, the @Stream@ data type grew out of "attempt to implement
`FreeT` in the style of `Pipes.Internal`".)

We could then say:

@
  type IStream a = FStream (Of a)
@

-}

--------------------------------------------------------------------------------

{-

Task 2:

Port the various instances and functions (including your @splitAt@
function!) to 'FStream'.

After that, implement a @chunksOf@ function.

Think about the actual minimum type required for both @splitAt@ and
@chunksOf@.

Test it: can you take a list, convert it to a stream, chunk it, then
convert each sub-stream into a list and print it?  The following
function may be useful:

-}

-- Compared to @mapM@, this transforms the functor rather than any
-- values stored within that functor (though the transformation
-- function may modify the values).
mapsM :: (Monad m, Functor f) => (forall x. f x -> m (g x))
         -> FStream f m r -> FStream g m r
mapsM phi = go
  where
    go str = case str of
               FStep f   -> FEffect (fmap FStep (phi (fmap go f)))
               FEffect m -> FEffect (fmap go m)
               FReturn r -> FReturn r

-- A traditional @mapM@-style function implemented using 'mapsM'.
fMapM :: (Monad m) => (a -> m b) -> FStream (Of a) m r -> FStream (Of b) m r
fMapM f = mapsM (\(a :> x) -> (:> x) <$> f a)

--------------------------------------------------------------------------------

{-

Motivating discussion:

(If you don't have experience with more "traditional" data streaming
libraries like /pipes/ or /conduit/, you may wish to skip this
section.  However, it will give you an idea of alternate ways of
approaching this problem.)

As you've hopefully seen (but will be explored in another exercise in
more depth), the @Stream@ type is (if you squint hard enough) kind of
like a traditional list interspersed with Monadic actions (which are
typically to obtain the next value).

Just about every other library that aims to solve the data streaming
problem in Haskell tends to do it using more of an automata/state
machine approach where they accept both inputs and outputs (and in the
case of pipes also allows for sending data back downstream!).  This
means some fundamental differences in how you approach and use these:

* In essence, a pipe or conduit is a /function/ that transforms inputs
  to outputs; with Streams we tend to denote this using an actual
  function.

* As such, pipes and conduits have their own set of functions and
  operators for composing them (@>->@ and @.|@ respectively for the
  simple cases).  With Streams, we use standard function composition.

* Furthermore, you typically have a need for some kind of @runPipe@ or
  @runConduit@ function to actually feed in inputs, etc.  Whereas with
  a Stream you'll use a function to create a Stream, then at the end
  use another (e.g. @mapM_@) to process one.

* At least in my experience, the downstream capabilities of pipes
  tends not to be very useful (typically setting @a'@ and @b'@ to
  @()@).

* In many ways, a Stream is more like a @Producer@ or @Source@ in
  pipes and conduit.

-}

--------------------------------------------------------------------------------

{-

Task 3:

Implement the following FStream <-> Conduit functions to get an idea
how they compare to each other.

These are all taken from streaming-conduit:
https://hackage.haskell.org/package/streaming-conduit (If you can
think of better definitions, please provide a pull request!)

-}

-- | Convert a Stream to a Producer-style 'ConduitM'.
--
--   Hint: try using 'CL.unfoldEitherM'.
fromFStream :: (Monad m) => FStream (Of o) m r -> ConduitM i o m r
fromFStream = error "fromFStream"

-- toFStream is a little more involved, so will provide it for you.
-- Uncomment the definition when you've defined instances for FStream.

-- | Convert a Producer-style Conduit to a 'FStream'.
toFStream :: (Monad m) => ConduitM () o m () -> FStream (Of o) m ()
toFStream = error "toFStream"
-- toFStream cnd = runConduit (transPipe lift cnd .| CL.mapM_ fYield)

asStream :: (Monad m) => ConduitM i o m () -> FStream (Of i) m () -> FStream (Of o) m ()
asStream = error "asStream"

-- This one is very manual...
asConduit :: (Monad m) => (FStream (Of i) m () -> FStream (Of o) m r) -> ConduitM i o m r
asConduit = error "asConduit"
