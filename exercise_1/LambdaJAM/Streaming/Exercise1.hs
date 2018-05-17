{-# LANGUAGE BangPatterns #-}

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
    deriving (Data, Eq, Foldable, Ord,
              Read, Show, Traversable)

infixr 5 :>

instance Functor (Of a) where
  fmap f (a :> x) = a :> f x

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

-}

--------------------------------------------------------------------------------

{-

Task 3:

-}

--------------------------------------------------------------------------------

{-

-}
