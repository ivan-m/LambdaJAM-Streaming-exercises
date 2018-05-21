{-# LANGUAGE DeriveFunctor #-}

{- |
   Module      : LambdaJAM.Streaming.Exercise0
   Description : Exercise 0
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   In this exercise, you will build up a simplistic version of the
   Stream data type and functions for it.

 -}
module LambdaJAM.Streaming.Exercise0 where

import           Data.Bifunctor (first)
import qualified Data.Foldable  as F

--------------------------------------------------------------------------------

{-

As it currently stands, the 'Stream' data type here is isomorphic to
@([a],r)@.

  * @a@ is the elements of the 'Stream'

  * @r@ is the final result from running the 'Stream'.

-}
data Stream a r = Step a (Stream a r)
                | Return r
  deriving (Functor)

--------------------------------------------------------------------------------

{-

Task 1:

Implement 'streamToList' and 'listToStream'.  These will serve as our
initial methods of creating/running 'Stream's.

-}

-- Renamed so there won't be clashes below.

streamToList1 :: Stream a r -> ([a], r)
streamToList1 = go
  where
    go (Step a str) = first (a:) (go str)
    go (Return r)   = ([], r)


listToStream1 :: [a] -> Stream a ()
listToStream1 = foldr Step (Return ())

--------------------------------------------------------------------------------

{-

Task 2:

As this current 'Stream' implementation is isomorphic to a list, it
doesn't actually serve any purpose.  The true power of the 'Stream'
type comes about when we allow a /monadic effect/.

Augment the 'Stream' datatype to add in the ability to have a monadic
action returning another 'Stream'.

The canonical type parameters for this are @Stream a m r@.

Make sure to update 'streamToList' and 'listToStream'!

Ideally you would also be able to define 'Applicative' and 'Monad'
instances for 'Stream'.

-}

-- Using a different name to make the progression more obvious.

data MStream a m r = MStep a (MStream a m r)
                   | MEffect (m (MStream a m r))
                   | MReturn r
  deriving (Functor)

instance (Monad m) => Applicative (MStream a m) where
  pure = MReturn

  streamf <*> streamx = do
    f <- streamf
    x <- streamx
    return (f x)

instance (Monad m) => Monad (MStream a m) where
  stream >>= f = go stream
    where
      go (MStep a str) = MStep a (go str)
      go (MEffect m)   = MEffect (fmap go m)
      go (MReturn r)   = f r

streamToList2 :: (Monad m) => MStream a m r -> m ([a], r)
streamToList2 = go
  where
    go (MStep a str) = first (a:) <$> go str
    go (MEffect m)   = m >>= go
    go (MReturn r)   = return ([], r)

listToStream2 :: [a] -> MStream a m ()
listToStream2 = foldr MStep (MReturn ())

--------------------------------------------------------------------------------

{-

Task 3:

We typically do not want to pattern match every time we interact with
a 'Stream'; instead, it would be convenient to have what we expect
from many of our functional data structures: a folding function:

> foldStream :: (Monad m) => (x -> a -> x) -> x -> (x -> b) -> Stream a m r -> m (b, r)

(The @(x -> b)@ parameter will often be the 'id' function; i.e. @x ~ b@.)

You may wish to re-implement 'streamToList' to use this function (and
possibly make 'listToStream' take a 'Foldable').

Try using this function to implement Stream analogues of functions
like 'length'.

-}

foldStream :: (Monad m) => (x -> a -> x) -> x -> (x -> b) -> MStream a m r -> m (b, r)
foldStream step begin done = go begin
  where
    go x str = case str of
                 MStep a str' -> go (step x a) str'
                 MEffect m    -> m >>= go x
                 MReturn r    -> return (done x, r)

-- This uses implicit difference lists.
streamToList3 :: (Monad m) => MStream a m r -> m ([a], r)
streamToList3 = foldStream (\diff a ls -> diff (a:ls)) id (\diff -> diff [])

listToStream3 :: (F.Foldable f) => f a -> MStream a m ()
listToStream3 = F.foldr MStep (MReturn ())

mLength :: (Monad m) => MStream a m r -> m (Int, r)
mLength = foldStream (\x _ -> x + 1) 0 id

--------------------------------------------------------------------------------

{-

Task 4:

We typically want to transform our streamed values.  Try defining
Stream analogues of 'map', 'mapM' and 'mapM_' (you may wish to define
a monadic fold function as well).

Use these to do the equivalent of:

> mapM_ print . map (*2) $ [1..10]

-}

mMap :: (Monad m) => (a -> b) -> MStream a m r -> MStream b m r
mMap f = go
  where
    go (MStep a str) = MStep (f a) (go str)
    go (MEffect m)   = MEffect (fmap go m)
    go (MReturn r)   = MReturn r

mMapM :: (Monad m) => (a -> m b) -> MStream a m r -> MStream b m r
mMapM f = go
  where
    go (MStep a str) = MEffect (f a >>= \b -> return (MStep b (go str)))
    go (MEffect m)   = MEffect (fmap go m)
    go (MReturn r)   = MReturn r

mMapM_ :: (Monad m) => (a -> m b) -> MStream a m r -> m r
mMapM_ f = go
  where
    go (MStep a str) = f a >> go str
    go (MEffect m)   = m >>= go
    go (MReturn r)   = return r

task4 :: IO ()
task4 = mMapM_ print . mMap (*2) . listToStream3 $ [(1::Int) .. 10]
