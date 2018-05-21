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

streamToList :: Stream a r -> ([a], r)
streamToList = error "streamToList"

listToStream :: [a] -> Stream a ()
listToStream = error "listToStream"

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

--------------------------------------------------------------------------------

{-

Task 4:

We typically want to transform our streamed values.  Try defining
Stream analogues of 'map', 'mapM' and 'mapM_' (you may wish to define
a monadic fold function as well).

Use these to do the equivalent of:

> mapM_ print . map (*2) $ [1..10]

-}
