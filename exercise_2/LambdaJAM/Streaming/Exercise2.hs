{-# OPTIONS_GHC -Wno-unused-imports #-}

{- |
   Module      : LambdaJAM.Streaming.Exercise2
   Description : Exercise 2
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   The different ways Streams can compose.

 -}
module LambdaJAM.Streaming.Exercise2 where

-- This is a common way of importing the streaming library; note that
-- the two modules do not have conflicting exports (excluding
-- re-exports).

-- Import the data structures.
import Streaming.Prelude (Of(..), Stream)

-- The "Streaming" module contains definitions for any functor `f`.
-- It usually does /not/ need to be imported qualified but can be
-- useful to do so.
import qualified Streaming as S

-- The "Streaming.Prelude" module offers functions that specialise
-- more on the 'Of' functor.  They typically have the same names (and
-- similar arguments) to functions in the "Prelude" and "Data.List".
import qualified Streaming.Prelude as S

-- Other imports
import Control.Monad.IO.Class (MonadIO)

--------------------------------------------------------------------------------

{-

Naming Conventions:

* * *

If you look through "Streaming" and "Streaming.Prelude" you will see
some similarly named functions.  For example:

@
  *LambdaJAM.Streaming.Exercise2> :i S.maps
  S.maps ::
    (Monad m, Functor f) =>
    (forall x. f x -> g x) -> Stream f m r -> Stream g m r
          -- Defined in `Streaming.Internal'

  *LambdaJAM.Streaming.Exercise2> :i S.map
  S.map ::
    Monad m => (a -> b) -> Stream (Of a) m r -> Stream (Of b) m r
          -- Defined in `Streaming.Prelude'
@

Functions that operate on any 'Functor' (such as 'S.maps') tend to
have an extra @s@ in their name (see also 'S.mapsM', the monadic
version).  Those without usually operate on @Stream (Of a)@ (thus
allowing for more targeted functions).

* * *

In "Streaming.Prelude", some functions that "run" streams typically have two variants:

1. A plain variant that returns an 'Of' (e.g. 'S.sum').

2. A variant that returns only the first component of the 'Of'
   appended with an underscore (e.g. 'S.sum_').

The first variant is mostly useful when processing sub-streams (see
Task 2).

-}

--------------------------------------------------------------------------------

{-

Comparing to previous exercises:

The previous exercises typically had you implement a @toList@ and
@fromList@ function.  Whilst 'S.toList'/'S.toList_' are there,
@fromList@ is actually named 'S.each'.

-}

--------------------------------------------------------------------------------

{-

Task 1:

As we saw in our implementation of 'S.splitAt', it is possible to have
one 'Stream' return another 'Stream'.  These are typically for
functions that split the 'Stream' in one place: 'S.splitAt', 'S.span',
etc.

To use the results of these, you will process the first 'Stream'
(using a function like 'S.mapM_' or 'S.effects'.

Try this out by converting @[1..10]@ to a 'Stream', splitting it at
some point (e.g. when @<5@ fails) and printing both components.

What do you expect the result to be?  What did you actually get?  Do
you understand why?

-}

--------------------------------------------------------------------------------

{-

Task 2:

'S.chunksOf' contains a 'Stream' as the Functor /within/ a 'Stream.
This allows us to process individual groupings without waiting to
collate values.

The way this is typically done in other stream-processing libraries is
either:

1. Accumulating values into a list (thus breaking the abstraction).

2. Explicit usage of @FreeT@ (remember from Exercise 1, 'Stream' can
   be seen as a specialised variant of @FreeT@); this is the approach
   taken by the @pipes-group@ library.  The problem with this approach
   is that you have to mix two different abstractions, possibly making
   it more difficult.

Other functions also use this when they need to provide a mechanism
for processing breaking a single Stream into multiple sub-Streams.

To practice with this usage of nested 'Stream's, take the provided
list and break it into sub-streams where each element is greater than
the first element.  Once you've done so:

a) Convert each sub-stream to a list and print all the lists.

b) Concate the sub-streams together and ensure you have the original
   stream again.

Hint: there is a similar example in "Streaming.Prelude"; try searching
through there to find it.

-}

task2Source :: (Monad m) => Stream (Of Int) m ()
task2Source = S.each [1,2,1,3,2,1,4,3,2,1]

{-

There are two things you can consider as an extension to this:

1. @Stream (Of (Stream f m v)) n r@: I don't think this is ever used
   as it isn't very practical (why bother putting the sub-stream
   inside an 'Of'?).

2. The @streaming-bytestring@ library provides a streaming @ByteString
   m r@ type; as such you typically have functions like @Stream
   (ByteString m) n r@.

-}

--------------------------------------------------------------------------------

{-

Task 3:

The final way Streams can be embedded into other Streams is by having
them the monad that a Stream runs in be /another/ Stream.

This is probably the most confusing of the various options:

1. Why use it?

2. How do you use it?

3. What can you expect in terms of the order of the results?

Putting aside the first question for now, consider the 'S.copy' function:

@
  S.copy
    :: Monad m =>
       Stream (Of a) m r -> Stream (Of a) (Stream (Of a) m) r
@

This duplicates every value and allows you to process it /twice/.

Consider the following code (don't try evaluating it yet!):

-}

task3Example :: IO ()
task3Example = processInnerStream
               . processOuterStream
               . S.copy
               $ S.each [1..10]
  where
    -- Note: we don't care what the Monad is!
    processOuterStream :: (MonadIO m) => Stream (Of Int) m () -> m ()
    processOuterStream = S.print

    processInnerStream :: Stream (Of Int) IO () -> IO ()
    processInnerStream = S.print . S.map show

{-

What do you expect the output of running this to be?

Run it.  Are you surprised? (Actually, I was slightly surprised;
admittedly, I tend not to use 'S.copy' much, or any of these types of
nested Streams where I print them both.)

Can you understand why this occurs?

* * *

In terms of typical usage of this behaviour: the most common is
probably with functions like 'S.partition':

@
  S.partition
    :: Monad m =>
       (a -> Bool)
       -> Stream (Of a) m r -> Stream (Of a) (Stream (Of a) m) r
@

Consider this scenario:

1. You read data in (from file, a database, etc.).

2. Some of the records are not suitable for your use cases; use
   'S.partition' to sort them out (typically the predicate is used to
   select the error cases).

3. Handle the outer 'Stream' (that is, the cases that satisfied the predicate).

4. Handle the inner 'Stream' (that is, the cases that failed the predicate).

This allows you to - in a streaming and efficient fashion - handle two
distinct cases completely separately with very clean code!

* * *

Some pointers:

* Have the cases that are the easiest to handle first (satisfy the
  predicate, a 'Left' value with 'S.partitionEithers', etc.).  Process
  these first.

* Process the outer 'Stream' immediately; don't be tempted to try and
  use functions like @hoist@ from the @mmorph@ package as they don't
  work well (due to 'Stream' being fundamentally
  continuation-passing-style).

* For sanity reasons, try not to have too many nested Streams at any
  one time (process the outer Stream first, then partition again if
  need be).

-}
