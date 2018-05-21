{-# OPTIONS_GHC -Wno-unused-imports #-}

{- |
   Module      : LambdaJAM.Streaming.Exercise4
   Description : Exercise 4
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   Streaming I/O.

 -}
module LambdaJAM.Streaming.Exercise4 where

import Streaming.Prelude (Of(..), Stream)

import qualified Streaming         as S
import qualified Streaming.Prelude as S

import qualified Streaming.With as W

import           Data.ByteString.Streaming       (ByteString)
import qualified Data.ByteString.Streaming       as SB
import qualified Data.ByteString.Streaming.Char8 as SB8

--------------------------------------------------------------------------------

{-

Task 1:

As a continuation of treating 'Stream's as more powerful lists and to
do some basic native I/O handling, implement the following function.
You should /not/ perform any explicit looping.

Try it out!

(Hint: 'S.stdinLine'.)

-}

-- | Read the number of specified lines from standard input and print
--   them back out.
takeNLines :: Int -> IO ()
takeNLines = error "takeNLines"

--------------------------------------------------------------------------------

{-

Task 2:

In the @bytestring@ library, the lazy @ByteString@ type is implemented
as (fundamentally) a list of strict @ByteString@s.

If you completed Exercise 0, then the initial @Stream@ type there is -
if you specialise the @a@ to strict @ByteString@s - identical (up to
naming) to lazy @ByteString@s.

In that exercise, we added monadic effects to that list analogue.
This is basically what the streaming 'ByteString' type does
(constructor names based upon those of the actual lazy type):

@
  *LambdaJAM.Streaming.Exercise4> :i ByteString
  type role ByteString nominal nominal
  data ByteString (m :: * -> *) r
    = Data.ByteString.Streaming.Internal.Empty r
    | Data.ByteString.Streaming.Internal.Chunk {-# UNPACK #-}bytestring-0.10.8.2:Data.ByteString.Internal.ByteString
                                               (ByteString m r)
    | Data.ByteString.Streaming.Internal.Go (m (ByteString m r))
          -- Defined in `Data.ByteString.Streaming.Internal'

    <snip>
@

'ByteString' is a /specialisation/ of 'Stream', and the API of
"Data.ByteString.Streaming" and "Data.ByteString.Streaming.Char8" are
based heavily upon their @.Lazy@ counterparts (though there have been
discussions about renaming the type to avoid the naming conflict; it's
confusing enough having both strict and lazy @ByteString@s!).

In this task, re-implement 'takeNLines' using streaming 'ByteString's
(the @.Char8@ module will be helpful).  It will not be a direct port,
but you shouldn't be a huge change.

Please note that you should /never/ actually do something like this:
many consider treating the underlying @Word8@ byte values in
@ByteStrings@ (of any flavour) as 'Char' values is a huge mistake.

-}

takeNLinesBS :: Int -> IO ()
takeNLinesBS = error "takeNLinesBS"
