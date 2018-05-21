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

(Hint: 'S.stdinLn'.)

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

(Unfortunately, at this time there is no Stream equivalent of Text.)

-}

takeNLinesBS :: Int -> IO ()
takeNLinesBS = error "takeNLinesBS"

--------------------------------------------------------------------------------

{-

Motivating discussion:

One of the biggest issues mentioned with lazy I/O is how to ensure
that file handles are closed after they are no longer used, etc.  The
same still applies with stream processing libraries, and a few
solutions have come up for this.

Probably the best known is the
[ResourceT](http://hackage.haskell.org/package/resourcet) library,
primarily used with Conduit.  This provides a monad transformer that
you can use to register resources that need to be released when no
longer needed.  Pipes has the
[pipes-safe](http://hackage.haskell.org/package/pipes-safe) library
that aims to provide similar functionality albeit more specialised.

The streaming library used to also use @ResourceT@ but this was
removed with the 0.2.0.0 release.  Why?  Turns out that there were
fundamental issues with how ResourceT interacted with Streams:

* streaming-bytestring kept trying to read from closed handles:
  https://github.com/michaelt/streaming-bytestring/issues/17 (Note:
  this repository is no longer used, hence why the issue has not been
  directly addressed)

* Due to lack of prompt finalization, usage wouldn't actually always
  work:

    - https://github.com/haskell-streaming/streaming/issues/36
    - https://github.com/haskell-streaming/streaming/issues/13
    - https://github.com/michaelt/streaming/issues/23

As such, the current recommended solution for resource management with
Streams is the @streaming-with@ library.  See its README for more
justification/discussions about how it works and why.

(Please note that this has not fully been adopted yet:
@streaming-postgresql-simple@ for example has not yet been ported,
though there is a pull request to do so.)

It's interesting to note though that as of version 1.3.0, Conduit has
removed much (all?) of its support for finalizers:
https://www.reddit.com/r/haskell/comments/7nuyk8/drop_conduits_finalizers/
(Reddit link used to see the discussion as well).

-}

--------------------------------------------------------------------------------

{-

Task 3:

The @streaming-with@ library uses the /bracket pattern/ to handle
resource management.

Use it to implement the following function, then test it out (for
example, if you are using @cabal repl@ or @stack ghci@ then you should
have at the very least @LICENSE@ and @README.md@ files in the same
directory as the REPL; check this by running @:! ls@).

-}

-- | Given @mergeTwoFilesTo in1 in2 out@, then the contents of @out@
--   afterwards will be composed of the contents of @in1@ followed by
--   the contents of @in2@.
mergeTwoFilesTo :: FilePath -> FilePath -> FilePath -> IO ()
mergeTwoFilesTo = error "mergeTwoFilesTo"

-- | Run @:! ls@ in your ghci session to ensure the required files are
--   available.
readmeAndLicense :: IO ()
readmeAndLicense = mergeTwoFilesTo "README.md" "LICENSE" "exercise4.md"
