{-# OPTIONS_GHC -Wno-unused-imports #-}

{- |
   Module      : LambdaJAM.Streaming.Exercise3
   Description : Exercise 3
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   Streams as powerful lists.

   Note: we are now using the actual library; whilst you can obtain
   the constructors for the 'Stream' type from 'Streaming.Internal' I
   recommend you don't.

   Instead, consider using 'S.next' as a deconstructor, and 'S.effect'
   and 'S.wrap' (and 'S.cons') as wrappers around the @Effect@ and
   @Step@ constructors.

 -}
module LambdaJAM.Streaming.Exercise3 where

import Streaming.Prelude (Of(..), Stream)

import qualified Streaming         as S
import qualified Streaming.Prelude as S

-- Other imports
import Data.Char     (isSpace, toUpper)
import Data.Function (on)

--------------------------------------------------------------------------------

{-

Task 1:

Write some textual functions.  Don't forget to use 'S.toList' to get
back "Strings" if you want to print them nicely.

'isSpace' and 'toUpper' should be useful.

-}

-- | Make every letter upper-case.
capitalise :: (Monad m) => Stream (Of Char) m r -> Stream (Of Char) m r
capitalise = error "capitalise"

-- | Split the Stream into Streams of words which were delimited by
--   whitespace (i.e. no whitespace should be present in any
--   sub-Stream).
streamWords :: (Monad m) => Stream (Of Char) m r -> Stream (Stream (Of Char) m) m r
streamWords = error "streamWords"

-- | Inverse operation to 'streamWords'.  Joins words with separating spaces.
streamUnwords :: (Monad m) => Stream (Stream (Of Char) m) m r -> Stream (Of Char) m r
streamUnwords = error "streamUnwords"

{-

Hint: you primarily need to use 'S.map', 'S.intercalates', 'S.next',
'S.wrap', 'S.effect', 'S.break' and 'S.dropWhile'.

The definition of 'streamWwords' is a bit messy.  Consider the list definition:

1. How do you know if you've reached the end?

2. If you know there's at least one 'Char', is it a space or
   non-space?

-}

--------------------------------------------------------------------------------

{-

Task 2:

Create a function that capitalises the first character of every word.

* Doesn't matter if you keep the original whitespace or use
  'streamWords' and 'streamUnwords'.

* Don't worry about the case of any other letter in a word or if a
  "word" doesn't actually start with (or even contain) a letter: just
  use 'toUpper'.

-}

titleCase :: (Monad m) => Stream (Of Char) m r -> Stream (Of Char) m r
titleCase = error "titleCase"

--------------------------------------------------------------------------------

{-

Task 3:

One of the most infamous Haskell list-based pieces of code is the
efficient (as long as you don't try and keep them in memory) way of
creating an infinite list of Fibonacci numbers:

-}

fibonacciList :: IO ()
fibonacciList = mapM_ print
                . take 10
                $ fibs
  where
    fibs :: [Int]
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

{-

Port this to using a Stream instead of a list.

-}

fibonacciStream :: IO ()
fibonacciStream = error "fibonacciStream"
