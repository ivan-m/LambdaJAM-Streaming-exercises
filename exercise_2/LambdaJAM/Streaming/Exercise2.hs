{-# OPTIONS_GHC -Wno-unused-imports #-}

{- |
   Module      : LambdaJAM.Streaming.Exercise1
   Description : Exercise 2
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   This exercise focuses on how Stream differs from Pipe and Conduit.

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

--------------------------------------------------------------------------------
