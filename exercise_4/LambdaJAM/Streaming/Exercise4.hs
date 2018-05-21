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

import           Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming as SB

--------------------------------------------------------------------------------
