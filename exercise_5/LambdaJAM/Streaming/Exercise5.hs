{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{- |
   Module      : LambdaJAM.Streaming.Exercise5
   Description : Exercise 5
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   Parse the file @languages.csv@ to find all functional languages.

   Extension: create separate CSV files for every paradigm (excluding
   'otherParadigms').  Note that some languages support more than one
   paradigm...

 -}
module LambdaJAM.Streaming.Exercise5 where

import Streaming.Prelude (Of(..), Stream)

import qualified Streaming         as S
import qualified Streaming.Prelude as S

import qualified Streaming.With.Lifted as W

-- CSV support.  The required instances have been defined for you.
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Streaming.Cassava          as C

import Control.Monad            (void)
import Control.Monad.Trans.Cont (ContT)
import Data.Csv.Incremental     (encode, encodeDefaultOrderedByNameWith,
                                 encodeNamedRecord, encodeRecord)
import System.IO                (Handle, IOMode(AppendMode))

-- You will probably need to use this in conjunction with
-- "Streaming.Cassava".
import Control.Monad.Trans.Except (ExceptT, runExceptT)

-- You don't need to worry about these imports.
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Csv               (DefaultOrdered(..),
                               EncodeOptions(encIncludeHeader),
                               FromNamedRecord(..), FromRecord, Options,
                               ToNamedRecord(..), ToRecord,
                               defaultDecodeOptions, defaultEncodeOptions,
                               defaultOptions, fieldLabelModifier,
                               genericHeaderOrder, genericParseNamedRecord,
                               genericToNamedRecord)
import GHC.Generics           (Generic)

--------------------------------------------------------------------------------

-- | A very simple representation of each data row in the CSV file.
--
--   You are able to use functions like 'C.decode' with it.
data LanguageDetails = Language
  { language       :: !String
  , intendedUse    :: !String
  , imperative     :: !String
  , objectOriented :: !String
  , functional     :: !String
  , procedural     :: !String
  , generic        :: !String
  , reflective     :: !String
  , eventDriven    :: !String
  , otherParadigms :: !String
  , standardized   :: !String
  } deriving (Eq, Show, Read, Generic, FromRecord, ToRecord)

-- | A very simple function to get the Booleanness of a field.
hasParadigm :: (LanguageDetails -> String) -> LanguageDetails -> Bool
hasParadigm f = ("Yes"==) . take 3 . f

instance FromNamedRecord LanguageDetails where
  parseNamedRecord = genericParseNamedRecord languageOptions

instance ToNamedRecord LanguageDetails where
  toNamedRecord = genericToNamedRecord languageOptions

instance DefaultOrdered LanguageDetails where
  headerOrder = genericHeaderOrder languageOptions

-- | Match record field names with column names.
languageOptions :: Options
languageOptions = defaultOptions { fieldLabelModifier = names }
  where
    names "language"       = "Language"
    names "intendedUse"    = "Intended use"
    names "imperative"     = "Imperative"
    names "objectOriented" = "Object-oriented"
    names "functional"     = "Functional"
    names "procedural"     = "Procedural"
    names "generic"        = "Generic"
    names "reflective"     = "Reflective"
    names "eventDriven"    = "Event-driven"
    names "otherParadigms" = "Other paradigm(s)"
    names "standardized"   = "Standardized?"
    names nm               = nm

--------------------------------------------------------------------------------

-- | You may wish to use this to handle the possible exception from
--   parsing.
printError :: (MonadIO m) => ExceptT C.CsvParseException m r -> m ()
printError = (either (liftIO . print) (const (return ())) =<<) . runExceptT

--------------------------------------------------------------------------------

{-

Solution notes:

Unfortunately, when we use 'S.copy' we can't actually write the outer
'Stream' out as 'Stream' is /not/ an instance of 'MonadMask', which is
required for how @streaming-with@ works (for safety).

As such, we end up having to manually handle the CSV writing process.

Furthermore, the 'ExceptT' instance for @MonadMask@ was only added in
@exceptions-0.10.0@, which isn't widely available at this time.  As
such, we have to be a bit more manual with our exception handling.

We use the @.Lifted@ variant of @streaming-with@ as it provides us a
monadic interface.

-}

exercise5 :: FilePath -> IO ()
exercise5 = W.runWith . processLanguageFile

type Paradigm = LanguageDetails -> String

paradigms :: [(String, Paradigm)]
paradigms = [ ("imperative", imperative)
            , ("objectOriented", objectOriented)
            , ("functional", functional)
            , ("procedural", procedural)
            , ("generic", generic)
            , ("reflective", reflective)
            ]

data ParadigmCount = ParadigmCount
  { lang          :: !String
  , paradigmCount :: !Int
  } deriving (Eq, Show, Read, Generic, ToNamedRecord, DefaultOrdered)

processLanguageFile :: FilePath -> ContT () IO ()
processLanguageFile fp = do
  cnts <- W.withBinaryFileContents fp
  writeResults
    -- In case there are any parsing errors, we print the errors out
    -- to stdout.
    . S.print
    . S.partitionEithers
    . C.decodeByNameWithErrors defaultDecodeOptions
    $ cnts

writeResults :: (W.Withable w) => Stream (Of LanguageDetails) (W.WithMonad w) r -> w ()
writeResults str = do
  cntH <- createCountFile
  hpds <- mapM createParadigmFile paradigms
  W.liftAction (void (S.mapM_ (processLanguage cntH hpds) str))

-- Assumes files don't already exist
createParadigmFile :: (W.Withable w) => (String, Paradigm) -> w (Handle, Paradigm)
createParadigmFile (nm,p) = do
  h <- W.withBinaryFile (nm ++ ".csv") AppendMode
  W.liftActionIO (LB8.hPutStr h hdr)
  return (h, p)
  where
    hdr = headerFor (undefined :: LanguageDetails)

createCountFile :: (W.Withable w) => w Handle
createCountFile = do
  h <- W.withBinaryFile "paradigmCount.csv" AppendMode
  W.liftActionIO (LB8.hPutStr h (headerFor (undefined :: ParadigmCount)))
  return h

headerFor :: (DefaultOrdered a) => a -> LB8.ByteString
headerFor = encode . encodeRecord . headerOrder

processLanguage :: (MonadIO m) => Handle -> [(Handle, Paradigm)] -> LanguageDetails -> m ()
processLanguage cntH hpds ld = liftIO $ do
  mapM_ (\h -> LB8.hPutStr h encLD) validPs
  LB8.hPutStr cntH (encodeSingle pCnt)
  where
    encLD = encodeSingle ld

    validPs = map fst . filter ((`hasParadigm`ld) . snd) $ hpds

    pCnt = ParadigmCount { lang = language ld, paradigmCount = length validPs }

encodeSingle :: (DefaultOrdered a, ToNamedRecord a) => a -> LB8.ByteString
encodeSingle = encodeDefaultOrderedByNameWith (defaultEncodeOptions {encIncludeHeader = False})
               . encodeNamedRecord
