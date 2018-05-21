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

import qualified Streaming.With as W

-- CSV support.  The required instances have been defined for you.
import qualified Streaming.Cassava as C

-- You will probably need to use this in conjunction with
-- "Streaming.Cassava".
import Control.Monad.Trans.Except (ExceptT, runExceptT)

-- You don't need to worry about these imports.
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Csv               (DefaultOrdered(..), FromNamedRecord(..),
                               FromRecord, Options, ToNamedRecord(..), ToRecord,
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
