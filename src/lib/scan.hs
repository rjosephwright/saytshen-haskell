{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Scan
  (
    runScan
  , Mode(..)
  , AuditStep(..)
  , Benchmark(..)
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text, unpack)
import Data.Yaml (FromJSON(..))
import GHC.Generics
import GHC.IO.Handle
import System.Exit
import System.Process

data Mode = Any | All deriving (Eq, Generic, Show)

instance FromJSON Mode

data AuditStep = AuditStep
  { run :: Text
  , expect :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON AuditStep

data Benchmark = Benchmark
  { section :: Text
  , description :: Text
  , audit :: [AuditStep]
  , mode :: Maybe Mode
  , skip :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON Benchmark

runScan :: IO ()
runScan = putStrLn "hello"
