{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Scan
  ( runBenchmark
  , runScan
  , Mode(..)
  , AuditStep(..)
  , Benchmark(..)
  , BenchmarkResult(..)
  ) where

import Data.Text (Text, pack, unpack)
import Data.Yaml (FromJSON(..))
import GHC.Generics
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

data BenchmarkResult = BenchmarkResult
  { sectionR :: Text
  , descriptionR :: Text
  , runR :: Text
  , passedR :: Bool
  , outputR :: Text
  , errorR :: Text
  , skipR :: Maybe Text
  } deriving (Generic, Show)

runScript :: Text -> IO (ExitCode, String, String)
runScript script = readProcessWithExitCode "/bin/sh" ["-c", unpack script] ""

runBenchmark :: Benchmark -> IO [BenchmarkResult]
runBenchmark benchmark = do
  outputs <- mapM (\s -> runScript (run s)) steps
  zipped <- return $ zip steps outputs
  return $ map (\(step, (ret, out, err)) -> BenchmarkResult
                 { sectionR = section benchmark
                 , descriptionR = description benchmark
                 , runR = run step
                 , passedR = ret == ExitSuccess
                 , outputR = pack out
                 , errorR = pack err
                 , skipR = skip benchmark
                 }) zipped
    where steps = audit benchmark

runScan :: IO ()
runScan = putStrLn "hello"
