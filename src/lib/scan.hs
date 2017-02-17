{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Scan
  ( runBenchmark
  , runBenchmarks
  , runScan
  , Mode(..)
  , AuditStep(..)
  , Benchmark(..)
  , BenchmarkResult(..)
  ) where

import Prelude hiding (writeFile)
import Data.ByteString.Lazy.Char8 (writeFile)
import Data.ByteString.Lazy (ByteString)
import Data.Csv (DefaultOrdered, ToField(..), ToNamedRecord, encodeDefaultOrderedByName)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, pack, unpack)
import Data.Yaml (FromJSON, ParseException, decodeFileEither, prettyPrintParseException)
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

instance ToField Bool where
  toField True = "true"
  toField False = "false"

instance ToNamedRecord BenchmarkResult

instance DefaultOrdered BenchmarkResult

type CommandResult = (ExitCode, String, String)

runScript :: Text -> IO CommandResult
runScript script = readProcessWithExitCode "/bin/sh" ["-c", unpack script] ""

isSuccess :: Benchmark -> [CommandResult] -> Bool
isSuccess benchmark outputs =
  let shouldSkip = isJust $ skip benchmark
      exitCodes = map (\(ec, _, _) -> ec) outputs
      check = case (fromMaybe All (mode benchmark)) of
        Any -> any
        All -> all
  in
    shouldSkip || check (== ExitSuccess) exitCodes

runBenchmark :: Benchmark -> IO [BenchmarkResult]
runBenchmark benchmark = do
  outputs <- mapM (\s -> runScript (run s)) steps
  passed <- return $ isSuccess benchmark outputs
  zipped <- return $ zip steps outputs
  return $ map (\(step, (_, out, err)) -> BenchmarkResult
                 { sectionR = section benchmark
                 , descriptionR = description benchmark
                 , runR = run step
                 , passedR = passed
                 , outputR = pack out
                 , errorR = pack err
                 , skipR = skip benchmark
                 }) zipped
    where steps = audit benchmark

runBenchmarks :: [Benchmark] -> IO [BenchmarkResult]
runBenchmarks benchmarks = do
  benchmarkResults <- mapM runBenchmark benchmarks
  return $ concat benchmarkResults

createReport :: [BenchmarkResult] -> ByteString
createReport benchmarkResults = encodeDefaultOrderedByName benchmarkResults

runScan :: FilePath -> IO (Either String Bool)
runScan spec = do
  benchmarks <- decodeFileEither spec :: IO (Either ParseException [Benchmark])
  case benchmarks of
    Right bs -> do
      benchmarkResults <- runBenchmarks bs
      report <- return $ createReport benchmarkResults
      writeFile "report.csv" report
      return $ Right (all (\br -> passedR br) benchmarkResults)
    Left err -> return $ Left (prettyPrintParseException err)
