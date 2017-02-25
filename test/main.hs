{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Yaml as Y
import Text.RawString.QQ

import Scan
import System.Exit

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "YAML parsing tests" ymlTests ]

ymlTests :: [TestTree]
ymlTests =
  [ testCase
      "Parse the YAML"
      parseOneBenchmark
  , testCase
      "Parse more YAML"
      parseTwoBenchmarks
  , testCase
      "Benchmark succeeds by exit status"
      runBenchmarkSucceeded1
  , testCase
      "Benchmark fails by exit status"
      runBenchmarkSucceeded2
  , testCase
      "Multiple benchmarks succeed by exit status"
      runBenchmarkSucceeded3
  , testCase
      "Multiple benchmarks succeed by exit status but one has unexpected output"
      runBenchmarkSucceeded4
  , testCase
      "Multiple benchmarks succeed by exit status and one has expected output"
      runBenchmarkSucceeded5
  , testCase
      "Multiple benchmarks succeed by exit status and one has unexpected output but mode is Any"
      runBenchmarkSucceeded6
  , testCase
      "One of multiple benchmarks fails by exit status but mode is Any"
      runBenchmarkSucceeded7
  , testCase
      "One of multiple benchmarks fails by exit status and other has unexpectd output"
      runBenchmarkSucceeded8
  , testCase
      "Run a benchmark"
      runOneBenchmark
  , testCase
      "Run a benchmark that should be skipped"
      runOneBenchmarkWithSkip
  , testCase
      "Run two benchmarks"
      runTwoBenchmarks
  ]

parseOneBenchmark :: Assertion
parseOneBenchmark =
  let yaml = [r|
- section: 1.1.1.1
  description: Ensure mounting of cramfs filesystems is disabled
  audit:
    - run: modprobe -n -v cramfs | grep 'install /bin/true'
    - run: |
        lsmod | grep cramfs
        [ ${?} -eq 1 ]
|]
      parsed = Y.decodeEither yaml :: Either String [Benchmark] in
    case parsed of
      Right [bm] -> do
        (section bm)        @?= ("1.1.1.1" :: Text)
        (length (audit bm)) @?= (2 :: Int)
        (mode bm)           @?= (Nothing :: Maybe Mode)
      Right v -> assertFailure (show v)
      Left e -> assertFailure (show e)

parseTwoBenchmarks :: Assertion
parseTwoBenchmarks =
  let yaml = [r|
- section: 2.2.1.1
  description: Ensure time synchronization is in use
  audit:
    - run: rpm -q ntp
    - run: rpm -q chrony
  mode: Any
- section: 6.2.19
  description: Ensure no duplicate group names exist
  audit:
    - run: |
        cat /etc/group | cut -f1 -d":" | sort -n | uniq -c | while read x ; do
            [ -z "${x}" ] && break
            set - $x
            if [ $1 -gt 1 ]; then
                gids=`gawk -F: '($1 == n) { print $3 }' n=$2 /etc/group | xargs`
                echo "Duplicate Group Name ($2): ${gids}"
            fi
        done
      expect: '^$'
|]
      parsed = Y.decodeEither yaml :: Either String [Benchmark] in
    case parsed of
      Right [b1, b2] -> do
        (section b1)               @?= ("2.2.1.1" :: Text)
        (length (audit b1))        @?= (2 :: Int)
        (expect ((audit b1) !! 0)) @?= (Nothing :: Maybe Text)
        (expect ((audit b1) !! 1)) @?= (Nothing :: Maybe Text)
        (mode b1)                  @?= (Just Any :: Maybe Mode)
        (section b2)               @?= ("6.2.19" :: Text)
        (length (audit b2))        @?= (1 :: Int)
        (expect ((audit b2) !! 0)) @?= (Just "^$" :: Maybe Text)
        (mode b2)                  @?= (Nothing :: Maybe Mode)
      Right v -> assertFailure (show v)
      Left e -> assertFailure (show e)

auditStep :: AuditStep
auditStep = AuditStep { run = "/bin/noexist"
                      , expect = Nothing
                      }

benchmark :: Benchmark
benchmark = Benchmark { section = "1.1.100"
                      , description = "Ensure noexist runs correctly"
                      , audit = [auditStep]
                      , mode = Nothing, skip = Nothing
                      }

runBenchmarkSucceeded1 :: Assertion
runBenchmarkSucceeded1 =
    benchmarkSucceeded benchmark [(auditStep, (ExitSuccess, "", ""))] @?= True

runBenchmarkSucceeded2 :: Assertion
runBenchmarkSucceeded2 =
    benchmarkSucceeded benchmark [(auditStep, (ExitFailure 2, "", ""))] @?= False

runBenchmarkSucceeded3 :: Assertion
runBenchmarkSucceeded3 =
  let auditStep2 = auditStep { expect = Just "[Ss]ome.output" }
      benchmark2 = benchmark { audit = [auditStep, auditStep2] }
  in
    benchmarkSucceeded benchmark2 [(auditStep, (ExitFailure 2, "", "")),
                                   (auditStep2, (ExitFailure 2, "some output", ""))] @?= False

runBenchmarkSucceeded4 :: Assertion
runBenchmarkSucceeded4 =
  let auditStep2 = auditStep { expect = Just "[Ss]ome.output" }
      benchmark2 = benchmark { audit = [auditStep, auditStep2] }
  in
    benchmarkSucceeded benchmark2 [(auditStep, (ExitSuccess, "", "")),
                                   (auditStep2, (ExitSuccess, "wrong output", ""))] @?= False

runBenchmarkSucceeded5 :: Assertion
runBenchmarkSucceeded5 =
  let auditStep2 = auditStep { expect = Just "[Ss]ome.output" }
      benchmark2 = benchmark { audit = [auditStep, auditStep2] }
  in
    benchmarkSucceeded benchmark2 [(auditStep, (ExitSuccess, "", "")),
                                   (auditStep2, (ExitSuccess, "some output", ""))] @?= True

runBenchmarkSucceeded6 :: Assertion
runBenchmarkSucceeded6 =
  let auditStep2 = auditStep { expect = Just "[Ss]ome.output" }
      benchmark2 = benchmark { audit = [auditStep, auditStep2]
                             , mode = Just Any
                             }
  in
    benchmarkSucceeded benchmark2 [(auditStep, (ExitSuccess, "", "")),
                                   (auditStep2, (ExitSuccess, "wrong output", ""))] @?= True

runBenchmarkSucceeded7 :: Assertion
runBenchmarkSucceeded7 =
  let auditStep2 = auditStep { expect = Just "^$" }
      benchmark2 = benchmark { audit = [auditStep, auditStep2]
                             , mode = Just Any
                             }
  in
    benchmarkSucceeded benchmark2 [(auditStep, (ExitFailure 1, "", "")),
                                   (auditStep2, (ExitSuccess, "", ""))] @?= True

runBenchmarkSucceeded8 :: Assertion
runBenchmarkSucceeded8 =
  let auditStep2 = auditStep { expect = Just "[Ss]ome.output" }
      benchmark2 = benchmark { audit = [auditStep, auditStep2]
                             , mode = Just Any
                             }
  in
    benchmarkSucceeded benchmark2 [(auditStep, (ExitFailure 1, "", "")),
                                   (auditStep2, (ExitSuccess, "wrong output", ""))] @?= False

runOneBenchmark :: Assertion
runOneBenchmark =
  let bm = Benchmark
        { section = "1.1.6"
        , description = "Ensure separate partition exists for /var"
        , audit = [
            AuditStep
            -- TODO: fix this janky testing strategy for running commands.
            { run = "mountzz | grep -E '.* on /var type'"
            , expect = Nothing
            }
          ]
        , mode = Nothing, skip = Nothing
        } in do
    result <- runBenchmark bm
    (map (\res -> (passedR res)) result) @?= [Passed False]

runOneBenchmarkWithSkip :: Assertion
runOneBenchmarkWithSkip =
  let bm = Benchmark
        { section = "1.1.6"
        , description = "Ensure separate partition exists for /var"
        , audit = [
            AuditStep
            { run = "mountzz | grep -E '.* on /var type'"
            , expect = Nothing
            }
          ]
        , mode = Nothing, skip = Just "reasons"
        } in do
    result <- runBenchmark bm
    (map (\res -> (passedR res)) result) @?= [Passed True]

runTwoBenchmarks :: Assertion
runTwoBenchmarks =
  let benchmarks = [
        Benchmark
        { section = "1.1.6"
        , description = "Ensure separate partition exists for /var"
        , audit = [
            AuditStep
            { run = "mountzz | grep -E '.* on /var type'"
            , expect = Nothing
            }
          ]
        , mode = Nothing, skip = Nothing
        },
        Benchmark
        { section = "1.1.7"
        , description = "Ensure separate partition exists for /var/tmp"
        , audit = [
            AuditStep
            { run = "mountzz | grep -E '.* on /var/tmp type'"
            , expect = Nothing
            }
          ]
        , mode = Nothing, skip = Nothing
        }] in do
    result <- runBenchmarks benchmarks
    (map (\res -> (passedR res)) result) @?= [Passed False, Passed False]
