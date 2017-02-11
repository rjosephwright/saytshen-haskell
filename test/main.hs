{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Yaml as Y
import Text.RawString.QQ

import Scan

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "YAML parsing tests" ymlTests ]

ymlTests :: [TestTree]
ymlTests =
  [ testCase "Parse the YAML" parseOneBenchmark
  , testCase "Parse more YAML" parseTwoBenchmarks
  , testCase "Run a benchmark" runOneBenchmark
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
      Right [benchmark] -> do
        (section benchmark)        @?= ("1.1.1.1" :: Text)
        (length (audit benchmark)) @?= (2 :: Int)
        (mode benchmark)           @?= (Nothing :: Maybe Mode)
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

runOneBenchmark :: Assertion
runOneBenchmark =
  let benchmark = Benchmark
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
    result <- runBenchmark benchmark
    (map (\res -> (passedR res)) result) @?= ([False] :: [Bool])
