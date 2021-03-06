import           System.Exit (ExitCode(..), exitFailure, exitSuccess, exitWith)
import           Scan (runScan)
import           Options.Applicative

spec :: Parser FilePath
spec = strOption
         (short 's'
          <> long "spec"
          <> metavar "SPEC"
          <> help "Path to audit specification file")

opts :: ParserInfo FilePath
opts = info (spec <**> helper) (fullDesc
                                <> header "A tool for running security compliance scans")

exitOn :: Either String Bool -> IO ()
exitOn (Left e) = putStrLn e >> exitWith (ExitFailure 255)
exitOn (Right b) = if b
                     then exitSuccess
                     else exitFailure

main :: IO ()
main = execParser opts >>= runScan >>= exitOn
