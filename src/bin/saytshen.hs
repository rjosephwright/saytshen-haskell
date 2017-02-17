import           System.Exit
import           Scan (runScan)
import           Options.Applicative

spec :: Parser FilePath
spec = strOption
  ( long "spec"
    <> metavar "SPEC"
    <> help "Path to audit specification file"
  )

opts :: ParserInfo FilePath
opts = info (spec <**> helper)
  ( fullDesc
  <> header "A tool for running security compliance scans"
  )

main :: IO ()
main = do
  scanResult <- runScan =<< execParser opts
  case scanResult of
    (Right b) -> if b then exitSuccess else exitFailure
    (Left e) -> putStrLn e >> exitWith (ExitFailure 255)
  return ()
