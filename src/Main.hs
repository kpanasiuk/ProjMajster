
import Control.Applicative
import Control.Monad.Extra
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import System.IO

build_dir   = "_build"
out_dir     = build_dir </> "bas"
prj_prj_dir = "prj" </> "shake"
prj_file    = prj_prj_dir </> "Main.hs"
stack_flags = prj_prj_dir </> "stack.flags"
bas_exe     = out_dir </> "bas"

main :: IO ()
main = do
    cd <- getCurrentDirectory
    maybePrjDir <- findM doesPrjFileExist . parents $ cd
    case maybePrjDir of
        Just prjDir -> setCurrentDirectory prjDir >> compile >> run
        Nothing     -> report_error "A project location was not found."
                    "You need to be in a project directory to run this command."
  where
    parents              = reverse . scanl1 combine . splitDirectories
    doesPrjFileExist dir = doesFileExist $ dir </> prj_file

compile :: IO ()
compile = do
    createDirectoryIfMissing True out_dir
    stackFlags <- words <$> readFile stack_flags <|> pure []
    command "stack" $
        ["ghc"] ++ stackFlags ++ ["--"] ++
        ["-hide-all-packages" | not . null $ stackFlags] ++
        [ "--make", prj_file
        , ("-i" ++) . takeDirectory $ prj_file
        , "-rtsopts", "-with-rtsopts=-I0"
        , "-outputdir", out_dir
        , "-o", bas_exe
        ]

run :: IO ()
run = command bas_exe =<< getArgs

report_error :: String -> String -> IO a
report_error msg help = do
    prog <- getProgName
    hPutStr stderr $ unlines
        [ ""
        , prog ++ " failed: " ++ msg
        , ""
        , help
        ]
    exitFailure

command :: String -> [String] -> IO ()
command cmd args = checkExitCode =<< rawSystem cmd args
  where
    checkExitCode ExitSuccess = return ()
    checkExitCode exitCode    = exitWith exitCode

