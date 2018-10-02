
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
ghc_flags   = prj_prj_dir </> "ghc.flags"
bas_exe     = out_dir </> "bas"

main :: IO ()
main = do
    cd <- getCurrentDirectory
    mb_prj_dir <- findM does_prj_file_exist . parents $ cd
    case mb_prj_dir of
        Just prj_dir -> setCurrentDirectory prj_dir >> compile >> run
        Nothing      -> report_error "A project location was not found."
                    "You need to be in a project directory to run this command."
  where
    parents                 = reverse . scanl1 combine . splitDirectories
    does_prj_file_exist dir = doesFileExist $ dir </> prj_file

compile :: IO ()
compile = do
    createDirectoryIfMissing True out_dir
    s_flags <- words <$> readFile stack_flags <|> pure []
    c_flags <- words <$> readFile ghc_flags <|> pure []
    command "stack" $
        ["ghc"] ++ s_flags ++ ["--"] ++
        ["-hide-all-packages" | not . null $ s_flags] ++
        [ "--make", prj_file
        , ("-i" ++) . takeDirectory $ prj_file
        , "-rtsopts", "-with-rtsopts=-I0"
        , "-outputdir", out_dir
        , "-o", bas_exe
        ] ++
        c_flags

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

