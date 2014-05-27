module Main where
import Parser (parseProgram)
import Compiler
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO --(openFile, ReadMode, hGetContents, writeFile)
import Data.Maybe ( fromJust, fromMaybe )
import Data.Functor

main = do args <- getArgs
	  (options, other) <- compilerOpts args
	  --case optInput options of
	  --    Nothing 
	  print options
	  text <- readFile  (fromJust $ optInput options)
	  case parseProgram (fromJust $ optInput options) text of
		Left error -> print error
		Right ast  -> writeFile (fromJust $ optOutput options) (show' (compile ast))


--https://www.haskell.org/ghc/docs/7.8.2/html/libraries/base-4.7.0.0/System-Console-GetOpt.html

data Options = Options
    { optOutput      :: Maybe String --FilePath?
    , optInput       :: Maybe String
    } deriving Show

defaultOptions    = Options
    { optOutput      = Nothing
    , optInput       = Nothing
    }

optionsHandler :: [OptDescr (Options -> Options)]
optionsHandler = [(Option ['o'] ["output"] (OptArg ((\ f opts -> opts { optOutput = f })) "FILE") "output FILE"), 
		  (Option ['i'] ["input"] (OptArg ((\ f opts -> opts { optInput = f })) "FILE") "input FILE")]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute optionsHandler argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header optionsHandler))
  where header = "Usage: milan [OPTION...]"


  
  
  
  