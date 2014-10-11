{-# LANGUAGE LambdaCase #-}
module Main where

import System.Console.GetOpt (ArgOrder(..),OptDescr(..)
                             ,ArgDescr(..),getOpt,usageInfo)
import System.Environment (getArgs,getProgName)


import False.Compiler (compileTarget)
import False.Core (Target(..),CompileError(..))
import False.Targets.C (cTarget)
import False.Targets.ASM (asmTarget)


version :: String
version = "0.1.0.0"


data Flag = Help
          | Version
          | OutputFile FilePath
          | TargetOpt String
            deriving (Show)


options :: [OptDescr Flag]
options = [ Option "h" ["help"] (NoArg Help) "Displays the help message"
          , Option "v" ["version"] (NoArg Version) "Displays the version info"
          , Option "o" [] (ReqArg OutputFile "outfile") "Write to outfile"
          , Option "x" ["target"] (ReqArg TargetOpt "target") "Target language"
          ]


getOut :: FilePath -> [Flag] -> FilePath
getOut = foldr (\a b -> case a of
                            OutputFile f -> f
                            _            -> b)

getTarget :: [Flag] -> Either CompileError Target
getTarget = foldr (\a b -> case a of
                               TargetOpt t -> parseTarget t
                               _           -> b) (Right asmTarget)
  where
      parseTarget "c "   = Right cTarget
      parseTarget "nasm" = Right asmTarget
      parseTarget t      = Left $ InvalidTarget t


hasHelp :: [Flag] -> Bool
hasHelp = any (\case
                   Help -> True
                   _    -> False)


hasVersion :: [Flag] -> Bool
hasVersion = any (\case
                      Version -> True
                      _       -> False)


handleOpts :: String -> ([Flag],[String],[String]) -> IO ()
handleOpts _ (fs,f:_,_)
    = readFile f
      >>= \cs -> case getTarget fs >>= compileTarget cs of
                     Left e  -> error $ show e
                     Right (c,t) -> case getOut (defaultFile t) fs of
                                        "-" -> putStrLn c
                                        fl  -> writeFile fl c
handleOpts argv0 (fs,_,_)
    | hasHelp fs    = printHelp
    | hasVersion fs = printVersion
    | otherwise     = putStrLn $ "Usage: " ++ argv0 ++ " infile"
  where
      printHelp    = putStrLn $ "Usage:\n" ++ usageInfo "" options
      printVersion = putStrLn version





main :: IO ()
main = getProgName
       >>= \p -> getArgs
       >>= \as -> handleOpts p (getOpt RequireOrder options as)
