{-# LANGUAGE LambdaCase #-}
module Main where


import Control.Applicative ((<$>))
import Control.Monad.ST (ST,runST,fixST)
import Data.List (intercalate)
import Data.STRef (STRef,newSTRef,readSTRef,modifySTRef')
import System.Console.GetOpt (ArgOrder(..),OptDescr(..)
                             ,ArgDescr(..),getOpt,usageInfo)
import System.Environment (getArgs)

import False.C (falseStr)
import False.Data (Node(..),Func(..),Lambda(..),CompileError(..),initPosition)
import False.Lexer (lexFalse)
import False.Parser (parseFalse)


version :: String
version = "0.1.0.0"


type C = String


data PNode = PLambda Int
           | PNode Node


instance Show PNode where
    show (PLambda n)          = stackPush $ "(size_t) &lambda_" ++ show n
    show (PNode (FuncNode f)) = callFunction $ funcToC f
    show (PNode (ValNode n))  = stackPush $ show n
    show (PNode (VarNode v))  = stackPush $ "(size_t) " ++ '&' : show v


stack :: C
stack = "stack"


stackPush :: String -> C
stackPush n = callFunctionWithArgs "stackpush" [n]


callFunction :: C -> C
callFunction f = f ++ "(" ++ stack ++ ");"


callFunctionWithArgs :: C -> [C] -> C
callFunctionWithArgs f as = f ++ "(" ++ intercalate "," (stack : as) ++ ");"


funcToC :: Func -> C
funcToC FAdd    = "f_add"
funcToC FSub    = "f_sub"
funcToC FMul    = "f_mul"
funcToC FDiv    = "f_div"
funcToC FNeg    = "f_neg"
funcToC FEq     = "f_eq"
funcToC FGt     = "f_gt"
funcToC FNot    = "f_not"
funcToC FAnd    = "f_and"
funcToC FOr     = "f_or"
funcToC FAssign = "f_assign"
funcToC FRead   = "f_read"
funcToC FApply  = "f_apply"
funcToC FDup    = "f_dup"
funcToC FDel    = "f_del"
funcToC FSwap   = "f_swap"
funcToC FRot    = "f_rot"
funcToC FPick   = "f_pick"
funcToC FIf     = "f_if"
funcToC FWhile  = "f_while"
funcToC FPrintI = "f_printi"
funcToC FPrintC = "f_printc"
funcToC FInput  = "f_input"
funcToC FFlush  = "f_flush"


-- | Preprocesses the lambdas and returns the body of the program.
-- the [(Int,[PNode])] is a list of all lambdas that occur in the program.
ppLambdas :: [Node] -> ([(Int,[PNode])],[PNode])
ppLambdas ns = runST (newSTRef []
                      >>= \ls -> newSTRef 0
                      >>= \lc -> ppLambdas' ns ls lc
                      >>= \ms -> readSTRef ls
                      >>= \ns -> return (ns,ms))


ppLambdas' :: [Node] -> STRef s [(Int,[PNode])] -> STRef s Int
           -> ST s [PNode]
ppLambdas' []                         ls _  = readSTRef ls
                                              >>= \ls' -> return []
ppLambdas' (LambdaNode (Lambda l):ns) ls lc
    = readSTRef lc
      >>= \c -> modifySTRef' lc (+ 1)
      >>  ppLambdas' l ls lc
      >>= \lb -> modifySTRef' ls ((:) (c,lb))
      >>  ((:) (PLambda c)) <$> ppLambdas' ns ls lc
ppLambdas' (n:ns) ls lc = ((:) (PNode n)) <$> ppLambdas' ns ls lc


-- | Defines lambda_n in the source code.
defLambda :: (Int,[PNode]) -> C
defLambda (n,ps) = "void lambda_" ++ show n ++ "(f_stack *stack){\n    "
                   ++ writePNodes ps ++ "\n}"


defMain :: [PNode] -> C
defMain ps = "int main(int argc,char **argv){\n\
             \    f_namespace ns;\n\
             \    f_stack stack = malloc(sizeof(f_stack));\n\
             \    f_init(ns,stack,argv);\n    " ++ writePNodes ps ++ "\n}"


writePNodes :: [PNode] -> C
writePNodes ps = intercalate "\n    " $ map show ps


compileNodes :: [Node] -> C
compileNodes ns = let (ls,ms) = ppLambdas ns
                  in falseStr ++ "\n\n\n"
                         ++ intercalate "\n\n\n" (map defLambda $ reverse ls)
                         ++ "\n\n\n" ++  defMain ms


compile :: String -> Either CompileError C
compile cs = case lexFalse initPosition cs of
                 Left (e,p)   -> Left $ CompileLexError e p
                 Right ts -> case parseFalse ts of
                                 Left e   -> Left $ CompileParseError e
                                 Right ns -> Right $ compileNodes ns


data Flag = Help
          | Version
          | OutputFile FilePath


options :: [OptDescr Flag]
options = [ Option "h" ["help"] (NoArg Help) "Displays the help message"
          , Option "v" ["version"] (NoArg Version) "Displays the version info"
          , Option "o" [] (ReqArg OutputFile "outfile") "Write to outfile"
          ]


getOut :: [Flag] -> FilePath
getOut fs = foldr (\a b -> case a of
                               OutputFile f -> f
                               _            -> b) "a.out" fs


hasHelp :: [Flag] -> Bool
hasHelp = any (\case
                   Help -> True
                   _    -> False)


hasVersion :: [Flag] -> Bool
hasVersion = any (\case
                      Version -> True
                      _       -> False)


handleOpts :: ([Flag],[String],[String]) -> IO ()
handleOpts (fs,f:_,_) = readFile f
                        >>= \cs -> case compile cs of
                                       Left e  -> error $ show e
                                       Right c -> appendFile (getOut fs) c
handleOpts (fs,_,_)
    | hasHelp fs    = printHelp
    | hasVersion fs = printVersion
    | otherwise     = putStrLn "Usage: f2c infile"
  where
      printHelp    = putStrLn $ "Usage:\n" ++ usageInfo "" options
      printVersion = putStrLn version
