-- |
-- Module      : False.Targets.C
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- PNodes to C

{-
Program structure:

C89 is the target.

The stack is stored in a structure called f_stack.
All the operators are function that mutate a single f_stack.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module False.Targets.C
    ( cTarget  -- :: Target
    ) where


import Control.Applicative ((<$>))
import Control.Parallel.Strategies (parMap,rseq)
import Data.Char (ord)
import Data.List (intercalate)

import False.Core (PNode(..),Target(..),Func(..),Node(..),Var(..))
import False.Targets.C.QQ (include)


type C = String


cTarget :: Target
cTarget = Target { compile     = cCompile
                 , defaultFile = "a.c"
                 }


cCompile :: [(Int,String)] -> [(Int,[PNode])] -> [PNode] -> String
cCompile _ ls ms = let ls' = parMap rseq defLambda $ reverse ls
                       ms' = defMain ms
                   in intercalate "\n\n\n"
                          $ [include|False/Targets/C/false.c|] : ls' ++ [ms']


cToTarget :: PNode -> C
cToTarget (PNode (FuncNode f))      = callFunction $ funcToC f
cToTarget (PNode (ValNode n))       = stackPush $ show n
cToTarget (PNode (VarNode (Var v))) = stackPush $ show $ ord v - ord 'a'
cToTarget (PString _ s)             = "puts(" ++ show s ++ ");"
cToTarget (PLambda n)               = stackPush $ "(size_t) &lambda_" ++ show n


stackPush :: String -> C
stackPush n = callFunctionWithArgs "f_stackpush" [n]


callFunction :: C -> C
callFunction f = f ++ "(stack);"


callFunctionWithArgs :: C -> [C] -> C
callFunctionWithArgs f as = f ++ "(" ++ intercalate "," ("stack" : as) ++ ");"


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


-- | Defines lambda_n in the source code.
defLambda :: (Int,[PNode]) -> C
defLambda (n,ps) = "void lambda_" ++ show n ++ "(f_stack *stack){\n    "
                   ++ writePNodes ps ++ "\n}"


defMain :: [PNode] -> C
defMain ps = "int main(int argc,char **argv){\n\
             \    f_stack *stack = malloc(sizeof(f_stack));\n\
             \    f_init(namespace,stack,argv);\n    "
                      ++ writePNodes ps
                      ++ "\n    free(f_stack);    return 0;\n}"


writePNodes :: [PNode] -> C
writePNodes ps = intercalate "\n    " $ parMap rseq cToTarget ps
