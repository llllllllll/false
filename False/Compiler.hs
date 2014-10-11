-- |
-- Module      : False.Compiler
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Nodes to Targets
module False.Compiler
    ( compileTarget  -- :: String
                     -- -> Either CompileErrorCompileError (String,Target)
    ) where



import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Control.Monad.ST (ST,runST)
import Data.List (intercalate)
import Data.STRef (STRef,newSTRef,readSTRef,modifySTRef')

import False.Core (CompileError(..),initPosition,
                   Target(..),PNode(..),Node(..),Lambda(..))
import False.Lexer (lexFalse)
import False.Parser (parseFalse)


-- | Preprocesses the lambdas and returns the body of the program.
-- the [(Int,[PNode])] is a list of all lambdas that occur in the program.
-- The [(Int,String)] is a list of all the strings used in the program.
-- Beware, this function is a tad arcane.
ppLambdas :: [Node] -> ([(Int,String)],[(Int,[PNode])],[PNode])
ppLambdas ns = runST (newSTRef []
                      >>= \ls -> newSTRef []
                      >>= \ss -> newSTRef 0
                      >>= \lc -> newSTRef 0
                      >>= \sc -> ppLambdas' ns ss sc ls lc
                      >>= \ms -> readSTRef ls
                      >>= \ns -> readSTRef ss
                      >>= \ts -> return (ts,ns,ms))


ppLambdas' :: [Node] -> STRef s [(Int,String)] -> STRef s Int
           -> STRef s [(Int,[PNode])] -> STRef s Int -> ST s [PNode]
ppLambdas' [] _ _ _ _  = return []
ppLambdas' (LambdaNode (Lambda l):ns) ss sc ls lc
    = readSTRef lc
      >>= \c -> modifySTRef' lc (+ 1)
      >>  ppLambdas' l ss sc ls lc
      >>= \lb -> modifySTRef' ls ((:) (c,lb))
      >>  (:) (PLambda c) <$> ppLambdas' ns ss sc ls lc
ppLambdas' (StringNode s:ns) ss sc ls lc
    = readSTRef sc
      >>= \c -> modifySTRef' sc (+ 1)
      >>  modifySTRef' ss ((:) (c,s))
      >>  (:) (PString c s) <$> ppLambdas' ns ss sc ls lc
ppLambdas' (n:ns) ss sc ls lc = (:) (PNode n) <$> ppLambdas' ns ss sc ls lc


compileTarget :: String -> Target -> Either CompileError (String,Target)
compileTarget cs t = case lexFalse initPosition cs of
                         Left (e,p)   -> Left $ CompileLexError e p
                         Right ts -> case parseFalse ts of
                                         Left e   -> Left $ CompileParseError e
                                         Right ns -> Right (uncurry3 (compile t)
                                                           $ ppLambdas ns,t)
  where
      uncurry3 f (a,b,c) = f a b c
