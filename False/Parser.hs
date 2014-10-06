-- |
-- Module      : False.Parser
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- False parsing.
module False.Parser where


import Control.Arrow (first)
import Control.Applicative ((<$>))
import Data.Char (ord)


import False.Data (Token(..)
                  ,ParseError(..)
                  ,Position(..)
                  ,Node(..)
                  ,Lambda(..)
                  ,Func(..)
                  ,Var(..)
                  )


-- | Parse the Token stream into a list of [Node]s.
parseFalse :: [Token] -> Either ParseError [Node]
parseFalse []             = Right []
parseFalse (TLambdaStart p:ts)
    = case parseLambda p ts 0 of
          (Right ns,rs) -> ((:) (LambdaNode $ Lambda ns)) <$> parseFalse rs
          (Left e,_)    -> Left e
parseFalse (TLambdaEnd p:_) = Left $ MismatchedEndLambda p
parseFalse (TQuoted c:ts) = ((:) (ValNode (ord c)))  <$> parseFalse ts
parseFalse (TAdd:ts)      = ((:) (FuncNode FAdd))    <$> parseFalse ts
parseFalse (TSub:ts)      = ((:) (FuncNode FSub))    <$> parseFalse ts
parseFalse (TMul:ts)      = ((:) (FuncNode FMul))    <$> parseFalse ts
parseFalse (TDiv:ts)      = ((:) (FuncNode FDiv))    <$> parseFalse ts
parseFalse (TNeg:ts)      = ((:) (FuncNode FNeg))    <$> parseFalse ts
parseFalse (TEq:ts)       = ((:) (FuncNode FEq))     <$> parseFalse ts
parseFalse (TGt:ts)       = ((:) (FuncNode FGt))     <$> parseFalse ts
parseFalse (TNot:ts)      = ((:) (FuncNode FNot))    <$> parseFalse ts
parseFalse (TAnd:ts)      = ((:) (FuncNode FAnd))    <$> parseFalse ts
parseFalse (TOr:ts)       = ((:) (FuncNode FOr))     <$> parseFalse ts
parseFalse (TAssign:ts)   = ((:) (FuncNode FAssign)) <$> parseFalse ts
parseFalse (TRead:ts)     = ((:) (FuncNode FRead))   <$> parseFalse ts
parseFalse (TApply:ts)    = ((:) (FuncNode FApply))  <$> parseFalse ts
parseFalse (TDup:ts)      = ((:) (FuncNode FDup))    <$> parseFalse ts
parseFalse (TDel:ts)      = ((:) (FuncNode FDel))    <$> parseFalse ts
parseFalse (TSwap:ts)     = ((:) (FuncNode FSwap))   <$> parseFalse ts
parseFalse (TRot:ts)      = ((:) (FuncNode FRot))    <$> parseFalse ts
parseFalse (TPick:ts)     = ((:) (FuncNode FPick))   <$> parseFalse ts
parseFalse (TIf:ts)       = ((:) (FuncNode FIf))     <$> parseFalse ts
parseFalse (TWhile:ts)    = ((:) (FuncNode FWhile))  <$> parseFalse ts
parseFalse (TPrintI:ts)   = ((:) (FuncNode FPrintI)) <$> parseFalse ts
parseFalse (TPrintC:ts)   = ((:) (FuncNode FPrintC)) <$> parseFalse ts
parseFalse (TInput:ts)    = ((:) (FuncNode FInput))  <$> parseFalse ts
parseFalse (TFlush:ts)    = ((:) (FuncNode FFlush))  <$> parseFalse ts
parseFalse (TVar c:ts)    = ((:) (VarNode (Var c)))  <$> parseFalse ts
parseFalse (TVal n:ts)    = ((:) (ValNode n))        <$> parseFalse ts
parseFalse (TString s:ts) = ((:) (StringNode s))     <$> parseFalse ts



-- | Recurs through the lambda keeping track of the level for nested
-- lambdas
parseLambda :: Position Int Int -> [Token] -> Int
            -> (Either ParseError [Node],[Token])
parseLambda p [] 0 = (Right [],[])
parseLambda p [] _ = (Left $ LambdaNotClosed p,[])
parseLambda p (TLambdaEnd q:ts) n = (Right [],ts)
parseLambda p (TLambdaStart q:ts) n
    = case parseLambda q ts (n + 1) of
          (Right ns,rs) -> let (a,b) = parseLambda p rs n
                           in (((:) (LambdaNode $ Lambda ns))
                            <$> a,b)
          e             -> e
parseLambda p (t:ts) n = case parseFalse [t] of
                             Right [pn] -> let (a,b) = parseLambda p ts n
                                           in (((:) pn) <$> a,b)
                             Left e    -> (Left e,[])
