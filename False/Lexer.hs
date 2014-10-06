-- |
-- Module      : False.Lexer
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- False lexical analysis.
module False.Lexer
    ( lexFalse  -- :: String -> Either (LexError,Position) [Node]
    ) where


import Data.Char (isLetter,isNumber,isSpace)
import Data.List (genericLength)
import Control.Applicative ((<$>))


import False.Data (Token(..)
                  ,LexError(..)
                  ,Position(..)
                  ,incrPosition
                  ,incrNPosition
                  ,newLine
                  )


-- | Lex False source code with a starting position of p.
lexFalse :: Position Int Int -> String
         -> Either (LexError,Position Int Int) [Token]
lexFalse p ""          = Right []
lexFalse p ('\n':cs)   = lexFalse          (newLine p)         cs
lexFalse p ('{':cs)    = lexComment p (incrPosition p)    cs
lexFalse p ('[':cs)    = ((:) (TLambdaStart p)) <$> lexFalse (incrPosition p) cs
lexFalse p (']':cs)    = ((:) (TLambdaEnd   p)) <$> lexFalse (incrPosition p) cs
lexFalse p ('"':cs)    = lexString "" p             (incrPosition p) cs
lexFalse p "'"         = Left (NoCharToQuote,p)
lexFalse p ('\'':c:cs) = ((:) (TQuoted c))      <$> lexFalse (incrPosition p) cs
lexFalse p ('+':cs)    = ((:) TAdd)             <$> lexFalse (incrPosition p) cs
lexFalse p ('-':cs)    = ((:) TSub)             <$> lexFalse (incrPosition p) cs
lexFalse p ('*':cs)    = ((:) TMul)             <$> lexFalse (incrPosition p) cs
lexFalse p ('/':cs)    = ((:) TDiv)             <$> lexFalse (incrPosition p) cs
lexFalse p ('_':cs)    = ((:) TNeg)             <$> lexFalse (incrPosition p) cs
lexFalse p ('=':cs)    = ((:) TEq)              <$> lexFalse (incrPosition p) cs
lexFalse p ('>':cs)    = ((:) TGt)              <$> lexFalse (incrPosition p) cs
lexFalse p ('~':cs)    = ((:) TNot)             <$> lexFalse (incrPosition p) cs
lexFalse p ('&':cs)    = ((:) TAnd)             <$> lexFalse (incrPosition p) cs
lexFalse p ('|':cs)    = ((:) TOr)              <$> lexFalse (incrPosition p) cs
lexFalse p (':':cs)    = ((:) TAssign)          <$> lexFalse (incrPosition p) cs
lexFalse p (';':cs)    = ((:) TRead)            <$> lexFalse (incrPosition p) cs
lexFalse p ('!':cs)    = ((:) TApply)           <$> lexFalse (incrPosition p) cs
lexFalse p ('$':cs)    = ((:) TDup)             <$> lexFalse (incrPosition p) cs
lexFalse p ('%':cs)    = ((:) TDel)             <$> lexFalse (incrPosition p) cs
lexFalse p ('\\':cs)   = ((:) TSwap)            <$> lexFalse (incrPosition p) cs
lexFalse p ('@':cs)    = ((:) TRot)             <$> lexFalse (incrPosition p) cs
lexFalse p ('ø':cs)    = ((:) TPick)            <$> lexFalse (incrPosition p) cs
lexFalse p ('?':cs)    = ((:) TIf)              <$> lexFalse (incrPosition p) cs
lexFalse p ('#':cs)    = ((:) TWhile)           <$> lexFalse (incrPosition p) cs
lexFalse p ('.':cs)    = ((:) TPrintI)          <$> lexFalse (incrPosition p) cs
lexFalse p (',':cs)    = ((:) TPrintC)          <$> lexFalse (incrPosition p) cs
lexFalse p ('^':cs)    = ((:) TInput)           <$> lexFalse (incrPosition p) cs
lexFalse p ('ß':cs)    = ((:) TFlush)           <$> lexFalse (incrPosition p) cs
lexFalse p w@(c:cs)
    | isLetter c       = ((:) (TVar c))         <$> lexFalse (incrPosition p) cs
    | isSpace  c       = lexFalse (incrPosition p) cs
    | isNumber c       = lexVal                 p  w
    | otherwise        = Left (UnexpectedChar c,p)  -- Not sure what this is.



-- | Lex a comment. Keeps track of the postion.
-- Returns a CommentNotClosed if the '{' is not matched.
lexComment :: Position Int Int -> Position Int Int
           -> String -> Either (LexError,Position Int Int) [Token]
lexComment o p ""        = Left (CommentNotClosed,o)
lexComment o p ('}':cs)  = lexFalse (incrPosition p) cs
lexComment o p ('\n':cs) = lexComment o (newLine p) cs
lexComment o p (_:cs)    = lexComment o (incrPosition p) cs


-- | Lexes a string literal. Newlines are part of strings in False,
-- however, this keeps the position updated.
lexString :: String -> Position Int Int -> Position Int Int
          -> String -> Either (LexError,Position Int Int) [Token]
lexString as o p ""        = Left (StringNotClosed,o)
lexString as o p ('"':cs)  = ((:) (TString as)) <$> lexFalse (incrPosition p) cs
lexString as o p ('\n':cs) = lexString ('\n':as) o     (newLine      p) cs
lexString as o p (c:cs)    = lexString (c:as)    o     (incrPosition p) cs


-- | Lexes a numerical value.
lexVal :: Position Int Int -> String
       -> Either (LexError,Position Int Int) [Token]
lexVal p cs = let (v,rs) = span isNumber cs
              in ((:) (TVal (read v)))
                     <$> lexFalse (incrNPosition p (genericLength v)) rs
