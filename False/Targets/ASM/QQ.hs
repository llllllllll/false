-- |
-- Module      : False.Targets.ASM.Data
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- nasm quasiquoter.
{-# LANGUAGE TupleSections #-}
module False.Targets.ASM.Data
    ( nasm  -- :: QuasiQuoter
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (first,second,(***))
import Control.Monad (join)
import Data.Char (isSpace)
import Data.List (intercalate)
import Language.Haskell.TH (Q(..),Exp)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Numeric (showHex)


data NASM = Instruction Instr [Param]
          | Label (Maybe (Alloc,String)) String
          | Extern String
          | Section SectionType
          | Global String
          | Newline  -- Used for formatting.


instance Show NASM where
    show (Instruction inst ps) = let i = show inst
                                     n = length i
                                 in tab 0
                                        ++ i
                                        ++ if null ps
                                             then ""
                                             else tab n
                                               ++ intercalate "," (map show ps)
    show (Label (Just (a,as)) name) = let n = name ++ ":"
                                          l = length n
                                          t = show a
                                          m = length t
                                      in n ++ tab l
                                             ++ t ++ tab m
                                             ++ as
    show (Label Nothing name) = name ++ ":"
    show (Extern n)           = "extern " ++ n
    show (Section s)          = tab 0 ++ "section " ++ show s ++ "\n"
    show (Global s)           = "global " ++ s ++ "\n"
    show Newline              = "\n"

tab :: Int -> String
tab n = replicate (8 - n) ' '


data Instr = MOV
           | ADD
           | SUB
           | IMUL
           | IDIV
           | NEG
           | NOT
           | AND
           | OR
           | XOR
           | CMP
           | JMP
           | JE
           | JG
           | PUSH
           | POP
           | CALL
           | SYSCALL


instrs :: [Instr]
instrs = [ MOV
         , ADD
         , SUB
         , IMUL
         , IDIV
         , NEG
         , NOT
         , AND
         , OR
         , XOR
         , CMP
         , JMP
         , JE
         , JG
         , PUSH
         , POP
         , CALL
         , SYSCALL
         ]


instance Show Instr where
    show MOV     = "mov"
    show ADD     = "add"
    show SUB     = "sub"
    show IMUL    = "imul"
    show IDIV    = "idiv"
    show NEG     = "neg"
    show NOT     = "not"
    show AND     = "and"
    show OR      = "or"
    show XOR     = "xor"
    show CMP     = "cmp"
    show JMP     = "jmp"
    show JE      = "je"
    show JG      = "jg"
    show PUSH    = "push"
    show POP     = "pop"
    show CALL    = "call"
    show SYSCALL = "syscall"


data SectionType = DataSection
                 | BSSSection
                 | TextSection


instance Show SectionType where
    show DataSection = ".data"
    show BSSSection  = ".bss"
    show TextSection = ".text"


data Param = RegParam Register
           | LabelParam String
           | IntParam Int
           | MemRegParam (Maybe CastType) Register
           | MemLabelParam (Maybe CastType) String


instance Show Param where
    show (RegParam r)               = show r
    show (LabelParam s)             = s
    show (IntParam n)               = '0' : 'x' : if n < 0
                                                    then '-' : showHex (abs n)
                                                             ""
                                                    else showHex n ""
    show (MemRegParam (Just c) r)   = show c ++ ' ' : '[' : show r ++ "]"
    show (MemRegParam Nothing r)    = '[' : show r ++ "]"
    show (MemLabelParam (Just c) l) = show c ++ ' ' : '[' : l ++ "]"
    show (MemLabelParam Nothing l)  = '[' : l ++ "]"


data Alloc = DB
           | DQ
           | RESB
           | RESD
           | RESQ


instance Show Alloc where
    show DB   = "db"
    show DQ   = "dq"
    show RESB = "resb"
    show RESD = "resd"
    show RESQ = "resq"


data Register = RAX
              | RBX
              | RCX
              | RDX
              | RDI
              | RSI
              | R8
              | RSP


registers :: [Register]
registers = [ RAX
            , RBX
            , RCX
            , RDX
            , RDI
            , RSI
            , R8
            , RSP
            ]


instance Show Register where
    show RAX = "rax"
    show RBX = "rbx"
    show RCX = "rcx"
    show RDX = "rdx"
    show RDI = "rdi"
    show RSI = "rsi"
    show R8  = "r8"
    show RSP = "rsp"


data CastType = QWord


instance Show CastType where
    show QWord = "qword"


lexNasm :: String -> Q [NASM]
lexNasm "" = return []
lexNasm cs = lexNasm' $ second (dropWhile isSpace) $ break isSpace cs

lexNasm' :: (String,String) -> Q [NASM]
lexNasm' ("extern",cs) = lexExtern cs
                         >>= \(e,rs) -> (:) e <$> lexNasm rs
lexNasm' ("global",cs) = lexGlobal cs
                         >>= \(g,rs) -> (:) g <$> lexNasm rs
lexNasm' (w,cs@(c:cs'))
    | w `elem` map show instrs = lexInstruction w cs
                                 >>= \(i,rs) -> (:) i <$> lexNasm rs
    | c == ':' = lexLabel w cs'
                 >>= \(l,rs) -> (:) l <$> lexNasm rs


lexExtern :: String -> Q (NASM,String)
lexExtern "" = fail "extern expected a name"
lexExtern cs = let (s,rs) = break isSpace cs
               in case s of
                      "" -> fail "extern expected a name"
                      _  -> case first (dropWhile isSpace)
                            $ break (== '\n') rs of
                                ("",rs') -> return ( Extern s
                                                   , rs'
                                                   )
                                _        -> fail "extern declerations should be\
                                                 \ followed by a new line"


lexGlobal :: String -> Q (NASM,String)
lexGlobal "" = fail "global expected a name"
lexGlobal cs = let (g,rs) = break isSpace
               in case g of
                      "" -> fail "global expected a name"
                      _  -> case first (dropWhile isSpace)
                            $ break (== '\n') rs of
                                ("",rs') -> return ( Global g
                                                   , rs'
                                                   )
                                _        -> fail "global declerations should be\
                                                 \ followed by a new line"


lexInstruction :: String -> String -> Q (NASM,String)
lexInstruction "mov"     cs = lexTwoParams MOV
lexInstruction "add"     cs = lexTwoParams ADD     cs
lexInstruction "sub"     cs = lexTwoParams SUB     cs
lexInstruction "imul"    cs = lexTwoParams IMUL    cs
lexInstruction "idiv"    cs = lexTwoParams IDIV    cs
lexInstruction "neg"     cs = lexOneParam  NEG     cs
lexInstruction "not"     cs = lexOneParam  NOT     cs
lexInstruction "and"     cs = lexTwoParams AND     cs
lexInstruction "or"      cs = lexTwoParams OR      cs
lexInstruction "xor"     cs = lexTwoParams XOR     cs
lexInstruction "cmp"     cs = lexTwoParams CMP     cs
lexInstruction "jmp"     cs = lexOneParam  JMP     cs
lexInstruction "je"      cs = lexOneParam  JE      cs
lexInstruction "jg"      cs = lexOneParam  JG      cs
lexInstruction "push"    cs = lexOneParam  PUSH    cs
lexInstruction "pop"     cs = lexOneParam  POP     cs
lexInstruction "call"    cs = lexOneParam  CALL    cs
lexInstruction "syscall" cs
    = let f = dropWhile isSpace
      in case f *** f $ break (== '\n') of
             ("",rs) -> return (Instruction SYSCALL [],rs)
             _       -> fail "syscall expects no paramaters"


lexParam :: Instr -> String -> Q (Param,String)
lexParam i "" = fail $ show i ++ " expected a paramater"
lexParam i cs = let f = dropWhile isSpace
                in case f *** f $ break (`elem` "\n,") of
                       ("",_)          -> fail
                                          $ show i ++ " expected a paramater"
                       (cs,rs)
                           | cs `elem` map show registers ->
                               return (lexRegister cs,rs)
                           | otherwise -> liftM (,rs) $ lexLabelParam cs


nasm :: QuasiQuoter
nasm = QuasiQuoter { quoteExp = nasmQuoteExp
                   , quotePat = nasmQuotePat
                   }


nasmQuoteExp :: String -> Q Exp
nasmQuoteExp cs = undefined

nasmQuotePat :: String -> Q Pat
nasmQuotePat cs = undefined


