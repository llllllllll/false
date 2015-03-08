-- |
-- Module      : False.Targets.ASM
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- PNodes to nasm

{-
Program structure:

64 bit nasm is the target.

This stack is used as the actual stack.
For this reason, a second call stack must exist so that lambdas
may affect the stack without trampling the return address.
The callstack is stored in lstack, the return pointer is [lstack + lsp - 8].
-}

module False.Targets.ASM
    ( asmTarget  -- :: Target
    ) where


import Control.Applicative ((<$>))
import Control.Monad.ST (ST,runST)
import Control.Parallel.Strategies (parMap,rseq)
import Data.Char (ord)
import Data.List (intercalate)
import Data.STRef (STRef,newSTRef,readSTRef,modifySTRef')
import Numeric (showHex)


import False.Core (PNode(..),Target(..),Func(..),Node(..),Var(..))


data ASM = Instruction Instr [Param]
         | Label (Maybe (Alloc,String)) String
         | Extern String
         | Section SectionType
         | Global String
         | Newline  -- Used for formatting.


instance Show ASM where
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


asmTarget :: Target
asmTarget = Target { compile     = asmCompile
                   , defaultFile = "a.asm"
                   }


asmCompile :: [(Int,String)] -> [(Int,[PNode])] -> [PNode] -> String
asmCompile ss ls ms
    = runST (newSTRef 0
             >>= \c -> concat <$> mapM (defLambda c) (reverse ls)
             >>= \ls' -> defMain c ms
             >>= \ms' -> let ss' = Label (Just (DB,"`%ld`")) "fmt"
                                   : Label (Just (DQ,"0")) "lsp"
                                   : (ss >>= declString)
                             es  = [ Extern "snprintf" ]
                         in return $ intercalate "\n" $ parMap rseq show
                                $ asmCompile' es ss' ls' ms')


asmCompile' :: [ASM] -> [ASM] -> [ASM] -> [ASM] -> [ASM]
asmCompile' es ss ls m = es ++ Newline : Section DataSection : ss
                         ++ Newline : Section BSSSection : declVars
                         ++ Newline : Section TextSection
                         : Global "main" : ls ++ m


-- | How many functions may be called.
stackSize :: Int
stackSize = 256


declVars :: [ASM]
declVars = Label (Just (RESQ,show stackSize)) "lstack"
           : Label (Just (RESD,"1")) "io"
           : Label (Just (RESB,"64")) "pb"
           : parMap rseq (Label (Just (RESQ,"1")) . flip (:) []) ['a'..'z']


declString :: (Int,String) -> [ASM]
declString (n,cs) = let n' = show n
                        s  = "str_" ++ n'
                    in [ Label (Just (DB,mkStr cs ++ ",0")) s
                       , Label (Just (DQ,"$-" ++ s)) $ "len_" ++ show n
                       ]
  where
      mkStr = flp foldl (flip ($)) [(:) '`' . init . tail,flip (++) "`"] . show
        where
            flp f a b c = f a c b


asmToTarget :: STRef s Int -> PNode -> ST s [ASM]
asmToTarget c (PNode (FuncNode f))      = funcToASM c f
asmToTarget _ (PNode (ValNode n))       = return $ pushVal n
asmToTarget _ (PNode (VarNode (Var v))) = return $ pushVar v
asmToTarget _ (PString n _)             = return $ writeOut n
asmToTarget _ (PLambda n)               = return $ pushLambda n


pushVal :: Int -> [ASM]
pushVal n = [ Instruction PUSH [ IntParam n ] ]


pushVar :: Char -> [ASM]
pushVar v = [ Instruction PUSH [ LabelParam [v]] ]


writeOut :: Int -> [ASM]
writeOut n = [ Instruction MOV [ RegParam RAX
                               , IntParam 1
                               ]
             , Instruction MOV [ RegParam RDI
                               , IntParam 1
                               ]
             , Instruction MOV [ RegParam RSI
                               , LabelParam $ "str_" ++ show n
                               ]
             , Instruction MOV [ RegParam RDX
                               , MemLabelParam Nothing $ "len_" ++ show n
                               ]
             , Instruction SYSCALL []
             ]


pushLambda :: Int -> [ASM]
pushLambda n = [ Instruction PUSH [ LabelParam $ "lambda_" ++ show n ] ]


funcToASM :: STRef s Int -> Func -> ST s [ASM]
funcToASM _ FAdd    = return
                      [ Instruction POP  [ RegParam RAX ]
                      , Instruction POP  [ RegParam RBX ]
                      , Instruction ADD  [ RegParam RAX
                                         , RegParam RBX
                                         ]
                      , Instruction PUSH [ RegParam RAX ]
                      ]

funcToASM _ FSub    = return
                      [ Instruction POP  [ RegParam RBX ]
                      , Instruction POP  [ RegParam RAX ]
                      , Instruction SUB  [ RegParam RAX
                                         , RegParam RBX
                                         ]
                      , Instruction PUSH [ RegParam RAX ]
                      ]
funcToASM _ FMul    = return
                      [ Instruction POP  [ RegParam RAX ]
                      , Instruction POP  [ RegParam RBX ]
                      , Instruction IMUL [ RegParam RAX
                                         , RegParam RBX
                                         ]
                      , Instruction PUSH [ RegParam RAX ]
                      ]
funcToASM _ FDiv    = return
                      [ Instruction POP  [ RegParam RAX ]
                      , Instruction XOR  [ RegParam RDX
                                         , RegParam RDX
                                         ]
                      , Instruction POP  [ RegParam RBX ]
                      , Instruction IDIV [ RegParam RBX ]
                      , Instruction PUSH [ RegParam RAX ]
                      ]
funcToASM _ FNeg    = return
                      [ Instruction POP  [ RegParam RAX ]
                      , Instruction NEG  [ RegParam RAX ]
                      , Instruction PUSH [ RegParam RAX ]
                      ]
funcToASM n FEq     = readSTRef n
                      >>= \c -> modifySTRef' n (+ 1)
                      >>  let end = "eq_end_" ++ show c
                          in return
                                 [ Instruction POP  [ RegParam RAX ]
                                 , Instruction POP  [ RegParam RBX ]
                                 , Instruction CMP  [ RegParam RAX
                                                    , RegParam RBX
                                                    ]
                                 , Instruction PUSH [ IntParam (-1) ]
                                 , Instruction JE   [ LabelParam end ]
                                 , Instruction POP  [ RegParam RAX ]
                                 , Instruction PUSH [ IntParam 0 ]
                                 , Label Nothing end
                                 ]
funcToASM n  FGt     = readSTRef n
                      >>= \c -> modifySTRef' n (+ 1)
                      >>  let end = "gt_end_" ++ show c
                          in return
                                 [ Instruction POP  [ RegParam RBX ]
                                 , Instruction POP  [ RegParam RAX ]
                                 , Instruction PUSH [ IntParam (-1) ]
                                 , Instruction CMP  [ RegParam RAX
                                                    , RegParam RBX
                                                    ]
                                 , Instruction JG   [ LabelParam end ]
                                 , Instruction POP  [ RegParam RAX ]
                                 , Instruction PUSH [ IntParam 0 ]
                                 , Label Nothing end
                                 ]
funcToASM _ FNot    = return
                      [ Instruction POP  [ RegParam RAX ]
                      , Instruction NOT  [ RegParam RAX ]
                      , Instruction PUSH [ RegParam RAX ]
                      ]
funcToASM _ FAnd    = return
                      [ Instruction POP  [ RegParam RAX ]
                      , Instruction POP  [ RegParam RBX ]
                      , Instruction AND  [ RegParam RAX
                                         , RegParam RBX
                                         ]
                      , Instruction PUSH [ RegParam RAX ]
                      ]

funcToASM _ FOr     = return
                      [ Instruction POP  [ RegParam RAX ]
                      , Instruction POP  [ RegParam RBX ]
                      , Instruction OR  [ RegParam RAX
                                         , RegParam RBX
                                         ]
                      , Instruction PUSH [ RegParam RAX ]
                      ]

funcToASM _ FAssign = return
                      [ Instruction POP [ RegParam RAX ]
                      , Instruction POP [ RegParam RBX ]
                      , Instruction MOV [ MemRegParam Nothing RAX
                                        , RegParam RBX
                                        ]
                      ]
funcToASM _ FRead   = return
                      [ Instruction POP  [ RegParam RAX ]
                      , Instruction PUSH [ MemRegParam (Just QWord) RAX ]
                      ]
funcToASM n FApply  = readSTRef n
                      >>= \c -> modifySTRef' n (+ 1)
                      >> let ret = "ret_" ++ show c
                         in return
                                [ Instruction POP [ RegParam RAX ]
                                , Instruction MOV [ RegParam RBX
                                                  , LabelParam "lstack"
                                                  ]
                                , Instruction ADD [ RegParam RBX
                                                  , MemLabelParam Nothing "lsp"
                                                  ]
                                , Instruction MOV [ MemRegParam (Just QWord) RBX
                                                  , LabelParam ret
                                                  ]
                                , Instruction ADD [ MemLabelParam (Just QWord)
                                                                      "lsp"
                                                  , IntParam 8
                                                  ]
                                , Instruction JMP [ RegParam RAX ]
                                , Label Nothing ret
                                ]
funcToASM _ FDup    = return
                      [ Instruction PUSH [ MemRegParam (Just QWord) RSP] ]
funcToASM _ FDel    = return
                      [ Instruction ADD [ RegParam RSP
                                        , IntParam 8
                                        ]
                      ]
funcToASM _ FSwap   = return
                      [ Instruction POP  [ RegParam RAX ]
                      , Instruction POP  [ RegParam RBX ]
                      , Instruction PUSH [ RegParam RAX ]
                      , Instruction PUSH [ RegParam RBX ]
                      ]
funcToASM _ FRot    = return
                      [ Instruction POP  [ RegParam RAX ]
                      , Instruction POP  [ RegParam RBX ]
                      , Instruction POP  [ RegParam RCX ]
                      , Instruction PUSH [ RegParam RAX ]
                      , Instruction PUSH [ RegParam RCX ]
                      , Instruction PUSH [ RegParam RBX ]
                      ]
funcToASM _ FPick   = return
                      [ Instruction POP  [ RegParam RAX ]
                      , Instruction IMUL [ RegParam RAX
                                         , IntParam 8
                                         ]
                      , Instruction ADD  [ RegParam RAX
                                         , RegParam RSP
                                         ]
                      , Instruction PUSH [ MemRegParam (Just QWord) RAX ]
                      ]
funcToASM n FIf     = readSTRef n
                      >>= \c -> modifySTRef' n (+ 1)
                      >> let endif = "endif_" ++ show c
                         in return
                                [ Instruction POP [ RegParam RAX ]
                                , Instruction POP [ RegParam RBX ]
                                , Instruction CMP [ RegParam RBX
                                                  , IntParam 0
                                                  ]
                                , Instruction JE  [ LabelParam endif ]
                                , Instruction MOV [ RegParam RCX
                                                  , LabelParam "lstack"
                                                  ]
                                , Instruction ADD [ RegParam RCX
                                                  , MemLabelParam Nothing "lsp"
                                                  ]
                                , Instruction MOV [ MemRegParam (Just QWord) RCX
                                                  , LabelParam endif
                                                  ]
                                , Instruction ADD [ MemLabelParam (Just QWord)
                                                                      "lsp"
                                                  , IntParam 8
                                                  ]
                                , Instruction JMP [ RegParam RAX ]
                                , Label Nothing endif
                                ]
funcToASM n FWhile  = readSTRef n
                      >>= \c -> modifySTRef' n (+ 1)
                      >> let end = "end_" ++ show c
                             beg = "beg_" ++ show c
                         in return
                                [ Instruction POP [ RegParam RAX ]
                                , Instruction POP [ RegParam RBX ]
                                , Instruction MOV [ RegParam RCX
                                                  , LabelParam "lstack"
                                                  ]
                                , Instruction ADD [ RegParam RCX
                                                  , MemLabelParam Nothing "lsp"
                                                  ]
                                , Instruction MOV [ MemRegParam (Just QWord) RCX
                                                  , LabelParam end
                                                  ]
                                , Instruction ADD [ MemLabelParam (Just QWord)
                                                                      "lsp"
                                                  , IntParam 8
                                                  ]
                                , Instruction JMP [ MemRegParam Nothing RBX ]
                                , Label Nothing beg
                                , Instruction POP [ RegParam RDX ]
                                , Instruction CMP [ RegParam RDX
                                                  , IntParam 0
                                                  ]
                                , Instruction JE  [ LabelParam end ]
                                , Instruction MOV [ RegParam RCX
                                                  , LabelParam "lstack"
                                                  ]
                                , Instruction ADD [ RegParam RCX
                                                  , MemLabelParam Nothing "lsp"
                                                  ]
                                , Instruction MOV [ MemRegParam (Just QWord) RCX
                                                  , LabelParam end
                                                  ]
                                , Instruction ADD [ MemLabelParam (Just QWord)
                                                                      "lsp"
                                                  , IntParam 8
                                                  ]
                                , Instruction JMP [ RegParam RAX ]
                                , Instruction JMP [ LabelParam beg ]
                                , Label Nothing end
                                ]
funcToASM n FPrintI = return
                      [ Instruction POP  [ RegParam RCX ]
                      , Instruction MOV  [ RegParam RDI
                                         , LabelParam "pb"
                                         ]
                      , Instruction MOV  [ RegParam RSI
                                         , IntParam 64
                                         ]
                      , Instruction MOV  [ RegParam RDX
                                         , LabelParam "fmt"
                                         ]
                      , Instruction CALL [ LabelParam "snprintf" ]
                      , Instruction MOV  [ RegParam RDX
                                         , RegParam RAX
                                         ]
                      , Instruction MOV  [ RegParam RAX
                                         , IntParam 1
                                         ]
                      , Instruction MOV  [ RegParam RDI
                                         , IntParam 1
                                         ]
                      , Instruction MOV  [ RegParam RSI
                                         , LabelParam "pb"
                                         ]
                      , Instruction SYSCALL []
                      ]
funcToASM _ FPrintC = return
                      [ Instruction POP [ RegParam RAX ]
                      , Instruction MOV [ MemLabelParam Nothing "io"
                                        , RegParam RAX
                                        ]
                      , Instruction MOV [ RegParam RAX
                                        , IntParam 1
                                        ]
                      , Instruction MOV [ RegParam RDI
                                        , IntParam 1
                                        ]
                      , Instruction MOV [ RegParam RSI
                                        , LabelParam "io"
                                        ]
                      , Instruction MOV [ RegParam RDX
                                        , IntParam 1
                                        ]
                      , Instruction SYSCALL []
                      ]
funcToASM _ FInput  = return
                      [ Instruction MOV  [ RegParam RAX
                                         , IntParam 0
                                         ]
                      , Instruction MOV  [ RegParam RDI
                                         , IntParam 0
                                         ]
                      , Instruction MOV  [ RegParam RSI
                                         , LabelParam "io"
                                         ]
                      , Instruction MOV  [ RegParam RDX
                                         , IntParam 1
                                         ]
                      , Instruction SYSCALL []
                      , Instruction PUSH [ MemLabelParam (Just QWord) "io" ]
                      ]
funcToASM _ FFlush  = return []  -- hehe, unbuffered io...


-- | Defines lambda_n in the source code.
defLambda :: STRef s Int -> (Int,[PNode]) -> ST s [ASM]
defLambda c (n,ps)
    = writePNodes c ps
      >>= \cs -> return $ (Label Nothing ("lambda_" ++ show n) : cs)
                 ++ [ Instruction SUB [ MemLabelParam (Just QWord) "lsp"
                                      , IntParam 8
                                      ]
                    , Instruction MOV [ RegParam R8
                                      , MemLabelParam Nothing "lsp"
                                      ]
                    , Instruction ADD [ RegParam R8
                                      , LabelParam "lstack"
                                      ]
                    , Instruction JMP [ MemRegParam Nothing R8 ]
                    , Newline
                    , Newline
                    ]


defMain :: STRef s Int -> [PNode] -> ST s [ASM]
defMain c ps = writePNodes c ps
               >>= \cs -> return $ (Label Nothing "main" : cs)
                          ++ [ Instruction MOV [ RegParam RAX
                                               , IntParam 60
                                               ]
                             , Instruction MOV [ RegParam RBX
                                               , IntParam 0
                                               ]
                             , Instruction SYSCALL []
                             ]


writePNodes :: STRef s Int -> [PNode] -> ST s [ASM]
writePNodes c = fmap concat . mapM (asmToTarget c)
