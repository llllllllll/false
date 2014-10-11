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
import Data.Char (ord)
import Data.List (intercalate)
import Data.STRef (STRef,newSTRef,readSTRef,modifySTRef')


import False.Targets.C.Operations (falseStr)
import False.Core (PNode(..),Target(..),Func(..),Node(..),Var(..))


type ASM = String


asmTarget :: Target
asmTarget = Target { compile     = asmCompile
                   , defaultFile = "a.asm"
                   }


asmCompile :: [(Int,String)] -> [(Int,[PNode])] -> [PNode] -> ASM
asmCompile ss ls ms
    = runST (newSTRef 0
             >>= \c -> mapM (defLambda c) (reverse ls)
             >>= \ls' -> defMain c ms
             >>= \ms' -> let ss' = "printifmt: db \"%ld\",0"
                                   : "\n    lsp: dq 0"
                                   : map declString ss
                             es  = "extern snprintf"
                         in return $ asmCompile' es ss' ls' ms')


asmCompile' :: ASM -> [ASM] -> [ASM] -> ASM -> ASM
asmCompile' es ss ls m = es ++ "\n\nsection .data\n    "
                         ++ intercalate "\n" ss ++ "\n\nsection .bss"
                         ++ declVars ++ "\n\n\nsection .text\nglobal main\n"
                         ++ intercalate "\n\n\n" ls ++ "\n\n" ++ m


-- | How many functions may be called.
stackSize :: Int
stackSize = 256


declVars :: ASM
declVars = indent
           $ ("lstack: resq " ++ show stackSize)
           : "io: resd 1"
           : "pb: resb 64"
           : [c : ": resq 1" | c <- ['a'..'z']]


declString :: (Int,String) -> ASM
declString (n,cs) = let n' = show n
                        s  = "str_" ++ n'
                    in indent [ s ++ " db " ++ mkStr cs ++ ",0"
                              , "len_" ++ n' ++ ": dq $-" ++ s
                              ]
  where
      mkStr = flp foldl (flip ($)) [(:) '`' . init . tail,flip (++) "`"] . show
        where
            flp f a b c = f a c b


asmToTarget :: STRef s Int -> PNode -> ST s ASM
asmToTarget c (PNode (FuncNode f))      = funcToASM c f
asmToTarget _ (PNode (ValNode n))       = return $ "\n    push " ++ show n
asmToTarget _ (PNode (VarNode (Var v))) = return $ "\n    push " ++ [v]
asmToTarget _ (PString n _)             = return $ writeOut n
asmToTarget _ (PLambda n)               = return $ pushLambda n


writeOut :: Int -> ASM
writeOut n = indent
             [ "mov rax,1"
             , "mov rdi,1"
             , "mov rsi,str_" ++ show n
             , "mov rdx,[len_" ++ show n ++ "]"
             , "syscall"
             ]


pushLambda :: Int -> ASM
pushLambda = (++) "\n    push lambda_" . show



indent :: [String] -> String
indent = flip (>>=) ("\n    " ++)


funcToASM :: STRef s Int -> Func -> ST s ASM
funcToASM _ FAdd    = return $ indent
                      [ "pop rax"
                      , "pop rbx"
                      , "add rax,rbx"
                      , "push rax"
                      ]
funcToASM _ FSub    = return $ indent
                      [ "pop rbx"
                      , "pop rax"
                      , "sub rax,rbx"
                      , "push rax"
                      ]
funcToASM _ FMul    = return $ indent
                      [ "pop rax"
                      , "pop rbx"
                      , "imul rax,rbx"
                      , "push rax"
                      ]
funcToASM _ FDiv    = return $ indent
                      [ "pop rax"
                      , "xor rdx,rdx"
                      , "pop rbx"
                      , "idiv rbx"
                      , "push rax"
                      ]
funcToASM _ FNeg    = return $ indent
                      [ "pop rax"
                      , "neg rax"
                      , "push rax"
                      ]
funcToASM n FEq     = readSTRef n
                      >>= \c -> modifySTRef' n (+ 1)
                      >>  let end = "eq_end_" ++ show c
                          in (return $ '\n' : intercalate "\n"
                                         [ "    pop rax"
                                         , "    pop rbx"
                                         , "    cmp rax,rbx"
                                         , "    push -1"
                                         , "    je " ++ end
                                         , "    pop rax"
                                         , "    push 0"
                                         , end ++ ":"
                                         ])
funcToASM n  FGt     = readSTRef n
                      >>= \c -> modifySTRef' n (+ 1)
                      >>  let end = "gt_end_" ++ show c
                          in (return $ '\n' : intercalate "\n"
                                         [ "    pop rbx"
                                         , "    pop rax"
                                         , "    push -1"
                                         , "    cmp rax,rbx"
                                         , "    jg " ++ end
                                         , "    pop rax"
                                         , "    push 0"
                                         , end ++ ":"
                                         ])
funcToASM _ FNot    = return $ indent
                      [ "pop rax"
                      , "not rax"
                      , "push rax"
                      ]
funcToASM _ FAnd    = return $ indent
                      [ "pop rax"
                      , "pop rbx"
                      , "and rax,rbx"
                      , "push rax"
                      ]
funcToASM _ FOr     = return $ indent
                      [ "pop rax"
                      , "pop rbx"
                      , "or rax,rbx"
                      , "push rax"
                      ]
funcToASM _ FAssign = return $ indent
                      [ "pop rax"
                      , "pop rbx"
                      , "mov [rax],rbx"
                      ]
funcToASM _ FRead   = return $ indent
                      [ "pop rax"
                      , "push qword [rax]"
                      ]
funcToASM n FApply  = readSTRef n
                      >>= \c -> modifySTRef' n (+ 1)
                      >>  return (let ret = "ret_" ++ show c
                                  in indent
                                         [ "pop rax"
                                         , "mov rbx,lstack"
                                         , "add rbx,[lsp]"
                                         , "mov qword [rbx]," ++ ret
                                         , "add qword [lsp],8"
                                         , "jmp rax"
                                         ] ++ '\n' : ret ++ ":")
funcToASM _ FDup    = return $ indent [ "push qword [rsp]" ]
funcToASM _ FDel    = return $ indent [ "add rsp,8" ]
funcToASM _ FSwap   = return $ indent
                      [ "pop rax"
                      , "pop rbx"
                      , "push rax"
                      , "push rbx"
                      ]
funcToASM _ FRot    = return $ indent
                      [ "pop rax"
                      , "pop rbx"
                      , "pop rcx"
                      , "push rax"
                      , "push rcx"
                      , "push rbx"
                      ]
funcToASM _ FPick   = return $ indent
                      [ "pop rax"
                      , "imul rax,8"
                      , "add rax,rsp"
                      , "push qword [rax]"
                    ]
funcToASM n FIf     = readSTRef n
                      >>= \c -> modifySTRef' n (+ 1)
                      >>  return (let endif = "endif_" ++ show c
                                  in indent
                                         [ "pop rax"
                                         , "pop rbx"
                                         , "cmp rbx,0"
                                         , "je " ++ endif
                                         , "mov rcx,lstack"
                                         , "add rcx,[lsp]"
                                         , "mov qword [rcx]," ++ endif
                                         , "add qword [lsp],8"
                                         , "jmp rax"
                                         ] ++ "\n" ++ endif ++ ":")
funcToASM n FWhile  = readSTRef n
                      >>= \c -> modifySTRef' n (+ 1)
                      >>  let end = "end_" ++ show c
                              beg = "beg_" ++ show c
                          in return $ '\n' : intercalate "\n"
                                 [ "    pop rax"
                                 , "    pop rbx"
                                 , "    mov rcx,lstack"
                                 , "    add rcx,[lsp]"
                                 , "    mov qword [rcx]," ++ end
                                 , "    add qword [lsp],8"
                                 , "    jmp [rbx]"
                                 , beg ++ ":"
                                 , "    pop rdx"
                                 , "    cmp rdx,0"
                                 , "    je " ++ end
                                 , "    mov rcx,lstack"
                                 , "    add rcx,[lsp]"
                                 , "    mov qword [rcx]," ++ end
                                 , "    add qword [lsp],8"
                                 , "    jmp rax"
                                 , "    jmp " ++ beg
                                 , end ++ ":"
                      ]
funcToASM n FPrintI = return $ indent
                      [ "pop rcx"
                      , "mov rdi,pb"
                      , "mov rsi,64"
                      , "mov rdx,printifmt"
                      , "call snprintf"
                      , "mov rdx,rax"
                      , "mov rax,1"
                      , "mov rdi,1"
                      , "mov rsi,pb"
                      , "syscall"
                      ]
funcToASM _ FPrintC = return $ indent
                      [ "pop rax"
                      , "mov [io],rax"
                      , "mov rax,1"
                      , "mov rdi,1"
                      , "mov rsi,io"
                      , "mov rdx,1"
                      , "syscall"
                      ]
funcToASM _ FInput  = return $ indent
                      [ "mov rax,0"
                      , "mov rdi,0"
                      , "mov rsi,io"
                      , "mov rdx,1"
                      , "syscall"
                      , "mov rax,[io]"
                      , "push rax"
                      ]
funcToASM _ FFlush  = return ""


-- | Defines lambda_n in the source code.
defLambda :: STRef s Int -> (Int,[PNode]) -> ST s ASM
defLambda c (n,ps)
    = writePNodes c ps
      >>= \cs -> return $ "lambda_" ++ show n ++ ":"
                 ++ cs ++ indent [ "sub qword [lsp],8"
                                 , "mov r8,[lsp]"
                                 , "add r8,lstack"
                                 , "jmp [r8]"
                                 ]


defMain :: STRef s Int -> [PNode] -> ST s ASM
defMain c ps = writePNodes c ps
               >>= \cs -> return $ "main:\n" ++ cs ++ indent [ "mov rax,60"
                                                             , "mov rbx,0"
                                                             , "syscall"
                                                             ]


writePNodes :: STRef s Int -> [PNode] -> ST s ASM
writePNodes c ns = concat <$> mapM (asmToTarget c) ns
