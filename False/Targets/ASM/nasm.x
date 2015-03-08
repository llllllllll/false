{
module Main (main) where
}

%wrapper "monad"

$alpha = [a-zA-Z]		-- alphabetic characters
$digit = 0-9                    -- digits
$hexdig = [0-9A-Fa-f]

tokens :-

    $white+                               ;
    ";".*                                 ;
    global$white+$digit+                  { \s _ -> 

{
-- Each action has type :: String -> Token

-- The token type:
data Token = MOV
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
           | QWORD
           | DB
           | DQ
           | RESB
           | RESD
           | RESQ
           | EXTERN String
           | GLOBAL String
        deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
