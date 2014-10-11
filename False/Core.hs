-- |
-- Module      : False.Core
-- Copyright   : Joe Jevnik
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- False data structures.
{-# LANGUAGE RankNTypes, TypeFamilies #-}
module False.Core
    ( Position(..)     -- :: Instances: Eq, Show
    , Token(..)        -- :: Instances: Eq, Show
    , CompileError(..) -- :: Instances: Eq, Show
    , ParseError(..)   -- :: Instances: Eq, Show
    , LexError(..)     -- :: Instances: Eq, Show
    , Func(..)         -- :: Instances: Eq, Show
    , Var(..)          -- :: Instances: Eq, Show
    , Lambda(..)       -- :: Instances: Eq, Show
    , Node(..)         -- :: Instances: Eq, Show
    , PNode(..)
    , Target(..)
    , initPosition     -- :: (Integral a,Integral b) => Position a b
    , incrPosition     -- :: (Integral a,Integral b) => Position a b
                       --                            -> Position a b
    , incrNPosition    -- :: (Integral a,Integral b) => Position a b -> b
                       --                            -> Position a b
    , newLine          -- :: (Integral a,Integral b) => Position a b
                       --                            -> Position a b
    ) where


data Position a b = Position a b deriving (Eq)


initPosition :: (Integral a,Integral b) => Position a b
initPosition = Position 1 0


instance (Show a,Show b) => Show (Position a b) where
    show (Position r c) = show r ++ ':' : show c


incrPosition :: (Integral a,Integral b) => Position a b -> Position a b
incrPosition (Position r c) = Position r (c + 1)


incrNPosition :: (Integral a,Integral b) => Position a b -> b -> Position a b
incrNPosition (Position r c) n = Position r (c + n)


newLine :: (Integral a,Integral b) => Position a b -> Position a b
newLine (Position r c) = Position (r + 1) 0


data CompileError = CompileLexError LexError (Position Int Int)
                  | CompileParseError ParseError
                  | InvalidTarget String
                    deriving (Eq)


instance Show CompileError where
    show (CompileLexError e p) = show e ++ ": " ++ show p
    show (CompileParseError e) = show e
    show (InvalidTarget t)     = "Invalid target: " ++ show t ++ "\""


data ParseError = LambdaNotClosed (Position Int Int)
                | MismatchedEndLambda (Position Int Int)
                  deriving (Eq)


instance Show ParseError where
    show (LambdaNotClosed     p) = "Lambda not closed at: " ++ show p
    show (MismatchedEndLambda p) = "Uneexpected lambda end at: " ++ show p


data LexError = CommentNotClosed
              | StringNotClosed
              | NoCharToQuote
              | UnexpectedChar Char
                deriving (Eq)


instance Show LexError where
    show CommentNotClosed   = "Comment not closed"
    show StringNotClosed    = "String not closed"
    show NoCharToQuote      = "No char to quote"
    show (UnexpectedChar c) = "Unexpected char: '" ++ c : "'"


data Token = TAdd                            -- ^ +
           | TSub                            -- ^ -
           | TMul                            -- ^ *
           | TDiv                            -- ^ /
           | TNeg                            -- ^ _
           | TEq                             -- ^ =
           | TGt                             -- ^ >
           | TNot                            -- ^ ~
           | TAnd                            -- ^ &
           | TOr                             -- ^ |
           | TAssign                         -- ^ :
           | TRead                           -- ^ ;
           | TApply                          -- ^ !
           | TDup                            -- ^ $
           | TDel                            -- ^ %
           | TSwap                           -- ^ \
           | TRot                            -- ^ @
           | TPick                           -- ^ ø
           | TIf                             -- ^ ?
           | TWhile                          -- ^ #
           | TPrintI                         -- ^ .
           | TPrintC                         -- ^ ,
           | TInput                          -- ^ ^
           | TFlush                          -- ^ ß
           | TLambdaStart (Position Int Int) -- ^ [
           | TLambdaEnd (Position Int Int)   -- ^ ]
           | TVal Int                        -- ^ Int
           | TString String                  -- ^ "String"
           | TQuoted Char                    -- ^ 'Char
           | TVar Char                       -- ^ Char
             deriving (Eq)


instance Show Token where
    show TAdd              = "+"
    show TSub              = "-"
    show TMul              = "*"
    show TDiv              = "/"
    show TNeg              = "_"
    show TEq               = "="
    show TGt               = ">"
    show TNot              = "~"
    show TAnd              = "&"
    show TOr               = "|"
    show TAssign           = ":"
    show TRead             = ";"
    show TApply            = "!"
    show TDup              = "$"
    show TDel              = "%"
    show TSwap             = "\\"
    show TRot              = "@"
    show TPick             = "ø"
    show TIf               = "?"
    show TWhile            = "#"
    show TPrintI           = "."
    show TPrintC           = ","
    show TInput            = "^"
    show TFlush            = "ß"
    show (TLambdaStart _)  = "["
    show (TLambdaEnd   _)  = "]"
    show (TVal n)          = show n
    show (TString s)       = show s
    show (TQuoted c)       = '\'' : [c]
    show (TVar c)          = [c]


-- | The elementary Functions
data Func = FAdd     -- ^ +
          | FSub     -- ^ -
          | FMul     -- ^ *
          | FDiv     -- ^ /
          | FNeg     -- ^ _
          | FEq      -- ^ =
          | FGt      -- ^ >
          | FNot     -- ^ ~
          | FAnd     -- ^ &
          | FOr      -- ^ |
          | FAssign  -- ^ :
          | FRead    -- ^ ;
          | FApply   -- ^ !
          | FDup     -- ^ $
          | FDel     -- ^ %
          | FSwap    -- ^ \
          | FRot     -- ^ @
          | FPick    -- ^ ø
          | FIf      -- ^ ?
          | FWhile   -- ^ #
          | FPrintI  -- ^ .
          | FPrintC  -- ^ ,
          | FInput   -- ^ ^
          | FFlush   -- ^ ß
             deriving (Eq)


instance Show Func where
    show FAdd    = "+"
    show FSub    = "-"
    show FMul    = "*"
    show FDiv    = "/"
    show FNeg    = "_"
    show FEq     = "="
    show FGt     = ">"
    show FNot    = "~"
    show FAnd    = "&"
    show FOr     = "|"
    show FAssign = ":"
    show FRead   = ";"
    show FApply  = "!"
    show FDup    = "$"
    show FDel    = "%"
    show FSwap   = "\\"
    show FRot    = "@"
    show FPick   = "ø"
    show FIf     = "?"
    show FWhile  = "#"
    show FPrintI = "."
    show FPrintC = ","
    show FInput  = "^"
    show FFlush  = "ß"


-- | A variable
newtype Var = Var Char deriving (Eq)


instance Show Var where
    show (Var c) = [c]


-- | A list of 'Node's constructs a lambda.
-- 'Lambda's may have lambdas in them.
newtype Lambda = Lambda [Node] deriving (Eq)


instance Show Lambda where
    show (Lambda ns) = ('[' : concatMap show ns) ++ "]"


-- | A 'Node' is a single unit of code.
data Node = FuncNode Func
          | ValNode Int
          | VarNode Var
          | LambdaNode Lambda
          | StringNode String
            deriving (Eq)


instance Show Node where
    show (FuncNode f)   = show f
    show (ValNode n)    = show n
    show (VarNode v)    = show v
    show (LambdaNode l) = show l
    show (StringNode s) = show s



-- | A preprocessed node. These are ready to be output to a target lang.
data PNode = PLambda Int
           | PString Int String
           | PNode Node


-- | The target languages.
data Target = Target { compile     :: [(Int,String)] -> [(Int,[PNode])]
                                   -> [PNode] -> String
                     , defaultFile :: FilePath
                     }
