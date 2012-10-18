{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Soy.Types where

import qualified Data.Attoparsec.Number as J

import qualified Data.Text as T
import Control.Monad.Error

data EscapeMode
    = NoEscape
    | EscapeHtml
    | EscapeUri
    | EscapeJs
    deriving (Eq, Ord, Show)

data Namespace
    = Namespace
    { ns_path :: [Identifier]
    , ns_escapeMode :: Maybe EscapeMode
    } deriving (Eq, Ord, Show)

data File
    = File
    { file_namespace :: Namespace
    , file_templates :: [Template]
    } deriving (Eq, Ord, Show)

data Template
    = Template
    { tmpl_name :: Identifier
    , tmpl_content :: [Content]
    , tmpl_private :: Bool
    , tmpl_escapeMode :: Maybe EscapeMode
    } deriving (Eq, Ord, Show)

data Content
    = ContentText T.Text
    | ContentCommand Command
    deriving (Eq, Ord, Show)

type Identifier = T.Text

data TemplatePath
    = PathRelative Identifier
    | PathFull [Identifier]
    deriving (Eq, Ord, Show)

data Command
    = CommandText T.Text
    | CommandPrint PrintCommand
    | CommandIf IfCommand
    | CommandSwitch SwitchCommand
    | CommandForeach ForeachCommand
    | CommandFor ForCommand
    | CommandCall CallCommand
    deriving (Eq, Ord, Show)

data PrintDirective
    = PrintEscape EscapeMode
    | PrintId
    | PrintChangeNewlineToBr
    | PrintInsertWordBreaks Int
    | PrintTruncate Int Bool
    deriving (Eq, Ord, Show)

data PrintCommand
    = PrintCommand
    { print_expr :: Expr
    , print_directives :: [PrintDirective]
    }
    deriving (Eq, Ord, Show)

data CallParam
    = ParamExpr Expr
    | ParamTemplate [Content]
    deriving (Eq, Ord, Show)

data CallData
    = CallDataExpr Expr
    | CallDataAll
    deriving (Eq, Ord, Show)

data CallCommand
    = CallCommand
    { ccall_target :: TemplatePath
    , ccall_data :: Maybe CallData
    , ccall_params :: [(Identifier, CallParam)]
    } deriving (Eq, Ord, Show)

data ForCommand
    = ForCommand
    { cfor_iterator :: Identifier
    , cfor_from :: Expr
    , cfor_to :: Expr
    , cfor_step :: Expr
    , cfor_body :: [Content]
    } deriving (Eq, Ord, Show)

data ForeachCommand
    = ForeachCommand
    { cforeach_iterator :: Identifier
    , cforeach_collection :: Expr
    , cforeach_body :: [Content]
    , cforeach_ifempty :: Maybe [Content]
    } deriving (Eq, Ord, Show)

data IfCommand
    = IfCommand
    { cif_cases :: [(Expr, [Content])]
    , cif_otherwise :: Maybe [Content]
    } deriving (Eq, Ord, Show)

data SwitchCommand
    = SwitchCommand
    { switch_expr :: Expr
    , switch_cases :: [([Expr], [Content])]
    , switch_default :: Maybe [Content]
    } deriving (Eq, Ord, Show)

data Location
    = Location
    { loc_identifier :: Identifier
    , loc_path :: [Expr]
    } deriving (Eq, Ord, Show)

data Variable
    = GlobalVar  { var_loc :: Location }
    | LocalVar { var_loc :: Location }
    | InjectedVar { var_loc :: Location }
    deriving (Eq, Ord, Show)

data FuncCall
    = FuncCall
    { func_name :: Identifier
    , func_args :: [Expr]
    }
    deriving (Eq, Ord, Show)

data Expr
    = ExprOp OpExpr
    | ExprLiteral Literal
    | ExprVar Variable
    | ExprFuncCall FuncCall
    deriving (Eq, Ord, Show)

data Literal
    = LiteralNull
    | LiteralString T.Text
    | LiteralNumber J.Number
    | LiteralBool Bool
    | LiteralList [Expr]
    | LiteralMap [(Expr, Expr)]
    deriving (Eq, Ord, Show)

data OpExpr
    = OpNot Expr
    | OpNeg Expr
    | OpMul Expr Expr
    | OpDiv Expr Expr
    | OpMod Expr Expr
    | OpPlus Expr Expr
    | OpMinus Expr Expr
    | OpGreater Expr Expr
    | OpLess Expr Expr
    | OpGreaterEq Expr Expr
    | OpLessEq Expr Expr
    | OpEqual Expr Expr
    | OpNotEqual Expr Expr
    | OpAnd Expr Expr
    | OpOr Expr Expr
    | OpConditional Expr Expr Expr
    deriving (Eq, Ord, Show)

data SoyError
    = LookupError T.Text
    | TypeError T.Text
    | KeyError T.Text
    | TemplateLookupError T.Text
    | GeneralError T.Text
    | NotImplementedError T.Text
    deriving (Eq, Show)

instance Error SoyError where
    noMsg = GeneralError $ T.pack "Unknown error"
    strMsg s = GeneralError $ T.pack s
