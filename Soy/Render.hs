{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Soy.Render where

import Soy.JSON()
import Soy.Parser (parseSoyFile)
import Soy.Sanitization
import Soy.Types

import qualified Data.Aeson.Encode as J
import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM
import Safe
import Test.Framework

import Control.Applicative
import Control.Arrow
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Random

data Function
    = PureFunction ([Value] -> Either SoyError Value)
    | SpecialFunction ([Expr] -> RenderM Value)

data Call
    = Call
    { call_file :: File
    , call_template :: Template
    , call_variables :: HM.HashMap Identifier Value
    } deriving (Eq, Show)

data LocalVar
    = ForeachVar { var_value :: Value
                 , foreach_index :: Int
                 , foreach_total :: Int }
    | SimpleVar { var_value :: Value }
    deriving (Eq, Show)

data RenderConfig
    = RenderConfig
    { cfg_files :: [File]
    , cfg_globals :: HM.HashMap Identifier Value
    , cfg_injected :: HM.HashMap Identifier Value
    , cfg_functions :: HM.HashMap Identifier Function
    , cfg_initState :: RenderState
    }

data RenderContext
    = RenderContext
    { ctx_currentCall :: Call
    , ctx_callStack :: [Call]
    , ctx_localStack :: [(Identifier, LocalVar)]
    }

data RenderState
    = RenderState
    { state_random :: StdGen
    }

type RenderM a = StateT RenderState (ReaderT (RenderContext, RenderConfig) (Either SoyError)) a

runRenderM cfg ctx x = runReaderT (evalStateT x (cfg_initState cfg)) (ctx, cfg)

asksContext getter = asks (getter . fst)
asksConfig getter = asks (getter . snd)
localContext f = local $ first f

defaultRenderState = RenderState (mkStdGen 1337)
defaultRenderConfig =
    RenderConfig [] HM.empty HM.empty (HM.fromList builtinFunctions) defaultRenderState

configure :: (Monad m) => RenderConfig -> StateT RenderConfig m () -> m RenderConfig
configure s inner = execStateT inner s

setup :: (Monad m) => StateT RenderConfig m () -> m RenderConfig
setup = configure defaultRenderConfig

modifyState f = modify (\c -> c { cfg_initState = f (cfg_initState c) })

setRandomSeed :: (MonadState RenderConfig m) => StdGen -> m ()
setRandomSeed gen = modifyState (\s -> s { state_random = gen })

setIOSeed :: (MonadState RenderConfig m, MonadIO m) => m ()
setIOSeed = liftIO newStdGen >>= setRandomSeed

addFiles :: (MonadState RenderConfig m, MonadIO m) => [FilePath] -> m ()
addFiles fps =
    do files <- mapM (parseSoyFile <=< liftIO . T.readFile) fps
       modifyFiles (files++)
    where modifyFiles f = modify $ \s -> s { cfg_files = f (cfg_files s) }

addIjData :: (MonadState RenderConfig m) => [(Identifier, Value)] -> m ()
addIjData vars = modifyIj $ HM.union (HM.fromList vars)
    where modifyIj f = modify $ \s -> s { cfg_injected = f (cfg_injected s) }

addGlobalData :: (MonadState RenderConfig m) => [(Identifier, Value)] -> m ()
addGlobalData vars = modifyGlobals $ HM.union (HM.fromList vars)
    where modifyGlobals f = modify $ \s -> s { cfg_globals = f (cfg_globals s) }

addFunction :: (MonadState RenderConfig m)
            => Identifier -> ([Value] -> Either SoyError Value)
            -> m ()
addFunction name func = modifyFunctions $ HM.insert name (PureFunction func)
    where modifyFunctions f = modify (\s -> s { cfg_functions = f (cfg_functions s) })

render :: (MonadError SoyError m)
       => [Identifier]
       -> [(Identifier, Value)]
       -> RenderConfig
       -> m T.Text
render path vars cfg =
    do (file, tmpl) <- findTemplateAbs path cfg
       let ctx = RenderContext (Call file tmpl (HM.fromList vars)) [] []
       let contents = renderContents (tmpl_content tmpl)
       either throwError return $ runRenderM cfg ctx contents

pushCall :: Call -> RenderContext -> RenderContext
pushCall call ctx = ctx
    { ctx_currentCall = call
    , ctx_callStack = (ctx_currentCall ctx : ctx_callStack ctx)
    }

withTemplate :: File -> Template -> HM.HashMap Identifier Value -> RenderM a -> RenderM a
withTemplate file tmpl vars = localContext $ pushCall (Call file tmpl vars)

withLocal :: Identifier -> LocalVar -> RenderM a -> RenderM a
withLocal name var = localContext $ pushLocal
    where pushLocal ctx = ctx { ctx_localStack = (name, var) : ctx_localStack ctx }

filePath = ns_path . file_namespace

findTemplateAbs :: (MonadError SoyError m)
                => [Identifier]
                -> RenderConfig
                -> m (File, Template)
findTemplateAbs path cfg =
    case filter (\(p,_,_) -> path == p) templates of
        [] -> throwError $ TemplateLookupError $ T.append "Template not found: " pathStr
        (_:_:_) -> throwError $ TemplateLookupError $ T.append "Template path not unique: " pathStr
        [(_,f,t)] -> return (f, t)
    where templates = concatMap templatesWithPathAndFile $ cfg_files cfg
          templatesWithPathAndFile f = map (mapper f) (file_templates f)
          mapper f t = (filePath f ++ [tmpl_name t], f, t)
          pathStr = T.intercalate "." path

findTemplate :: TemplatePath -> RenderM (File, Template)
findTemplate (PathFull path) = asksConfig id >>= findTemplateAbs path
findTemplate (PathRelative path) =
    do curfile <- liftM call_file getCurrentCall
       cfg <- asksConfig id
       findTemplateAbs (filePath curfile ++ [path]) cfg

renderTemplate :: HM.HashMap Identifier Value -> File -> Template -> RenderM T.Text
renderTemplate vars file template =
    do withTemplate file template vars $ renderContents (tmpl_content template)

renderContent :: Content -> RenderM T.Text
renderContent (ContentText t) = return t
renderContent (ContentCommand c) = renderCommand c

renderContents conts = liftM T.concat $ mapM renderContent conts

renderCommand :: Command -> RenderM T.Text
renderCommand cmd =
    case cmd of
        CommandText t -> return t
        CommandPrint p -> renderPrintCommand p
        CommandIf c -> renderIfCommand c
        CommandForeach c -> renderForeachCommand c
        CommandFor c -> renderForCommand c
        CommandCall c -> renderCallCommand c
        CommandSwitch c -> renderSwitchCommand c

defaultEscapeMode = EscapeHtml

getDefaultEscaping :: RenderM EscapeMode
getDefaultEscaping =
    do call <- getCurrentCall
       return $ fromMaybe defaultEscapeMode $ msum [tmplEsc call, fileEsc call]
    where fileEsc call = ns_escapeMode $ file_namespace $ call_file $ call
          tmplEsc call = tmpl_escapeMode $ call_template $ call

escape :: T.Text -> EscapeMode -> T.Text
escape str mode =
    case mode of
        NoEscape -> str
        EscapeUri -> escapeUri str
        EscapeHtml -> escapeHtml str
        EscapeJs -> escapeJs str

insertWordBreaks :: Int -> T.Text -> T.Text
insertWordBreaks thresh str = T.unwords $ map processWord $ T.words str
    where processWord w = T.intercalate "<wbr>" $ T.chunksOf thresh w

truncateEllipsis :: Int -> T.Text -> T.Text
truncateEllipsis size str =
    if T.length str > size then T.concat [T.take (size - 3) str, "..."]
                           else str

applyDirective :: T.Text -> PrintDirective -> T.Text
applyDirective str dir =
    case dir of
        PrintEscape mode -> escape str mode
        PrintId -> str
        PrintChangeNewlineToBr -> T.replace "\n" "<br />" str
        PrintInsertWordBreaks threshold -> insertWordBreaks threshold str
        PrintTruncate size ellipsis ->
            (if ellipsis then truncateEllipsis else T.take) size str

renderPrintCommand :: PrintCommand -> RenderM T.Text
renderPrintCommand (PrintCommand exp directives) =
    do str <- valToString <$> evalExprDefined exp
       let str' = foldl applyDirective str directives
       if any stopDefaultEscape directives
            then return str'
            else liftM (escape str') getDefaultEscaping
    where stopDefaultEscape (PrintEscape _) = True
          stopDefaultEscape PrintId = True
          stopDefaultEscape _ = False

evalCases :: Maybe [Content] -> [(RenderM Bool, [Content])] -> RenderM T.Text
evalCases fallback cases =
        findM fst (cases ++ fallback') >>= maybe (return "") (renderContents . snd)
    where fallback' = maybe [] (\x -> [(return True, x)]) fallback

renderIfCommand :: IfCommand -> RenderM T.Text
renderIfCommand (IfCommand cases otherwise) =
    evalCases otherwise $ map (first evalBoolean) cases

renderSwitchCommand :: SwitchCommand -> RenderM T.Text
renderSwitchCommand (SwitchCommand exp cases otherwise) =
        evalCases otherwise $ map (first condition) cases
    where condition exps = liftM2 elem (evalExpr exp) (mapM evalExpr exps)

renderForCommand :: ForCommand -> RenderM T.Text
renderForCommand (ForCommand iter fromExp toExp stepExp body) =
    do from <- evalInt fromExp
       to <- evalInt toExp
       step <- evalInt stepExp
       T.concat <$> mapM (iteration . ValInt) [from, from + step .. to - 1]
    where iteration item = withLocal iter (SimpleVar item) $ renderContents body

renderForeachCommand :: ForeachCommand -> RenderM T.Text
renderForeachCommand (ForeachCommand iter exp body ifempty) =
    do coll <- evalExprDefined exp
       case coll of
          ValList [] -> renderIfEmpty
          ValList lst -> renderNonEmpty lst
          _ -> typeError "Can only iterate over lists"
    where renderIfEmpty = maybe (return "") renderContents ifempty
          renderNonEmpty lst = T.concat <$> iterations lst
          iterations lst = mapM (iteration (length lst)) $ zip lst [0..]
          iteration total (item, idx) =
              withLocal iter (ForeachVar item idx total) $ renderContents body

getCurrentCall :: RenderM Call
getCurrentCall = asksContext ctx_currentCall

renderCallCommand :: CallCommand -> RenderM T.Text
renderCallCommand (CallCommand target calldata params) =
    do (file, targetTempl) <- findTemplate target
       params' <- HM.fromList <$> processParams <$> mapM mapper params
       additional' <- evalAdditional
       renderTemplate (HM.union params' additional') file targetTempl
    where evalParam (ParamExpr exp) = evalExpr exp
          evalParam (ParamTemplate conts) = Just . ValString <$> renderContents conts
          mapper (name, arg) = (name,) <$> evalParam arg
          processParams lst = [ (k, v) | (k, Just v) <- lst ]
          evalAdditional = case calldata of
                              Just CallDataAll -> call_variables <$> getCurrentCall
                              Just (CallDataExpr exp) -> evalExprDefined exp >>= mapOrError
                              Nothing -> return HM.empty
          mapOrError (ValMap m) = return m
          mapOrError _ = typeError $ "Expected map"

-- I tried to mimick the Java backend here, which seems to have more
-- well-defined semantics than the JS backend (the latter just leaves most of
-- the logic to the Javascript engine)
followPath :: [Expr] -> Maybe Value -> RenderM (Maybe Value)
followPath [] value = return value
followPath (e:es) (Just (ValList lst)) =
    do x <- evalExpr e
       case x of
          Just (ValInt i) -> followPath es $ atMay lst (fromInteger i)
          Just (ValString s) ->
             case readMay $ T.unpack s of
                Just i -> followPath es $ atMay lst i
                _ -> return Nothing
          _ -> typeError "List index: Expected integer or integer-like string"
followPath (e:es) (Just (ValMap hm)) =
    do key <- evalExprDefined e
       case key of
          ValInt i -> throwError $ TypeError "Cannot access a map item by index"
          _ -> followPath es $ HM.lookup (valToString key) hm
followPath _ _ = return Nothing

findLocal :: Identifier -> [(Identifier, LocalVar)] -> Maybe LocalVar
findLocal name locals = fmap snd $ find ((name==) . fst) locals

lookupVar :: Variable -> RenderM (Maybe Value)
lookupVar var =
    case var of
       InjectedVar loc -> asksConfig cfg_injected >>= lookupHM loc
       GlobalVar loc -> asksConfig cfg_globals >>= lookupHM loc
       LocalVar loc -> asksContext ctx_localStack >>= lookupLocal loc
    where lookupHM (Location root path) hm = followPath path $ HM.lookup root hm
          lookupLocal loc@(Location root path) locals =
             case findLocal root locals of
                Just var -> followPath path $ Just (var_value var)
                Nothing -> liftM call_variables getCurrentCall >>= lookupHM loc

valToString :: Value -> T.Text
valToString val =
    case val of
        ValNull -> "null"
        ValBool True -> "true"
        ValBool False -> "false"
        ValString s -> s
        ValInt i -> showT i
        ValFloat f -> showT f
        ValList lst -> showListLike "[" "]" $ map valToString lst
        ValMap hm -> showListLike "{" "}" $ map pairToStr (HM.toList hm)
    where pairToStr (k, v) = T.concat [k, ": ", valToString v]
          showListLike l r lst = T.concat [l, T.intercalate ", " lst, r]

valToBool :: Maybe Value -> Bool
valToBool val =
    case val of
        Nothing -> False
        Just ValNull -> False
        Just (ValBool b) -> b
        Just (ValString s) -> s /= T.empty
        Just (ValInt 0) -> False
        Just (ValFloat 0.0) -> False
        _ -> True

evalExpr :: Expr -> RenderM (Maybe Value)
evalExpr exp =
    case exp of
        ExprLiteral lit -> Just <$> evalLiteral lit
        ExprVar var -> lookupVar var
        ExprOp op -> evalOp op
        ExprFuncCall call -> Just <$> evalFuncCall call

evalFuncCall :: FuncCall -> RenderM Value
evalFuncCall (FuncCall name args) =
    do evaluatedArgs <- mapM evalExprDefined args
       functions <- asksConfig cfg_functions
       case HM.lookup name functions of
         Just (PureFunction f) -> either throwError return $ f evaluatedArgs
         Just (SpecialFunction f) -> f args
         Nothing -> throwError $ LookupError $ T.append "Function not found: " name

evalOp :: OpExpr -> RenderM (Maybe Value)
evalOp op =
    case op of
        OpNot exp -> Just . ValBool . not <$> evalBoolean exp
        OpNeg exp -> Just <$> (evalExprDefined exp >>= evalNeg)
        OpMul e1 e2 -> liftBinOp (evalArithmeticOp (*)) e1 e2
        OpDiv e1 e2 -> liftBinOp evalDiv e1 e2
        OpMod e1 e2 -> liftBinOp evalMod e1 e2
        OpPlus e1 e2 -> liftBinOp evalPlus e1 e2
        OpMinus e1 e2 -> liftBinOp (evalArithmeticOp (-)) e1 e2
        OpGreater e1 e2 -> liftBinOp (evalCmp (>)) e1 e2
        OpLess e1 e2 -> liftBinOp (evalCmp (<)) e1 e2
        OpGreaterEq e1 e2 -> liftBinOp (evalCmp (>=)) e1 e2
        OpLessEq e1 e2 -> liftBinOp (evalCmp (<=)) e1 e2
        OpEqual e1 e2 -> liftBinOp (evalEq (==)) e1 e2
        OpNotEqual e1 e2 -> liftBinOp (evalEq (/=)) e1 e2
        OpAnd e1 e2 -> Just . ValBool <$> allM (valsToBool [e1, e2])
        OpOr e1 e2 -> Just . ValBool <$> anyM (valsToBool [e1, e2])
        OpConditional cond true false ->
              evalBoolean cond >>= evalExpr . switch true false
    where evalEq cmp a b = return $ ValBool (a `cmp` b)
          liftBinOp f e1 e2 =
              do v1 <- evalExprDefined e1
                 v2 <- evalExprDefined e2
                 res <- f v1 v2
                 return $ Just res
          valsToBool = map evalBoolean

evalPlus :: (MonadError SoyError m) => Value -> Value -> m Value
evalPlus (ValString x) y = return $ ValString $ T.append x (valToString y)
evalPlus x (ValString y) = return $ ValString $ T.append (valToString x) y
evalPlus x y = evalArithmeticOp (+) x y

evalNeg :: (MonadError SoyError m) => Value -> m Value
evalNeg x = case x of
                ValInt i -> return $ ValInt $ (-i)
                ValFloat d -> return $ ValFloat $ (-d)
                _ -> typeError "Cannot negate a non-number"

evalCmpBool :: (MonadError SoyError m)
            => (forall a. Ord a => a -> a -> Bool)
            -> Value -> Value
            -> m Bool
evalCmpBool cmp (ValInt i) (ValInt j) = return $ cmp i j
evalCmpBool cmp (ValFloat i) (ValFloat j) = return $ cmp i j
evalCmpBool cmp (ValString i) (ValString j) = return $ cmp i j
evalCmpBool _ _ _ = typeError "Cannot compare values of this kind"

evalCmp :: (MonadError SoyError m) => (forall a. Ord a => a -> a -> Bool)
        -> Value -> Value -> m Value
evalCmp cmp a b = liftM ValBool $ evalCmpBool cmp a b

evalMod :: (MonadError SoyError m) => Value -> Value -> m Value
evalMod (ValInt i) (ValInt j) = return $ ValInt (i `mod` j)
evalMod _ _ = typeError "Cannot apply modulo to non-integers"

evalDiv :: (MonadError SoyError m) => Value -> Value -> m Value
evalDiv (ValInt i) (ValInt j) = return $ ValFloat $ (fromIntegral i) / (fromIntegral j)
evalDiv (ValFloat i) (ValFloat j) = return $ ValFloat (i / j)
evalDiv (ValFloat i) (ValInt j) = return $ ValFloat $ i / (fromIntegral j)
evalDiv (ValInt i) (ValFloat j) = return $ ValFloat $ (fromIntegral i) / j
evalDiv _ _ = typeError "Cannot divide non-numbers"

evalArithmeticOp :: (MonadError SoyError m)
                 => (forall a. Num a => a -> a -> a)
                 -> Value -> Value
                 -> m Value
evalArithmeticOp op (ValInt i) (ValInt j) = return $ ValInt (op i j)
evalArithmeticOp op (ValFloat i) (ValFloat j) = return $ ValFloat (op i j)
evalArithmeticOp op (ValFloat i) (ValInt j) = return $ ValFloat (op i (fromIntegral j))
evalArithmeticOp op (ValInt i) (ValFloat j) = return $ ValFloat (op (fromIntegral i) j)
evalArithmeticOp _ _ _ = typeError "Cannot do arithmetics on values of this kind"

evalExprDefined :: Expr -> RenderM Value
evalExprDefined exp = evalExpr exp >>= maybe err return
    where err = typeError "Encountered undefined while evaluating an expression"

evalLiteral :: Literal -> RenderM Value
evalLiteral lit = case lit of
                    LiteralNull -> return ValNull
                    LiteralString s -> return $ ValString s
                    LiteralInt i -> return $ ValInt i
                    LiteralFloat d -> return $ ValFloat d
                    LiteralBool b -> return $ ValBool b
                    LiteralList lst -> ValList <$> mapM evalExprDefined lst
                    LiteralMap m -> ValMap . HM.fromList <$> mapM mapper m
    where mapper (ke, ve) = do k <- evalStr ke
                               v <- evalExprDefined ve
                               return (k, v)

evalInt :: Expr -> RenderM Integer
evalInt exp =
    do val <- evalExprDefined exp
       case val of
          ValInt i -> return i
          _ -> typeError "Expected an integer"

evalStr :: Expr -> RenderM T.Text
evalStr = liftM valToString . evalExprDefined

evalBoolean :: Expr -> RenderM Bool
evalBoolean = liftM valToBool . evalExpr

typeError :: (MonadError SoyError m) => T.Text -> m a
typeError = throwError . TypeError

builtinFunctions =
    [ ("length", PureFunction func_length)
    , ("keys", PureFunction func_keys)
    , ("round", PureFunction func_round)
    , ("floor", PureFunction func_floor)
    , ("ceiling", PureFunction func_ceiling)
    , ("min", PureFunction func_min)
    , ("max", PureFunction func_max)
    , ("isItem", PureFunction func_isItem)
    , ("isFirst", SpecialFunction func_isFirst)
    , ("isLast", SpecialFunction func_isLast)
    , ("index", SpecialFunction func_index)
    , ("randomInt", SpecialFunction func_randomInt)
    , ("debug", SpecialFunction func_debug)
    , ("toJson", PureFunction func_toJson)
    ]

funcError :: (MonadError SoyError m) => T.Text -> m a
funcError f = typeError $
    T.append "Wrong argument count or type mismatch in function call to " f

debugCtx :: RenderM T.Text
debugCtx =
    do cs <- asksContext ctx_callStack
       ls <- asksContext ctx_localStack
       glob <- asksConfig cfg_globals
       inj <- asksConfig cfg_injected
       return $ T.concat [ showT cs, showT ls, showT glob, showT inj ]

func_debug :: [Expr] -> RenderM Value
func_debug [] = liftM ValString $ debugCtx
func_debug _ = funcError "debug()"

func_length :: [Value] -> Either SoyError Value
func_length [ValList lst] = return $ ValInt $ fromIntegral (length lst)
func_length [ValString s] = return $ ValInt $ fromIntegral (T.length s)
func_length _ = funcError "length(<list>)"

func_keys :: [Value] -> Either SoyError Value
func_keys [ValMap hm] = return $ ValList (map ValString (HM.keys hm))
func_keys _ = funcError "keys(<map>)"

func_round :: [Value] -> Either SoyError Value
func_round [ValFloat d] = return $ ValInt $ round d
func_round [x@(ValInt i)] = return x
func_round [ValFloat x, ValInt d] =
    return $ if d > 0 then ValFloat $ (fromIntegral $ round (x * (10^d))) / 10^d
                      else ValInt $ (round (x / (10^(-d)))) * 10^(-d)
func_round [ValInt x, ValInt d] =
    return $ ValInt $ if d >= 0 then x else (x `div` 10^(-d)) * 10^(-d)
func_round _ = funcError "round(<float>[, <int>])"

func_floor :: [Value] -> Either SoyError Value
func_floor [ValFloat d] = return $ ValInt $ floor d
func_floor [x@(ValInt i)] = return $ x
func_floor _ = funcError "floor(<number>)"

func_ceiling :: [Value] -> Either SoyError Value
func_ceiling [ValFloat d] = return $ ValInt $ ceiling d
func_ceiling [x@(ValInt i)] = return $ x
func_ceiling _ = funcError "ceiling(<number>)"

func_min :: [Value] -> Either SoyError Value
func_min [x, y] = evalCmpBool (<) x y >>= result
    where result b = return $ if b then x else y
func_min _ = funcError "min(<number>)"

func_max :: [Value] -> Either SoyError Value
func_max [x, y] = evalCmpBool (>) x y >>= result
    where result b = return $ if b then x else y
func_max _ = funcError "max(<number>)"

func_toJson :: [Value] -> Either SoyError Value
func_toJson [v] = return $ ValString $ jsonToStrictText v
    where jsonToStrictText = TL.toStrict . TLB.toLazyText . J.fromValue . J.toJSON
func_toJson _ = funcError "dump(<obj>)"

func_isItem :: [Value] -> Either SoyError Value
func_isItem [x, ValList lst] = return $ ValBool $ x `elem` lst
func_isItem _ = funcError "isItem(<obj>, <list>)"

func_randomInt :: [Expr] -> RenderM Value
func_randomInt [exp] =
    do upper <- evalExpr exp
       case upper of
          Just (ValInt i) ->
              do gen <- gets state_random
                 let (val, gen') = randomR (0, i - 1) gen
                 return $ ValInt val
          _ -> funcError "randomInt(<int>)"
func_randomInt _ = funcError "randomInt(<int>)"

getForeachVar :: Identifier -> RenderM (Int, Int)
getForeachVar var = asksContext ctx_localStack >>= findVar var
    where findVar var locals =
            case findLocal var locals of
                Just (ForeachVar _ index total) -> return $ (index, total)
                _ -> typeError $ T.concat ["$", var, " is not a foreach loop variable"]

func_index :: [Expr] -> RenderM Value
func_index [ExprVar (LocalVar (Location var []))] =
    liftM (ValInt . toInteger . fst) $ getForeachVar var
func_index _ = funcError "index($<foreach variable>)"

func_isFirst :: [Expr] -> RenderM Value
func_isFirst [ExprVar (LocalVar (Location var []))] =
    liftM (ValBool . (0==) . fst) $ getForeachVar var
func_isFirst _ = funcError "isFirst($<foreach variable>)"

func_isLast :: [Expr] -> RenderM Value
func_isLast [ExprVar (LocalVar (Location var []))] =
    liftM (\(i, size) -> ValBool $ i + 1 == size) $ getForeachVar var
func_isLast _ = funcError "isLast($<foreach variable>)"

-- helpers

switch :: a -> a -> Bool -> a
switch yes no cond = if cond then yes else no

showT :: Show a => a -> T.Text
showT = T.pack . show

allM :: (Monad m) => [m Bool] -> m Bool
allM [] = return True
allM (x:xs) = x >>= switch (allM xs) (return False)

anyM :: (Monad m) => [m Bool] -> m Bool
anyM [] = return False
anyM (x:xs) = x >>= switch (return True) (anyM xs)

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM f (x:xs) = f x >>= (\b -> if b then return $ Just x else findM f xs)
findM f [] = return Nothing

-- ==============================================================
--Testing
-- ==============================================================

litInt x = ExprLiteral (LiteralInt x)
litFloat x = ExprLiteral (LiteralFloat x)
litStr x = ExprLiteral (LiteralString x)
litBool x = ExprLiteral (LiteralBool x)
litList x  = ExprLiteral (LiteralList x)
litMap x  = ExprLiteral (LiteralMap x)
undefinedExpr = ExprVar (LocalVar (Location "notexisting" []))

printVar t var = CommandPrint (PrintCommand (ExprVar (t (Location var []))) [])

tests = htfMain htf_Soy_Render_thisModulesTests

testOk a b = assertEqual (Right b) a
testGen prep inout errin = mapM_ (\(x,y) -> testOk (prep x) y) inout
                        >> mapM_ (\x -> assertLeft (prep x)) errin

testRenderM cfg ctx f = testGen (\inp -> runRenderM cfg ctx $ f inp)

print_foo = Template "print_foo" [ContentText "foo"] False Nothing
print_bar = Template "print_bar" [ContentText "bar"] False Nothing
print_param1 = Template "print_param1"
        [ContentCommand (printVar LocalVar "param1")] False Nothing
print_param2 = Template "print_param2"
        [ContentCommand (printVar LocalVar "param2")] False (Just EscapeHtml)
print_param12 = Template "print_param12"
        [ ContentCommand (printVar LocalVar "param1")
        , ContentCommand (printVar LocalVar "param2")
        ]
        False (Just EscapeHtml)
print_baz = Template "print_baz" [ContentText "baz"] False (Just EscapeJs)
print_sub2 = Template "print_sub2" [ContentText "sub2"] False Nothing
print_all_foos = Template "print_foos"
                    [ ContentCommand (printVar LocalVar "foo")
                    , ContentCommand (printVar GlobalVar "foo")
                    , ContentCommand (printVar InjectedVar "foo")
                    ] False Nothing
ns = File (Namespace ["ns"] (Just NoEscape))
        [ print_foo
        , print_bar
        , print_param1
        , print_param2
        , print_param12
        , print_all_foos]
ns_sub = File (Namespace ["ns","sub"] Nothing)
        [ print_baz
        , print_sub2]

testConfig = defaultRenderConfig
    { cfg_files = [ns, ns_sub]
    }

testContext = RenderContext
    { ctx_currentCall = Call ns print_foo HM.empty
    , ctx_callStack = []
    , ctx_localStack = []
    }

test_render = testRenderM defaultRenderConfig testContext
        (\t -> renderTemplate HM.empty (File (Namespace ["ns"] (Just NoEscape)) [t]) t)
    [ (Template "foo" [ContentText "Hallo"] False Nothing, "Hallo" ) ]
    []

test_findTemplate = testRenderM testConfig testContext findTemplate
    [ (PathFull ["ns", "sub", "print_baz"], (ns_sub, print_baz))
    , (PathFull ["ns", "print_bar"], (ns, print_bar))
    , (PathRelative "print_bar", (ns, print_bar))
    , (PathRelative "print_foo", (ns, print_foo))
    ]
    [ PathFull ["ns", "print_baz"]
    , PathRelative "print_baz"
    ]

uncurry3 f (a,b,c) = f a b c

test_renderTemplate = testRenderM (testConfig
        { cfg_globals = HM.fromList [("foo", ValInt 2)]
        , cfg_injected = HM.fromList [("foo", ValInt 3)]
        })
        testContext
        (uncurry3 renderTemplate)
    [ ((HM.fromList [("foo", ValInt 1)], ns, print_all_foos), "123")
    ]
    []

test_getDefaultEscaping =
    do assertEqual (Right EscapeHtml) $ defEscapingFor print_sub2
       assertEqual (Right EscapeJs) $ defEscapingFor print_baz
    where defEscapingFor tmpl =
            runRenderM testConfig
                       testContext { ctx_currentCall = Call ns_sub tmpl HM.empty }
                       getDefaultEscaping

test_renderPrintCommand = testRenderM
        testConfig
        testContext { ctx_currentCall = Call ns_sub print_sub2 HM.empty }
        renderPrintCommand
    [ (printStr "&'\"<>" [], "&#x26;&#x27;&#x22;&#x3C;&#x3E;")
    , (printStr "&\"'foo\xff\x1234" [PrintEscape EscapeJs], "&\\x22\\x27foo\\xFF\\u1234")
    , (printStr "\"&" [PrintId], "\"&")
    , (printStr "foobar" [PrintTruncate 3 False], "foo")
    , (printStr "&&&" [PrintTruncate 6 True, PrintEscape EscapeHtml], "&#x26;&#x26;&#x26;")
    , (printStr "&&&" [PrintEscape EscapeHtml, PrintTruncate 6 True], "&#x...")
    , (printStr "a\na" [PrintEscape EscapeHtml, PrintChangeNewlineToBr], "a<br />a")
    , (printStr "fooba fooba" [PrintInsertWordBreaks 2, PrintEscape NoEscape],
                      "fo<wbr>ob<wbr>a fo<wbr>ob<wbr>a")
    ]
    []
    where printStr str dir = PrintCommand (litStr str) dir

test_renderIfCommand = testRenderM testConfig testContext renderIfCommand
    [ (IfCommand [ (litBool False, [ContentText "foo"])
                 , (litBool True, [ContentText "bar"])
                 ]
                 (Just [ContentText "baz"]),
            "bar")
    , (IfCommand [ (litBool True, [ContentText "foo"])
                 , (litBool False, [ContentText "bar"])
                 ]
                 (Just [ContentText "baz"]),
            "foo")
    , (IfCommand [ (litBool False, [ContentText "foo"])
                 , (litBool False, [ContentText "bar"])
                 ]
                 (Just [ContentText "baz"]),
            "baz")
    , (IfCommand [ (litBool False, [ContentText "foo"])
                 , (litBool False, [ContentCommand
                      (CommandPrint (PrintCommand
                          (ExprVar (LocalVar (Location "notexisting" []))) []))])
                 ]
                 (Just [ContentText "baz"]),
            "baz")
    , (IfCommand [ (litBool True, [ContentText "foo"])
                 , (ExprVar (LocalVar (Location "notexisting" [])), [ContentText "bar"])
                 ]
                 Nothing,
            "foo")
    ]
    []

test_renderSwitchCommand = testRenderM testConfig testContext renderSwitchCommand
    [ (SwitchCommand (litInt 2)
                     [ ([litInt 1, litStr "test"], [ContentText "foo"])
                     , ([litInt 3, litInt 2], [ContentText "bar"]) ]
                     Nothing,
            "bar")
    , (SwitchCommand (litInt 4)
                     [ ([litInt 1, litStr "test"], [ContentText "foo"])
                     , ([litInt 2, litInt 3], [ContentText "bar"]) ]
                     Nothing,
            "")
    , (SwitchCommand (litInt 4)
                     [ ([litInt 1, litStr "test"], [ContentText "foo"])
                     , ([litInt 2, litInt 3], [ContentText "bar"]) ]
                     (Just [ContentText "baz"]),
            "baz")
    ]
    []

test_renderForeachCommand = testRenderM testConfig testContext renderForeachCommand
    [ (ForeachCommand "iter"
                      (litList [ litInt 1, litStr "foo", litInt 3 ])
                      [ ContentCommand (printVar LocalVar "iter") ]
                      Nothing,
            "1foo3")
    , (ForeachCommand "iter"
                      (litList [ litInt 1, litStr "foo", litInt 3 ])
                      [ funcCall "isFirst" [ localVar "iter" ]
                      , funcCall "isLast" [ localVar "iter" ]
                      , funcCall "index" [ localVar "iter" ]
                      ]
                      Nothing,
            "truefalse0falsefalse1falsetrue2")
    , (ForeachCommand "iter"
                      (litList [])
                      [ContentText "foo"]
                      (Just [ContentText "bar"]),
            "bar")
    , (ForeachCommand "iter"
            (litList [ litInt 1, litInt 2, litInt 3 ])
            [ ContentCommand (CommandForeach (ForeachCommand "iter"
                (litList [ litInt 3, litInt 4 ])
                [ ContentCommand (printVar LocalVar "iter") ]
                Nothing))]
            Nothing,
                "343434")
    ]
    []
    where funcCall name args = ContentCommand (CommandPrint (PrintCommand (ExprFuncCall
                                    (FuncCall name args)) []))
          localVar var = ExprVar (LocalVar (Location var []))

test_renderForCommand = testRenderM testConfig testContext renderForCommand
    [ (ForCommand "iter" (litInt 0) (litInt 6) (litInt 2)
            [ ContentCommand (CommandPrint (PrintCommand
                (ExprVar (LocalVar (Location "iter" []))) []))
            ],
            "024")
    ]
    []

test_renderCallCommand = testRenderM
        testConfig
        testContext
        { ctx_currentCall = Call ns print_foo $ HM.fromList [("param1", ValInt 1)]
        }
        renderCallCommand
    [ (CallCommand (PathFull ["ns", "print_param1"]) Nothing
                  [("param1", ParamExpr (litStr "'"))],
            "'")
    , (CallCommand (PathFull ["ns", "print_param12"]) Nothing
                  [ ("param1", ParamTemplate
                       [ ContentCommand (CommandPrint (PrintCommand (litStr "test") [])) ])
                  , ("param2", ParamExpr (litInt 2))
                  ],
            "test2")
    , (CallCommand (PathFull ["ns", "print_param2"]) Nothing
                  [("param2", ParamExpr (litStr "'"))],
            "&#x27;")
    , (CallCommand (PathFull ["ns", "print_param1"]) (Just CallDataAll) [],
            "1")
    , (CallCommand (PathFull ["ns", "print_param1"])
               (Just (CallDataExpr (litMap [(litStr "param1", litInt 2)]))) [],
            "2")
    , (CallCommand (PathFull ["ns", "print_param12"]) (Just CallDataAll)
                  [("param2", ParamExpr (litInt 3))],
            "13")
    ]
    [ CallCommand (PathFull ["ns", "print_param1"]) (Just (CallDataExpr (litInt 1))) []
    ]

test_followPath = testRenderM testConfig testContext (uncurry followPath)
    [ (([litInt 0, litStr "0"],
        Just $ ValList [ValList [ValInt 1]]),
                      Just $ ValInt 1)
    , (([litStr "0"], Just $ ValList [ValInt 1]), Just $ ValInt 1)
    , (([litInt (-10)], Just $ ValList [ValInt 1]), Nothing)

    , (([litStr "a", litFloat 0, litList [], litMap []],
            Just $ valMap [("a",
                            valMap [("0.0",
                                     valMap [("[]",
                                              valMap [("{}",
                                                       ValInt 1)])])])]),
            Just $ ValInt 1)
    , (([litStr "test"], Just $ ValMap (HM.fromList [])), Nothing)

    , (([litStr "test"], Nothing), Nothing)
    , (([undefinedExpr], Nothing), Nothing)
    ]

    [ ([undefinedExpr], Just $ ValList [ValInt 1])
    , ([litFloat 0.0], Just $ ValList [ValInt 1])

    , ([undefinedExpr], Just $ valMap [("test", ValInt 1)])
    , ([litInt 1], Just $ valMap [("test", ValInt 1)])
    ]

    where valMap = ValMap . HM.fromList

test_lookupVar = testRenderM
        testConfig
        { cfg_globals = (HM.fromList
            [ ("global", ValInt 1) ])
        , cfg_injected = (HM.fromList
            [ ("ij", ValInt 2) ])
        }
        testContext
        { ctx_currentCall = Call ns print_foo (HM.fromList
                                [ ("foo", ValInt 6) ])
        , ctx_localStack =
            [ ("foo", SimpleVar (ValInt 3))
            , ("bar", ForeachVar (ValInt 4) 0 1)
            , ("bar", SimpleVar (ValInt 5))
            ]
        }
        lookupVar
    [ (GlobalVar (Location "global" []), Just $ ValInt 1)
    , (InjectedVar (Location "ij" []), Just $ ValInt 2)
    , (LocalVar (Location "foo" []), Just $ ValInt 3)
    , (LocalVar (Location "bar" []), Just $ ValInt 4)
    , (GlobalVar (Location "ij" []), Nothing)
    , (InjectedVar (Location "global" []), Nothing)
    , (LocalVar (Location "global" []), Nothing)
    ]
    []

test_evalLiteral = testRenderM testConfig testContext evalLiteral
    [ (LiteralList [ litInt 1, litInt 2 ],
            ValList [ ValInt 1, ValInt 2 ])
    , (LiteralMap [(litStr "test", litInt 1), (litStr "abc", litInt 2)],
            ValMap $ HM.fromList [("test", ValInt 1), ("abc", ValInt 2)])
    ]
    []

test_evalOp = testRenderM testConfig testContext evalOp
    [ (OpNot (litBool True), Just $ ValBool False)
    , (OpNot (litBool False), Just $ ValBool True)
    , (OpNeg (litInt (-10)), Just $ ValInt 10)
    , (OpNeg (litFloat 10.0), Just $ ValFloat (-10.0))
    , (OpMul (litFloat 2) (litFloat 3), Just $ ValFloat 6)
    , (OpMul (litInt 2) (litInt 3), Just $ ValInt 6)
    , (OpMul (litInt 2) (litFloat 3), Just $ ValFloat 6)
    , (OpMul (litFloat 2) (litInt 3), Just $ ValFloat 6)
    , (OpDiv (litFloat 10) (litFloat 5), Just $ ValFloat 2)
    , (OpDiv (litInt 10) (litInt 5), Just $ ValFloat 2)
    , (OpMod (litInt 10) (litInt 4), Just $ ValInt 2)
    , (OpPlus (litInt 10) (litInt 4), Just $ ValInt 14)
    , (OpPlus (litStr "test") (litInt 4), Just $ ValString "test4")
    , (OpMinus (litInt 10) (litInt 4), Just $ ValInt 6)
    , (OpGreater (litInt 10) (litInt 4), Just $ ValBool True)
    , (OpGreaterEq (litInt 10) (litInt 10), Just $ ValBool True)
    , (OpLess (litInt 10) (litInt 4), Just $ ValBool False)
    , (OpLess (litStr "a") (litStr "b"), Just $ ValBool True)
    , (OpLessEq (litStr "b") (litStr "a"), Just $ ValBool False)
    , (OpEqual (litInt 1) (litInt 1), Just $ ValBool True)
    , (OpNotEqual (litInt 1) (litInt 1), Just $ ValBool False)
    , (OpAnd (litBool True) (litBool False), Just $ ValBool False)
    , (OpAnd (litBool False) undefinedExpr, Just $ ValBool False)
    , (OpOr (litBool False) (litBool False), Just $ ValBool False)
    , (OpOr (litBool True) undefinedExpr, Just $ ValBool True)
    , (OpConditional (litBool True) (litInt 1) undefinedExpr, Just $ ValInt 1)
    , (OpConditional (litBool False) undefinedExpr (litInt 2), Just $ ValInt 2)
    ]
    []

test_round = testGen func_round
    [ ([ValFloat 1.6], ValInt 2)
    , ([ValInt 10], ValInt 10)
    , ([ValFloat 123.456, ValInt 0], ValInt 123)
    , ([ValFloat 123.456, ValInt 2], ValFloat 123.46)
    , ([ValFloat 123.456, ValInt (-2)], ValInt 100)
    , ([ValInt 12345, ValInt 0], ValInt 12345)
    , ([ValInt 12345, ValInt 2], ValInt 12345)
    , ([ValInt 12345, ValInt (-2)], ValInt 12300)
    ]
    []

test_evalFuncCall = testRenderM testConfig testContext evalFuncCall
    [ (FuncCall "length" [ litList [ litInt 1 ]], ValInt 1)
    , (FuncCall "keys" [ litMap [(litStr "test", litInt 1)] ],
            ValList [ ValString "test" ])
    , (FuncCall "round" [litFloat 4.6], ValInt 5)
    , (FuncCall "floor" [litFloat 4.2], ValInt 4)
    , (FuncCall "ceiling" [litFloat 4.2], ValInt 5)
    , (FuncCall "min" [litInt 1, litInt 2], ValInt 1)
    , (FuncCall "max" [litStr "a", litStr "b"], ValString "b")
    , (FuncCall "isItem" [litStr "a", litList [litStr "b", litStr "a"]], ValBool True)
    , (FuncCall "isItem" [litStr "c", litList [litStr "b", litStr "a"]], ValBool False)
    ]
    []
