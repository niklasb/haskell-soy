{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-incomplete-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Language.Soy.Render
    ( RenderConfig
    , render
    , configure
    , setup
    , setRandomSeed
    , setIOSeed
    , addFiles
    , addIjData
    , addGlobalData
    , addFunction
    ) where

import Language.Soy.Parser
import Language.Soy.Sanitization
import Language.Soy.Types

import qualified Data.Aeson as J
import qualified Data.Attoparsec.Number as J
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
import qualified Data.Vector as V
import Data.Vector ((!?))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Random

data Function
    = PureFunction ([J.Value] -> Either SoyError J.Value)
    | SpecialFunction ([Expr] -> RenderM J.Value)

data PrintDirectiveImpl
    = PrintDirectiveImpl
    { pdir_func :: ([J.Value] -> T.Text -> Either SoyError T.Text)
    , pdir_stopAutoescape :: Bool
    }

data Call
    = Call
    { call_file :: File
    , call_template :: Template
    , call_variables :: HM.HashMap Identifier J.Value
    } deriving (Eq, Show)

data LocalVar
    = ForeachVar { var_value :: J.Value
                 , foreach_index :: Int
                 , foreach_total :: Int }
    | SimpleVar { var_value :: J.Value }
    deriving (Eq, Show)

data RenderConfig
    = RenderConfig
    { cfg_files :: [File]
    , cfg_globals :: HM.HashMap Identifier J.Value
    , cfg_injected :: HM.HashMap Identifier J.Value
    , cfg_functions :: HM.HashMap Identifier Function
    , cfg_printDirectives :: HM.HashMap Identifier PrintDirectiveImpl
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
    RenderConfig [] HM.empty HM.empty
                 (HM.fromList builtinFunctions)
                 (HM.fromList builtinPrintDirectives)
                 defaultRenderState

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

addIjData :: (MonadState RenderConfig m) => [(Identifier, J.Value)] -> m ()
addIjData vars = modifyIj $ HM.union (HM.fromList vars)
    where modifyIj f = modify $ \s -> s { cfg_injected = f (cfg_injected s) }

addGlobalData :: (MonadState RenderConfig m) => [(Identifier, J.Value)] -> m ()
addGlobalData vars = modifyGlobals $ HM.union (HM.fromList vars)
    where modifyGlobals f = modify $ \s -> s { cfg_globals = f (cfg_globals s) }

addFunction :: (MonadState RenderConfig m)
            => Identifier
            -> ([J.Value] -> Either SoyError J.Value)
            -> m ()
addFunction name func = modifyFunctions $ HM.insert name (PureFunction func)
    where modifyFunctions f = modify (\s -> s { cfg_functions = f (cfg_functions s) })

addPrintDirective :: (MonadState RenderConfig m)
                  => Identifier
                  -> ([J.Value] -> T.Text -> Either SoyError T.Text)
                  -> Bool
                  -> m ()
addPrintDirective name func stopAutoescape =
        modifyPrintDirectives $ HM.insert name (PrintDirectiveImpl func stopAutoescape)
    where modifyPrintDirectives f =
              modify (\s -> s { cfg_printDirectives = f (cfg_printDirectives s) })

render :: (MonadError SoyError m)
       => [Identifier]
       -> [(Identifier, J.Value)]
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

withTemplate :: File -> Template -> HM.HashMap Identifier J.Value -> RenderM a -> RenderM a
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
    do curfile <- call_file <$> getCurrentCall
       cfg <- asksConfig id
       findTemplateAbs (filePath curfile ++ [path]) cfg

renderTemplate :: HM.HashMap Identifier J.Value -> File -> Template -> RenderM T.Text
renderTemplate vars file template =
    do withTemplate file template vars $ renderContents (tmpl_content template)

-- implement the line joining heuristics
stripContentGen :: (T.Text -> T.Text -> T.Text) -> T.Text -> T.Text
stripContentGen sep txt = T.concat segments
    where lines' = mapTailInit T.stripStart T.stripEnd $ T.splitOn "\n" txt
          lines'' = filter (not . T.null) lines'
          segments
            | null lines'' = []
            | otherwise = head lines'' : [ T.append (sep a b) b
                                          | (a, b) <- zip lines'' (tail lines'') ]

stripContentSmart = stripContentGen sep
    where sep a b = if T.last a == '>' || T.head b == '<' then "" else " "

stripContentDumb = stripContentGen (\_ _ -> " ")

renderContent :: Content -> RenderM T.Text
renderContent (ContentText t) = return $ stripContentSmart t
renderContent (ContentCommand c) = renderCommand c

renderContents conts = T.concat <$> mapM renderContent conts

renderCommand :: Command -> RenderM T.Text
renderCommand cmd =
    case cmd of
        CommandText t -> return t
        CommandPrint c -> renderPrintCommand c
        CommandIf c -> renderIfCommand c
        CommandForeach c -> renderForeachCommand c
        CommandFor c -> renderForCommand c
        CommandMsg c -> renderMsgCommand c
        CommandCss c -> renderCssCommand c
        CommandCall c -> renderCallCommand c
        CommandSwitch c -> renderSwitchCommand c

renderMsgCommand :: MsgCommand -> RenderM T.Text
renderMsgCommand = renderContents . msg_content

renderCssCommand :: CssCommand -> RenderM T.Text
renderCssCommand (CssCommand txt) = return $ stripContentDumb txt

defaultEscapeMode = EscapeHtml

getDefaultEscaping :: RenderM EscapeMode
getDefaultEscaping =
    do call <- getCurrentCall
       return $ fromMaybe defaultEscapeMode $ msum [tmplEsc call, fileEsc call]
    where fileEsc call = ns_escapeMode $ file_namespace $ call_file $ call
          tmplEsc call = tmpl_escapeMode $ call_template $ call

escape :: EscapeMode -> T.Text -> T.Text
escape mode =
    case mode of
        NoEscape -> id
        EscapeUri -> escapeUri
        EscapeHtml -> escapeHtml
        EscapeJs -> escapeJs

applyDirectives :: [PrintDirective] -> T.Text -> RenderM T.Text
applyDirectives dirs txt =
    do directivesWithArgs <- lookupDirectives
       txt' <- foldM applyDirective txt directivesWithArgs
       if any pdir_stopAutoescape $ map fst directivesWithArgs
            then return txt'
            else flip escape txt' <$> getDefaultEscaping
    where lookupDirectives =
            do dirDict <- asksConfig cfg_printDirectives
               mapM (lookupDirective dirDict) dirs
          lookupDirective dirDict (PrintDirective name args) =
            do evaluatedArgs <- mapM evalExprDefined args
               maybe (err name) (return . (,evaluatedArgs)) $ HM.lookup name dirDict
          err name = throwError $ PrintDirectiveLookupError
                                $ T.append "Print directive not found: " name
          applyDirective txt (PrintDirectiveImpl f _, args) =
              either throwError return $ f args txt

renderPrintCommand :: PrintCommand -> RenderM T.Text
renderPrintCommand (PrintCommand exp dirs) = evalStr exp >>= applyDirectives dirs

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
    where condition exps = elem <$> evalExpr exp <*> mapM evalExpr exps

renderForCommand :: ForCommand -> RenderM T.Text
renderForCommand (ForCommand iter fromExp toExp stepExp body) =
    do from <- evalInt fromExp
       to <- evalInt toExp
       step <- evalInt stepExp
       T.concat <$> mapM (iteration . J.Number . J.I) [from, from + step .. to - 1]
    where iteration item = withLocal iter (SimpleVar item) $ renderContents body

renderForeachCommand :: ForeachCommand -> RenderM T.Text
renderForeachCommand (ForeachCommand iter exp body ifempty) =
    do coll <- evalExprDefined exp
       case coll of
          J.Array ary -> if V.null ary then renderIfEmpty
                                       else renderNonEmpty $ V.toList ary
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
          evalParam (ParamTemplate conts) = Just . J.toJSON <$> renderContents conts
          mapper (name, arg) = (name,) <$> evalParam arg
          processParams lst = [ (k, v) | (k, Just v) <- lst ]
          evalAdditional = case calldata of
                              Just CallDataAll -> call_variables <$> getCurrentCall
                              Just (CallDataExpr exp) -> evalExprDefined exp >>= mapOrError
                              Nothing -> return HM.empty
          mapOrError (J.Object m) = return m
          mapOrError _ = typeError $ "Expected map"

-- I tried to mimick the Java backend here, which seems to have more
-- well-defined semantics than the JS backend (the latter just leaves most of
-- the logic to the Javascript engine)
followPath :: [Expr] -> Maybe J.Value -> RenderM (Maybe J.Value)
followPath [] value = return value
followPath (e:es) (Just (J.Array ary)) =
    do x <- evalExpr e
       case x of
          Just (J.Number (J.I i)) -> followPath es (ary !? fromInteger i)
          Just (J.String s) ->
             case readMay $ T.unpack s of
                Just i -> followPath es (ary !? i)
                _ -> return Nothing
          _ -> typeError "List index: Expected integer or integer-like string"
followPath (e:es) (Just (J.Object hm)) =
    do key <- evalExprDefined e
       case key of
          J.Number (J.I i) -> throwError $ TypeError "Cannot access a map item by index"
          _ -> followPath es $ HM.lookup (valToString key) hm
followPath _ _ = return Nothing

findLocal :: Identifier -> [(Identifier, LocalVar)] -> Maybe LocalVar
findLocal name locals = fmap snd $ find ((name==) . fst) locals

lookupVar :: Variable -> RenderM (Maybe J.Value)
lookupVar var =
    case var of
       InjectedVar loc -> asksConfig cfg_injected >>= lookupHM loc
       GlobalVar loc -> asksConfig cfg_globals >>= lookupHM loc
       LocalVar loc -> asksContext ctx_localStack >>= lookupLocal loc
    where lookupHM (Location root path) hm = followPath path $ HM.lookup root hm
          lookupLocal loc@(Location root path) locals =
             case findLocal root locals of
                Just var -> followPath path $ Just (var_value var)
                Nothing -> getCurrentCall >>= lookupHM loc . call_variables

valToString :: J.Value -> T.Text
valToString val =
    case val of
        J.Null -> "null"
        J.Bool True -> "true"
        J.Bool False -> "false"
        J.String s -> s
        J.Number n -> showT n
        J.Array ary -> showListLike "[" "]" $ map valToString $ V.toList ary
        J.Object hm -> showListLike "{" "}" $ map pairToStr $ HM.toList hm
    where pairToStr (k, v) = T.concat [k, ": ", valToString v]
          showListLike l r lst = T.concat [l, T.intercalate ", " lst, r]

valToBool :: Maybe J.Value -> Bool
valToBool val =
    case val of
        Nothing -> False
        Just J.Null -> False
        Just (J.Bool b) -> b
        Just (J.String s) -> not $ T.null s
        Just (J.Number (J.I 0)) -> False
        Just (J.Number (J.D 0.0)) -> False
        _ -> True

evalExpr :: Expr -> RenderM (Maybe J.Value)
evalExpr exp =
    case exp of
        ExprLiteral lit -> Just <$> evalLiteral lit
        ExprVar var -> lookupVar var
        ExprOp op -> evalOp op
        ExprFuncCall call -> Just <$> evalFuncCall call

evalFuncCall :: FuncCall -> RenderM J.Value
evalFuncCall (FuncCall name args) =
    do evaluatedArgs <- mapM evalExprDefined args
       functions <- asksConfig cfg_functions
       case HM.lookup name functions of
           Just (PureFunction f) -> either throwError return $ f evaluatedArgs
           Just (SpecialFunction f) -> f args
           Nothing -> throwError $ FunctionLookupError
                                 $ T.append "Function not found: " name

evalOp :: OpExpr -> RenderM (Maybe J.Value)
evalOp op =
    case op of
        OpNot exp -> Just . J.toJSON . not <$> evalBoolean exp
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
        OpAnd e1 e2 -> Just . J.toJSON <$> allM (valsToBool [e1, e2])
        OpOr e1 e2 -> Just . J.toJSON <$> anyM (valsToBool [e1, e2])
        OpConditional cond true false ->
              evalBoolean cond >>= evalExpr . switch true false
    where evalEq cmp a b = returnJSON (a `cmp` b)
          liftBinOp f e1 e2 =
              do v1 <- evalExprDefined e1
                 v2 <- evalExprDefined e2
                 res <- f v1 v2
                 return $ Just res
          valsToBool = map evalBoolean

evalPlus :: (MonadError SoyError m) => J.Value -> J.Value -> m J.Value
evalPlus (J.String x) y = returnJSON $ T.append x (valToString y)
evalPlus x (J.String y) = returnJSON $ T.append (valToString x) y
evalPlus x y = evalArithmeticOp (+) x y

evalNeg :: (MonadError SoyError m) => J.Value -> m J.Value
evalNeg x = case x of
                J.Number i -> returnJSON (-i)
                _ -> typeError "Cannot negate a non-number"

evalCmpBool :: (MonadError SoyError m)
            => (forall a. Ord a => a -> a -> Bool)
            -> J.Value -> J.Value
            -> m Bool
evalCmpBool cmp (J.Number x) (J.Number y) = return $ cmp x y
evalCmpBool cmp (J.String x) (J.String y) = return $ cmp x y
evalCmpBool _ _ _ = typeError "Cannot compare values of this kind"

evalCmp :: (MonadError SoyError m)
        => (forall a. Ord a => a -> a -> Bool)
        -> J.Value -> J.Value -> m J.Value
evalCmp cmp a b = liftM J.toJSON $ evalCmpBool cmp a b

evalMod :: (MonadError SoyError m) => J.Value -> J.Value -> m J.Value
evalMod (J.Number (J.I i)) (J.Number (J.I j)) = returnJSON $ i `mod` j
evalMod _ _ = typeError "Cannot apply modulo to non-integers"

evalDiv :: (MonadError SoyError m) => J.Value -> J.Value -> m J.Value
evalDiv (J.Number x) (J.Number y) = return $ J.Number $ x / y
evalDiv _ _ = typeError "Cannot divide non-numbers"

evalArithmeticOp :: (MonadError SoyError m)
                 => (J.Number -> J.Number -> J.Number)
                 -> J.Value -> J.Value
                 -> m J.Value
evalArithmeticOp op (J.Number x) (J.Number y) = returnJSON $ op x y
evalArithmeticOp _ _ _ = typeError "Cannot do arithmetics on values of this kind"

evalExprDefined :: Expr -> RenderM J.Value
evalExprDefined exp = evalExpr exp >>= maybe err return
    where err = typeError "Encountered undefined while evaluating an expression"

evalLiteral :: Literal -> RenderM J.Value
evalLiteral lit = case lit of
                    LiteralNull -> return J.Null
                    LiteralString s -> returnJSON s
                    LiteralNumber i -> returnJSON i
                    LiteralBool b -> returnJSON b
                    LiteralList lst -> J.toJSON . V.fromList <$> mapM evalExprDefined lst
                    LiteralMap m -> J.toJSON . HM.fromList <$> mapM mapper m
    where mapper (ke, ve) = do k <- evalStr ke
                               v <- evalExprDefined ve
                               return (k, v)

evalInt :: Expr -> RenderM Integer
evalInt exp =
    do val <- evalExprDefined exp
       case val of
          J.Number (J.I i) -> return i
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
    ]

funcError :: (MonadError SoyError m) => T.Text -> m a
funcError f = typeError $
    T.append "Wrong argument count or type mismatch in function call to " f

func_length :: [J.Value] -> Either SoyError J.Value
func_length [J.Array ary] = returnJSON $ V.length ary
func_length _ = funcError "length(<list>)"

func_keys :: [J.Value] -> Either SoyError J.Value
func_keys [J.Object hm] = returnJSON $ map J.toJSON $ HM.keys hm
func_keys _ = funcError "keys(<map>)"

func_round :: [J.Value] -> Either SoyError J.Value
func_round [J.Number x] = returnJSON $ toInteger $ round x
func_round [J.Number (J.D x), J.Number (J.I d)] =
    returnJSON $
        if d > 0 then J.D $ (fromIntegral $ round (x * (10^d))) / 10^d
                 else J.I $ (round (x / (10^(-d)))) * 10^(-d)
func_round [J.Number (J.I x), J.Number (J.I d)] =
    returnJSON $ if d >= 0 then x else (x `div` 10^(-d)) * 10^(-d)
func_round _ = funcError "round(<number>[, <int>])"

func_floor :: [J.Value] -> Either SoyError J.Value
func_floor [J.Number x] = returnJSON $ toInteger $ floor x
func_floor _ = funcError "floor(<number>)"

func_ceiling :: [J.Value] -> Either SoyError J.Value
func_ceiling [J.Number x] = returnJSON $ toInteger $ ceiling x
func_ceiling _ = funcError "ceiling(<number>)"

func_min :: [J.Value] -> Either SoyError J.Value
func_min [x, y] = evalCmpBool (<) x y >>= return . switch x y
func_min _ = funcError "min(<number>)"

func_max :: [J.Value] -> Either SoyError J.Value
func_max [x, y] = evalCmpBool (>) x y >>= return . switch x y
func_max _ = funcError "max(<number>)"

func_isItem :: [J.Value] -> Either SoyError J.Value
func_isItem [x, J.Array ary] = returnJSON $ x `V.elem` ary
func_isItem _ = funcError "isItem(<obj>, <list>)"

func_randomInt :: [Expr] -> RenderM J.Value
func_randomInt [exp] =
    do upper <- evalExprDefined exp
       case upper of
          J.Number (J.I i) ->
              do gen <- gets state_random
                 let (val, gen') = randomR (0, i - 1) gen
                 returnJSON val
          _ -> funcError "randomInt(<int>)"
func_randomInt _ = funcError "randomInt(<int>)"

getForeachVar :: Identifier -> RenderM (Int, Int)
getForeachVar var = asksContext ctx_localStack >>= findVar var
    where findVar var locals =
            case findLocal var locals of
                Just (ForeachVar _ index total) -> return $ (index, total)
                _ -> typeError $ T.concat ["$", var, " is not a foreach loop variable"]

func_index :: [Expr] -> RenderM J.Value
func_index [ExprVar (LocalVar (Location var []))] =
    J.toJSON . fst <$> getForeachVar var
func_index _ = funcError "index($<foreach variable>)"

func_isFirst :: [Expr] -> RenderM J.Value
func_isFirst [ExprVar (LocalVar (Location var []))] =
    J.toJSON . (0==) . fst <$> getForeachVar var
func_isFirst _ = funcError "isFirst($<foreach variable>)"

func_isLast :: [Expr] -> RenderM J.Value
func_isLast [ExprVar (LocalVar (Location var []))] =
    J.toJSON . isLast <$> getForeachVar var
    where isLast (i, size) = i + 1 == size
func_isLast _ = funcError "isLast($<foreach variable>)"

builtinPrintDirectives =
    [ ("noAutoescape", PrintDirectiveImpl pdir_noAutoescape True)
    , ("id", PrintDirectiveImpl pdir_noAutoescape True)
    , ("escapeUri", PrintDirectiveImpl pdir_escapeUri True)
    , ("escapeJs", PrintDirectiveImpl pdir_escapeJs True)
    , ("escapeHtml", PrintDirectiveImpl pdir_escapeHtml True)
    , ("insertWordBreaks", PrintDirectiveImpl pdir_insertWordBreaks False)
    , ("changeNewlineToBr", PrintDirectiveImpl pdir_changeNewlineToBr False)
    , ("truncate", PrintDirectiveImpl pdir_truncate False)
    ]

pdirError :: (MonadError SoyError m) => T.Text -> m a
pdirError p = typeError $
    T.append "Wrong argument count or type mismatch in usage of print directive " p

pdir_noAutoescape :: [J.Value] -> T.Text -> Either SoyError T.Text
pdir_noAutoescape [] = Right
pdir_noAutoescape _ = const $ pdirError "noAutoescape"

pdir_escapeUri :: [J.Value] -> T.Text -> Either SoyError T.Text
pdir_escapeUri [] = Right . escapeUri
pdir_escapeUri _ = const $ pdirError "escapeUri"

pdir_escapeJs :: [J.Value] -> T.Text -> Either SoyError T.Text
pdir_escapeJs [] = Right . escapeJs
pdir_escapeJs _ = const $ pdirError "escapeJs"

pdir_escapeHtml :: [J.Value] -> T.Text -> Either SoyError T.Text
pdir_escapeHtml [] = Right . escapeHtml
pdir_escapeHtml _ = const $ pdirError "escapeHtml"

insertWordBreaks :: Int -> T.Text -> T.Text
insertWordBreaks thresh str = T.unwords $ map processWord $ T.words str
    where processWord w = T.intercalate "<wbr>" $ T.chunksOf thresh w

pdir_insertWordBreaks :: [J.Value] -> T.Text -> Either SoyError T.Text
pdir_insertWordBreaks [(J.Number (J.I thresh))] =
    Right . insertWordBreaks (fromInteger thresh)
pdir_insertWordBreaks _ = const $ pdirError "insertWordBreaks:<int>"

pdir_changeNewlineToBr :: [J.Value] -> T.Text -> Either SoyError T.Text
pdir_changeNewlineToBr [] = Right . T.replace "\n" "<br />"
pdir_changeNewlineToBr _ = const $ pdirError "changeNewlineToBr"

truncateEllipsis :: Int -> T.Text -> T.Text
truncateEllipsis size str =
    if T.length str > size then T.concat [T.take (size - 3) str, "..."]
                           else str
pdir_truncate :: [J.Value] -> T.Text -> Either SoyError T.Text
pdir_truncate [(J.Number (J.I size))] = Right . T.take (fromInteger size)
pdir_truncate [(J.Number (J.I size)), (J.Bool ellipsis)] =
    Right . (if ellipsis then truncateEllipsis else T.take) (fromInteger size)
pdir_truncate _ = const $ pdirError "truncate:<int>[,<bool>]"

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

-- apply a transformation to the tail of the list and another one to the head of the
-- list in a single iteration
mapTailInit :: (a -> a) -> (a -> a) -> [a] -> [a]
mapTailInit fTail fInit lst =
    case lst of
        [] -> []
        [x] -> [x]
        x:xs -> fInit x : mapTailInit id fInit (map fTail xs)

returnJSON :: (J.ToJSON a, Monad m) => a -> m J.Value
returnJSON = return . J.toJSON

-- ==============================================================
--Testing
-- ==============================================================

valInt = J.Number . J.I
valFloat = J.Number . J.D
valMap = J.Object . HM.fromList

exprInt x = ExprLiteral (LiteralNumber $ J.I x)
exprFloat x = ExprLiteral (LiteralNumber $ J.D x)
exprStr x = ExprLiteral (LiteralString x)
exprBool x = ExprLiteral (LiteralBool x)
exprList x  = ExprLiteral (LiteralList x)
exprMap x  = ExprLiteral (LiteralMap x)
undefinedExpr = ExprVar (LocalVar (Location "notexisting" []))

printVar t var = CommandPrint (PrintCommand (ExprVar (t (Location var []))) [])

tests = htfMain htf_Language_Soy_Render_thisModulesTests

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

test_stripContentSmart =
    do assertEqual " a>b c \t<d<e " (stripContentSmart " a> \n \n b \n c \t<d \n <e ")
       assertEqual " a b "          (stripContentSmart " a \n b ")
       assertEqual " a "            (stripContentSmart " a ")
       assertEqual "a "             (stripContentSmart "\n a ")
       assertEqual "  "             (stripContentSmart "  ")
       assertEqual ""               (stripContentSmart "")

test_renderContents = testRenderM testConfig testContext renderContents
    [ ([ContentText "\n a ", ContentCommand (CommandText ""), ContentText " b \n"],
            "a  b")
    , ([ContentCommand (CommandText ""), ContentText "  ", ContentCommand (CommandText "")],
            "  ")
    , ([ ContentCommand (CommandText "")
       , ContentText " a> "
       , ContentCommand (CommandText "")
       , ContentText "\n b"
       ],
            " a> b")
    , ([ ContentText "a\n"
       , ContentCommand (CommandIf (IfCommand [(exprBool True, [ContentText "\n  b\n"])] Nothing))
       , ContentText "\nc"
       ],
            "abc")
    , ([ ContentText "\n  a\n  "
       , ContentCommand (CommandIf (IfCommand [(exprBool True, [ContentText " b\n  \n  "])] Nothing))
       , ContentText "\n  c"
       ],
            "a bc")
    , ([ ContentCommand (CommandIf (IfCommand [(exprBool True, [ContentText "  a\n"])] Nothing))
       , ContentText "\nb"
       ],
            "  ab")
    , ([ ContentCommand (CommandIf (IfCommand [(exprBool True, [ContentText "  a\n"])] Nothing))
       , ContentCommand (CommandText " ")
       , ContentText "\nb"
       ],
            "  a b")
    ]
    []

test_render = testRenderM testConfig testContext
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
        { cfg_globals = HM.fromList [("foo", valInt 2)]
        , cfg_injected = HM.fromList [("foo", valInt 3)]
        })
        testContext
        (uncurry3 renderTemplate)
    [ ((HM.fromList [("foo", valInt 1)], ns, print_all_foos), "123")
    ]
    []

test_getDefaultEscaping =
    do assertEqual (Right EscapeHtml) $ defEscapingFor print_sub2
       assertEqual (Right EscapeJs) $ defEscapingFor print_baz
    where defEscapingFor tmpl =
            runRenderM testConfig
                       testContext { ctx_currentCall = Call ns_sub tmpl HM.empty }
                       getDefaultEscaping

test_renderCssCommand = testRenderM testConfig testContext renderCssCommand
    [ (CssCommand "a  a \n  \n  \n b  b", "a  a b  b") ]
    []

test_renderPrintCommand = testRenderM
        testConfig
        testContext { ctx_currentCall = Call ns_sub print_sub2 HM.empty }
        renderPrintCommand
    [ (printStr "&'\"<>" [], "&#x26;&#x27;&#x22;&#x3C;&#x3E;")
    , (printStr "&\"'foo\xff\x1234" [PrintDirective "escapeJs" []], "&\\x22\\x27foo\\xFF\\u1234")
    , (printStr "\"&" [PrintDirective "id" []], "\"&")
    , (printStr "foobar" [PrintDirective "truncate" [exprInt 3]], "foo")
    , (printStr "&&&"
            [ PrintDirective "truncate" [exprInt 6, exprBool True]
            , PrintDirective "escapeHtml" []
            ],
        "&#x26;&#x26;&#x26;")
    , (printStr "&&&"
            [ PrintDirective "escapeHtml" []
            , PrintDirective "truncate" [exprInt 6, exprBool True]
            ],
        "&#x...")
    , (printStr "a\na"
            [ PrintDirective "escapeHtml" []
            , PrintDirective "changeNewlineToBr" []
            ],
        "a<br />a")
    , (printStr "fooba fooba"
            [ PrintDirective "insertWordBreaks" [exprInt 2]
            , PrintDirective "noAutoescape" []
            ],
        "fo<wbr>ob<wbr>a fo<wbr>ob<wbr>a")
    ]
    []
    where printStr str dir = PrintCommand (exprStr str) dir

test_renderIfCommand = testRenderM testConfig testContext renderIfCommand
    [ (IfCommand [ (exprBool False, [ContentText "foo"])
                 , (exprBool True, [ContentText "bar"])
                 ]
                 (Just [ContentText "baz"]),
            "bar")
    , (IfCommand [ (exprBool True, [ContentText "foo"])
                 , (exprBool False, [ContentText "bar"])
                 ]
                 (Just [ContentText "baz"]),
            "foo")
    , (IfCommand [ (exprBool False, [ContentText "foo"])
                 , (exprBool False, [ContentText "bar"])
                 ]
                 (Just [ContentText "baz"]),
            "baz")
    , (IfCommand [ (exprBool False, [ContentText "foo"])
                 , (exprBool False, [ContentCommand
                      (CommandPrint (PrintCommand
                          (ExprVar (LocalVar (Location "notexisting" []))) []))])
                 ]
                 (Just [ContentText "baz"]),
            "baz")
    , (IfCommand [ (exprBool True, [ContentText "foo"])
                 , (ExprVar (LocalVar (Location "notexisting" [])), [ContentText "bar"])
                 ]
                 Nothing,
            "foo")
    ]
    []

test_renderSwitchCommand = testRenderM testConfig testContext renderSwitchCommand
    [ (SwitchCommand (exprInt 2)
                     [ ([exprInt 1, exprStr "test"], [ContentText "foo"])
                     , ([exprInt 3, exprInt 2], [ContentText "bar"]) ]
                     Nothing,
            "bar")
    , (SwitchCommand (exprInt 4)
                     [ ([exprInt 1, exprStr "test"], [ContentText "foo"])
                     , ([exprInt 2, exprInt 3], [ContentText "bar"]) ]
                     Nothing,
            "")
    , (SwitchCommand (exprInt 4)
                     [ ([exprInt 1, exprStr "test"], [ContentText "foo"])
                     , ([exprInt 2, exprInt 3], [ContentText "bar"]) ]
                     (Just [ContentText "baz"]),
            "baz")
    ]
    []

test_renderForeachCommand = testRenderM testConfig testContext renderForeachCommand
    [ (ForeachCommand "iter"
                      (exprList [ exprInt 1, exprStr "foo", exprInt 3 ])
                      [ ContentCommand (printVar LocalVar "iter") ]
                      Nothing,
            "1foo3")
    , (ForeachCommand "iter"
                      (exprList [ exprInt 1, exprStr "foo", exprInt 3 ])
                      [ funcCall "isFirst" [ localVar "iter" ]
                      , funcCall "isLast" [ localVar "iter" ]
                      , funcCall "index" [ localVar "iter" ]
                      ]
                      Nothing,
            "truefalse0falsefalse1falsetrue2")
    , (ForeachCommand "iter"
                      (exprList [])
                      [ContentText "foo"]
                      (Just [ContentText "bar"]),
            "bar")
    , (ForeachCommand "iter"
            (exprList [ exprInt 1, exprInt 2, exprInt 3 ])
            [ ContentCommand (CommandForeach (ForeachCommand "iter"
                (exprList [ exprInt 3, exprInt 4 ])
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
    [ (ForCommand "iter" (exprInt 0) (exprInt 6) (exprInt 2)
            [ ContentCommand (CommandPrint (PrintCommand
                (ExprVar (LocalVar (Location "iter" []))) []))
            ],
            "024")
    ]
    []

test_renderCallCommand = testRenderM
        testConfig
        testContext
        { ctx_currentCall = Call ns print_foo $ HM.fromList [("param1", valInt 1)]
        }
        renderCallCommand
    [ (CallCommand (PathFull ["ns", "print_param1"]) Nothing
                  [("param1", ParamExpr (exprStr "'"))],
            "'")
    , (CallCommand (PathFull ["ns", "print_param12"]) Nothing
                  [ ("param1", ParamTemplate
                       [ ContentCommand (CommandPrint (PrintCommand (exprStr "test") [])) ])
                  , ("param2", ParamExpr (exprInt 2))
                  ],
            "test2")
    , (CallCommand (PathFull ["ns", "print_param2"]) Nothing
                  [("param2", ParamExpr (exprStr "'"))],
            "&#x27;")
    , (CallCommand (PathFull ["ns", "print_param1"]) (Just CallDataAll) [],
            "1")
    , (CallCommand (PathFull ["ns", "print_param1"])
               (Just (CallDataExpr (exprMap [(exprStr "param1", exprInt 2)]))) [],
            "2")
    , (CallCommand (PathFull ["ns", "print_param12"]) (Just CallDataAll)
                  [("param2", ParamExpr (exprInt 3))],
            "13")
    ]
    [ CallCommand (PathFull ["ns", "print_param1"]) (Just (CallDataExpr (exprInt 1))) []
    ]

test_followPath = testRenderM testConfig testContext (uncurry followPath)
    [ (([exprInt 0, exprStr "0"],
        Just $ J.toJSON [J.toJSON [valInt 1]]),
                      Just $ valInt 1)
    , (([exprStr "0"], Just $ J.toJSON [valInt 1]), Just $ valInt 1)
    , (([exprInt (-10)], Just $ J.toJSON [valInt 1]), Nothing)

    , (([exprStr "a", exprFloat 0, exprList [], exprMap []],
            Just $ valMap [("a",
                            valMap [("0.0",
                                     valMap [("[]",
                                              valMap [("{}",
                                                       valInt 1)])])])]),
            Just $ valInt 1)
    , (([exprStr "test"], Just $ J.Object HM.empty), Nothing)

    , (([exprStr "test"], Nothing), Nothing)
    , (([undefinedExpr], Nothing), Nothing)
    ]

    [ ([undefinedExpr], Just $ J.toJSON [valInt 1])
    , ([exprFloat 0.0], Just $ J.toJSON [valInt 1])

    , ([undefinedExpr], Just $ valMap [("test", valInt 1)])
    , ([exprInt 1], Just $ valMap [("test", valInt 1)])
    ]

test_lookupVar = testRenderM
        testConfig
        { cfg_globals = (HM.fromList
            [ ("global", valInt 1) ])
        , cfg_injected = (HM.fromList
            [ ("ij", valInt 2) ])
        }
        testContext
        { ctx_currentCall = Call ns print_foo (HM.fromList
                                [ ("foo", valInt 6) ])
        , ctx_localStack =
            [ ("foo", SimpleVar (valInt 3))
            , ("bar", ForeachVar (valInt 4) 0 1)
            , ("bar", SimpleVar (valInt 5))
            ]
        }
        lookupVar
    [ (GlobalVar (Location "global" []), Just $ valInt 1)
    , (InjectedVar (Location "ij" []), Just $ valInt 2)
    , (LocalVar (Location "foo" []), Just $ valInt 3)
    , (LocalVar (Location "bar" []), Just $ valInt 4)
    , (GlobalVar (Location "ij" []), Nothing)
    , (InjectedVar (Location "global" []), Nothing)
    , (LocalVar (Location "global" []), Nothing)
    ]
    []

test_valToString =
    do assertEqual (valToString (valMap [])) "{}"
       assertEqual (valToString (valFloat (0.0/0.0))) "NaN"
       assertEqual (valToString (valFloat (1.0/0.0))) "Infinity"
       assertEqual (valToString (valFloat ((-1.0)/0.0))) "-Infinity"

test_evalLiteral = testRenderM testConfig testContext evalLiteral
    [ (LiteralList [ exprInt 1, exprInt 2 ],
            J.toJSON [ valInt 1, valInt 2 ])
    , (LiteralMap [(exprStr "test", exprInt 1), (exprStr "abc", exprInt 2)],
            valMap [("test", valInt 1), ("abc", valInt 2)])
    ]
    []

test_evalOp = testRenderM testConfig testContext evalOp
    [ (OpNot (exprBool True), Just $ J.toJSON False)
    , (OpNot (exprBool False), Just $ J.toJSON True)

    , (OpNeg (exprInt (-10)), Just $ valInt 10)
    , (OpNeg (exprFloat 10.0), Just $ valFloat (-10.0))

    , (OpMul (exprFloat 2) (exprFloat 3), Just $ valFloat 6)
    , (OpMul (exprInt 2) (exprInt 3), Just $ valInt 6)
    , (OpMul (exprInt 2) (exprFloat 3), Just $ valFloat 6)
    , (OpMul (exprFloat 2) (exprInt 3), Just $ valFloat 6)

    , (OpDiv (exprFloat 10) (exprFloat 5), Just $ valFloat 2)
    , (OpDiv (exprInt 10) (exprInt 5), Just $ valFloat 2)
    , (OpDiv (exprInt 3) (exprFloat 2), Just $ valFloat 1.5)

    , (OpMod (exprInt 10) (exprInt 4), Just $ valInt 2)

    , (OpPlus (exprInt 10) (exprInt 4), Just $ valInt 14)
    , (OpPlus (exprStr "test") (exprInt 4), Just $ "test4")

    , (OpMinus (exprInt 10) (exprInt 4), Just $ valInt 6)

    , (OpGreater (exprInt 10) (exprInt 4), Just $ J.toJSON True)
    , (OpGreater (exprFloat 0.9) (exprInt 1), Just $ J.toJSON False)

    , (OpGreaterEq (exprInt 10) (exprInt 10), Just $ J.toJSON True)

    , (OpLess (exprInt 10) (exprInt 4), Just $ J.toJSON False)
    , (OpLess (exprStr "a") (exprStr "b"), Just $ J.toJSON True)

    , (OpLessEq (exprStr "b") (exprStr "a"), Just $ J.toJSON False)

    , (OpEqual (exprInt 1) (exprInt 2), Just $ J.toJSON False)
    , (OpEqual (exprFloat 1.0) (exprInt 1), Just $ J.toJSON True)
    , (OpEqual (exprStr "foo") (exprStr "foo"), Just $ J.toJSON True)

    , (OpNotEqual (exprInt 1) (exprInt 1), Just $ J.toJSON False)

    , (OpAnd (exprBool True) (exprBool False), Just $ J.toJSON False)
    , (OpAnd (exprBool False) undefinedExpr, Just $ J.toJSON False)

    , (OpOr (exprBool False) (exprBool False), Just $ J.toJSON False)
    , (OpOr (exprBool True) undefinedExpr, Just $ J.toJSON True)

    , (OpConditional (exprBool True) (exprInt 1) undefinedExpr, Just $ valInt 1)
    , (OpConditional (exprBool False) undefinedExpr (exprInt 2), Just $ valInt 2)
    ]
    [ OpMod (exprFloat 1) (exprInt 1)
    ]

test_round = testGen func_round
    [ ([valFloat 1.6], valInt 2)
    , ([valInt 10], valInt 10)
    , ([valFloat 123.456, valInt 0], valInt 123)
    , ([valFloat 123.456, valInt 2], valFloat 123.46)
    , ([valFloat 123.456, valInt (-2)], valInt 100)
    , ([valInt 12345, valInt 0], valInt 12345)
    , ([valInt 12345, valInt 2], valInt 12345)
    , ([valInt 12345, valInt (-2)], valInt 12300)
    ]
    []

test_evalFuncCall = testRenderM testConfig testContext evalFuncCall
    [ (FuncCall "length" [ exprList [ exprInt 1 ]], valInt 1)
    , (FuncCall "keys" [ exprMap [(exprStr "test", exprInt 1)] ],
            J.toJSON [ J.String "test" ])
    , (FuncCall "round" [exprFloat 4.6], valInt 5)
    , (FuncCall "floor" [exprFloat 4.2], valInt 4)
    , (FuncCall "ceiling" [exprFloat 4.2], valInt 5)
    , (FuncCall "min" [exprInt 1, exprInt 2], valInt 1)
    , (FuncCall "max" [exprStr "a", exprStr "b"], "b")
    , (FuncCall "isItem" [exprStr "a", exprList [exprStr "b", exprStr "a"]], J.toJSON True)
    , (FuncCall "isItem" [exprStr "c", exprList [exprStr "b", exprStr "a"]], J.toJSON False)
    ]
    []
