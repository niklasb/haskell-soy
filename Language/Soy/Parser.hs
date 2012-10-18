{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Language.Soy.Parser
    ( parseSoyFile
    ) where

import Language.Soy.Types
import qualified Data.Attoparsec.Expr as E

import Data.Attoparsec.Text
import Test.Framework
import qualified Data.Text as T

import Prelude hiding (takeWhile)
import Control.Applicative
import Control.Monad
import Data.Char (isSpace)
import Safe
import Data.Maybe
import Data.Monoid
import Numeric (readHex)

parseSoyFile :: Monad m => T.Text -> m File
parseSoyFile txt = either fail return $ parseOnly file txt

file :: Parser File
file = File <$> (optTopLevel_ *> namespace)
            <*> (many1 (optTopLevel_ *> template) <* optTopLevel_ <* endOfInput)
    where optTopLevel_ = many (spaceAndComments_ <|> topLevelText_)
          topLevelText_ = () <$ takeWhile1 (/='{')

simpleEscapeMode :: Parser EscapeMode
simpleEscapeMode = NoEscape <$ string "false"
               <|> EscapeHtml <$ string "true"

namespace :: Parser Namespace
namespace = openTag "namespace" header
    where header = Namespace <$> fullPath <*> escapeMode
          escapeMode = headMay <$> attributes attr
          attr = [ ("autoescape", simpleEscapeMode) ]

openTag :: T.Text -> Parser a -> Parser a
openTag name body = between (string "{{") (string "}}") inner
                <|> between (char '{') (char '}') inner
    where inner = name .*> space_ *> body <* optSpace_

openCloseTag :: T.Text -> Parser a -> Parser a
openCloseTag name body = between (char '{') (string "/}")
                                 (name .*> space_ *> body <* optSpace_)

closeTag_ :: T.Text -> Parser ()
closeTag_ name = () <$ between (string "{/") (char '}') (string name)

template :: Parser Template
template =
    do base <- openTag "template" header
       inner <- contents []
       closeTag_ "template"
       return $ base { tmpl_content = inner }
    where header = do name <- (char '.' *> identifier)
                      modifier <- liftM (appEndo . mconcat) $ attributes
                          [ ("autoescape", setEscapeMode <$> simpleEscapeMode)
                          , ("private", setPrivate <$> bool)
                          ]
                      return $ modifier $ Template name [] False Nothing
          setEscapeMode e = Endo $ \n -> n { tmpl_escapeMode = Just e }
          setPrivate p = Endo $ \n -> n { tmpl_private = p }

contents :: [T.Text] -> Parser [Content]
contents excludeTags = many content
    where content = ContentText <$> contentText
                <|> ContentCommand <$> command excludeTags

contentText = T.append <$> (lineComment_ *> lineEnding) <*> contentText
          <|> lineComment_ *> option "" lineEnding
          <|> T.concat <$> many1 part
    where part = T.append <$> (T.singleton <$> (satisfy isHorizontalSpace <* lineComment_))
                          <*> lineEnding
             <|> multiLineComment_ *> lineComment_ *> lineEnding
             <|> "" <$ multiLineComment_
             <|> T.singleton <$> notChar '{'

command :: [T.Text] -> Parser Command
command excludeTags = CommandText "" <$ string "{nil}"
      <|> CommandText " " <$ string "{sp}"
      <|> CommandText "\r" <$ string "{\\r}"
      <|> CommandText "\n" <$ string "{\\n}"
      <|> CommandText "\t" <$ string "{\\t}"
      <|> CommandText "{" <$ string "{lb}"
      <|> CommandText "}" <$ string "{rb}"
      <|> CommandText <$> literalCommand
      <|> CommandForeach <$> foreachCommand
      <|> CommandFor <$> forCommand
      <|> CommandIf <$> ifCommand
      <|> CommandSwitch <$> switchCommand
      <|> CommandCall <$> callCommand
      <|> CommandPrint <$> printCommand excludeTags

literalCommand :: Parser T.Text
literalCommand = "{literal}" .*>
                     (T.pack <$> manyTill anyChar (string "{/literal}"))

exclude :: [Parser a] -> Parser b -> Parser b
exclude excl p = inner >>= maybe (fail "") return
    where inner = Nothing <$ choice excl
              <|> Just <$> p

printCommand :: [T.Text] -> Parser PrintCommand
printCommand excludeTags = uncurry PrintCommand <$> (explicit <|> implicit)
    where implicit =  bracedC '{' '}' $ exclude (map string excludeTags)
                                                (optSpace_ *> printExpr <* optSpace_)
          explicit = openTag "print" printExpr
          printExpr = (,) <$> expr <*> (optSpace_ *> directives)
          directives = printDirective `sepBy` optSpace_

printEscapeMode :: Parser EscapeMode
printEscapeMode = NoEscape <$ string "noAutoescape"
              <|> EscapeHtml <$ string "escapeHtml"
              <|> EscapeJs <$ string "escapeJs"
              <|> EscapeUri <$ string "escapeUri"

printDirective :: Parser PrintDirective
printDirective = char '|' *> directive
    where directive = PrintId <$ string "id"
                  <|> PrintEscape <$> printEscapeMode
                  <|> PrintChangeNewlineToBr <$ string "changeNewlineToBr"
                  <|> PrintInsertWordBreaks <$>
                           (string "insertWordBreaks:" *> optSpace_ *> decimal)
                  <|> truncateDirective
          truncateDirective = PrintTruncate
              <$> (string "truncate:" *> optSpace_ *> decimal)
              <*> (option True
                          (False <$ (betweenSpace (char ',') *> string "false")))

foreachCommand :: Parser ForeachCommand
foreachCommand =
    do (iter, coll) <- openTag "foreach" commandLine
       body <- contents ["ifempty"]
       ifempty <- option Nothing (Just <$> ("{ifempty}" .*> contents []))
       closeTag_ "foreach"
       return $ ForeachCommand iter coll body ifempty
    where commandLine = (,) <$> (char '$' *> identifier)
                            <*> (betweenSpace1 (string "in") *> expr)

forCommand :: Parser ForCommand
forCommand =
    do (iter, from, to, step) <- openTag "for" header
       body <- contents []
       closeTag_ "for"
       return $ ForCommand iter from to step body
    where header = do iter <- (char '$' *> identifier)
                      betweenSpace1 (string "in")
                      func <- funcCall
                      switchFunc func iter
          switchFunc f iter = case f of
             FuncCall "range" [ to ]
               -> return (iter, exprInt 0, to, exprInt 1)
             FuncCall "range" [ from, to ]
               -> return (iter, from, to, exprInt 1)
             FuncCall "range" [ from, to, step ]
               -> return (iter, from, to, step)
             _ -> fail "invalid `for` command"

ifCommand :: Parser IfCommand
ifCommand = IfCommand <$> branches <*> optional ("{else}" .*> restrContents)
                      <* closeTag_ "if"
    where branches = (:) <$> branch "if" <*> many (branch "elseif")
          branch tag = (,) <$> openTag tag expr <*> restrContents
          restrContents = contents ["else", "elseif"]

switchCommand :: Parser SwitchCommand
switchCommand = SwitchCommand <$> openTag "switch" expr
                              <*> (optSpaceAndComments_ *> many exprCase)
                              <*> optional defaultCase
                              <*  closeTag_ "switch"
    where exprCase = (,) <$> openTag "case" exprList
                         <*> restrContents
          defaultCase = "{default}" .*> restrContents
          exprList = listLike nothing nothing (char ',') expr
          restrContents = contents ["case", "default"]

callCommand :: Parser CallCommand
callCommand = inlineCallCommand <|> parameterizedCallCommand

callHeader :: Parser (TemplatePath, Maybe CallData)
callHeader = (,) <$> templatePath <*> attributeList
    where attributeList =
              (maybe Nothing Just . headMay) <$> attributes validAttr
          validAttr = [ ("data", dataValue) ]
          dataValue = CallDataAll <$ string "all"
                  <|> CallDataExpr <$> expr

inlineCallCommand :: Parser CallCommand
inlineCallCommand =
    do (target, calldat) <- openCloseTag "call" callHeader
       return $ CallCommand target calldat []

parameterizedCallCommand :: Parser CallCommand
parameterizedCallCommand =
    do (target, calldat) <- openTag "call" callHeader
       params <- many (optSpaceAndComments_ *> callParam) <* optSpaceAndComments_
       closeTag_ "call"
       return $ CallCommand target calldat params

callParam :: Parser (Identifier, CallParam)
callParam = inlineParam <|> templateParam
    where inlineParam = openCloseTag "param" header
          paramExpr = ParamExpr <$> expr
          header = (,) <$> identifier
                       <*> (betweenSpace (char ':') *> paramExpr)
          templateParam = (,) <$> openTag "param" identifier
                              <*> (paramTempl <* closeTag_ "param")
          paramTempl = ParamTemplate <$> contents ["param"]

opSymbol s = betweenSpace (string s)

liftOp op x = ExprOp $ op x
liftOp2 op x y = ExprOp $ op x y
liftOp3 op x y z = ExprOp $ op x y z

conditionalExpr :: Parser Expr
conditionalExpr = liftOp3 OpConditional <$> binopExpr
                                        <*> (opSymbol "?" *> expr)
                                        <*> (opSymbol ":" *> expr)

expr :: Parser Expr
expr = conditionalExpr <|> binopExpr

binopExpr :: Parser Expr
binopExpr = E.buildExpressionParser opTable term

term :: Parser Expr
term = bracedC '(' ')' expr
   <|> ExprLiteral <$> literal
   <|> ExprFuncCall <$> funcCall
   <|> ExprVar <$> variable

opTable = [ [ prefix (string "-") (liftOp OpNeg)
            , prefix (string "not" *> space_) (liftOp OpNot)
            ]
          , [ binarySym "*" (liftOp2 OpMul) E.AssocLeft
            , binarySym "/" (liftOp2 OpDiv) E.AssocLeft
            , binarySym "%" (liftOp2 OpMod) E.AssocLeft
            ]
          , [ binarySym "+" (liftOp2 OpPlus) E.AssocLeft
            , binarySym "-" (liftOp2 OpMinus) E.AssocLeft
            ]
          , [ binarySym "<" (liftOp2 OpLess) E.AssocNone
            , binarySym ">" (liftOp2 OpGreater) E.AssocNone
            , binarySym "<=" (liftOp2 OpLessEq) E.AssocNone
            , binarySym ">=" (liftOp2 OpGreaterEq) E.AssocNone
            ]
          , [ binarySym "==" (liftOp2 OpEqual) E.AssocNone
            , binarySym "!=" (liftOp2 OpNotEqual) E.AssocNone
            ]
          , [ binaryWord "and" (liftOp2 OpAnd) E.AssocRight
            ]
          , [ binaryWord "or" (liftOp2 OpOr) E.AssocRight
            ]
          ]
    where
        binary op fun assoc = E.Infix (fun <$ op <* optSpace_) assoc
        prefix op fun = E.Prefix (fun <$ op <* optSpace_)
        binarySym sym = binary (opSymbol sym)
        binaryWord w = binary (betweenSpace1 $ string w)

binOp :: Parser a
      -> (Expr -> Expr -> OpExpr)
      -> Parser Expr
      -> Parser Expr
      -> Parser Expr
binOp op constr inner1 inner2 = ExprOp <$>
         (constr <$> inner1 <*> (op *> inner2))

funcCall :: Parser FuncCall
funcCall = FuncCall <$> identifier <* optSpace_
                    <*> listLike (char '(') (char ')') (char ',') expr

literal :: Parser Literal
literal = constant
      <|> (LiteralString <$> quotedString '\'')
      <|> numLiteral
      <|> (LiteralList <$> jsListLike expr)
      <|> (LiteralMap <$> literalMap)

constant :: Parser Literal
constant = LiteralNull <$ string "null"
       <|> LiteralBool <$> bool

bool :: Parser Bool
bool = True <$ string "true"
   <|> False <$ string "false"

numLiteral :: Parser Literal
numLiteral = LiteralNumber <$> (signed hexNumber)
         <|> LiteralNumber <$> number
    where hexNumber = I <$> ("0x" .*> (decodeHexUnsafe <$> many hexDigitUpper))

jsListLike = listLike (char '[') (char ']') (char ',')

literalMap :: Parser [(Expr, Expr)]
literalMap = jsListLike (keyValuePair ":" expr expr) <|> emptyMap
    where emptyMap = [] <$ bracedC '[' ']' (char ':')

variable :: Parser Variable
variable = InjectedVar <$> ("$ij." .*> location)
       <|> LocalVar <$> (char '$' *> location)
       <|> GlobalVar <$> location

locSegment :: Parser Expr
locSegment = char '.' *> optSpace_ *> offsetOrId
         <|> bracedC '[' ']' expr
    where offsetOrId = (exprInt <$> decimal) <|> (exprStr <$> identifier)

location :: Parser Location
location = Location <$> identifier <*> many (optSpace_ *> locSegment)

findDup :: (Eq a) => [a] -> Maybe a
findDup (x:xs) = if x `elem` xs then Just x else findDup xs
findDup [] = Nothing

attributes :: [(T.Text, Parser a)] -> Parser [a]
attributes lst = fromMaybe [] <$> optional (space_ *> attrLst)
    where attrLst = do pairs <- (choice choices) `sepBy` space_
                       case findDup $ map fst pairs of
                          Just key -> fail $ "duplicate attribute: " ++ T.unpack key
                          _ -> return ()
                       return $ map snd pairs
          choices = map attr lst
          attr (k,p) = (k,) <$> (string k *> char '=' *> quoted '"' p)

keyValuePair :: T.Text -> Parser a -> Parser b -> Parser (a, b)
keyValuePair sep k v = (,) <$> k <*> (betweenSpace (string sep) *> v)

anyTagText :: Parser T.Text
anyTagText = takeTill (=='}')

identifier :: Parser Identifier
identifier = T.pack <$> ((:) <$> first <*> many rest)
    where first = letter <|> (char '_')
          rest = first <|> digit

templatePath :: Parser TemplatePath
templatePath = PathRelative <$> (char '.' *> identifier)
           <|> PathFull <$> fullPath

fullPath :: Parser [Identifier]
fullPath = identifier `sepBy1` (char '.')

quoted c p = quotedString c >>= delegate p

quotedString :: Char -> Parser T.Text
quotedString c = T.pack <$> between (char c) (char c) (many innerChar)
    where innerChar = char '\\' *> (escapeSeq <|> unicodeSeq)
                  <|> satisfy (`notElem` [c,'\\'])

escapeSeq :: Parser Char
escapeSeq = choice (zipWith decode "bnfrt\\\"'" "\b\n\f\r\t\\\"'")
    where decode c r = r <$ char c

unicodeSeq :: Parser Char
unicodeSeq = char 'u' *> (intToChar <$> decodeHexUnsafe <$> count 4 hexDigit)
    where intToChar = toEnum . fromIntegral

-- whitespace and comments

spaceAndComments_ :: Parser ()
spaceAndComments_ = () <$ many1 (lineComment_ <|> multiLineComment_ <|> space_)

optSpaceAndComments_ :: Parser ()
optSpaceAndComments_ = () <$ optional spaceAndComments_

space_ = skipWhile1 isSpace
optSpace_ = skipWhile isSpace

lineComment_ :: Parser ()
lineComment_ = "//" .*> skipWhile (not . isVSpace)

multiLineComment_ :: Parser ()
multiLineComment_ = () <$ string "/*" <* manyTill anyChar (string "*/")

-- helpers

delegate :: (Monad m) => Parser a -> T.Text -> m a
delegate p txt =
    case parseOnly (p <* endOfInput) txt of
        Left x -> fail x
        Right y -> return y

listLike l r sep inner = braced l r (inner `sepBySpaceTrailing` sep)

nothing = string T.empty

isVSpace c = isSpace c && not (isHorizontalSpace c)
skipWhile1 pred = () <$ takeWhile1 pred

-- parses the line ending as a single linefeed and the
-- end of the file as the empty string
lineEnding = "\n" <$ (string "\r\n" <|> string "\n" <|> string "\r")
         <|> "" <$ endOfInput

braced :: Parser l -> Parser r -> Parser a -> Parser a
braced l r = between (l *> optSpace_) (optSpace_ *> r)

bracedC l r = braced (char l) (char r)

between :: Parser a -> Parser b -> Parser c -> Parser c
between left right main = left *> main <* right

betweenSpace = between optSpace_ optSpace_
betweenSpace1 = between space_ space_

hexDigitUpper = satisfy (inClass "0-9A-F")
hexDigit = satisfy (inClass "0-9a-fA-F")

sepBySpace :: Parser a -> Parser s -> Parser [a]
a `sepBySpace` sep = a `sepBy` (betweenSpace sep)

sepBySpaceTrailing :: Parser a -> Parser s -> Parser [a]
a `sepBySpaceTrailing` sep = a `sepBySpace` sep <* optional (optSpace_ *> sep)

decodeHexUnsafe :: String -> Integer
decodeHexUnsafe hex = fromMaybe 0 (headMay $ map fst $ readHex hex)

exprInt = ExprLiteral . litInt
exprStr = ExprLiteral . LiteralString

litInt = LiteralNumber . I
litFloat = LiteralNumber . D

-- =====================================================================
--- TESTING
-- =====================================================================

joinT = T.init . T.unlines

tests = htfMain htf_Language_Soy_Parser_thisModulesTests

testParser p inout errin =
    do mapM_ (\(inp, exp) -> assertEqual (Right exp) (parseAll inp)) inout
       mapM_ (assertLeft . parseAll) errin
    where parseAll = parseOnly (p <* endOfInput)

testParserNoOut p okin errin =
    do mapM_ (assertRight . parseAll) okin
       mapM_ (assertLeft . parseAll) errin
    where parseAll = parseOnly (p <* endOfInput)

test_spaceAndComments_ = testParserNoOut spaceAndComments_
        [ joinT [ "   "
                , "  //test asd"
                , "// test "
                , "/* asd "
                , "   foo "
                , " */ "
                , " " ]
        , "//test"
        ]
        [ "foo"
        , "/*/**/*/"
        ]

test_contentText = testParser contentText
        [ (joinT [ "  http://test /**///foo\r"
                 , " asd"
                 , "   asd //test  "
                 , "  foo " ], "  http://test \n asd\n   asd \n  foo ")
        , ("//test\nasd", "\nasd")
        , ("//test", "")
        , ("//test\n//abc\n/**///abc\nasd//test\nasd //", "\n\n\nasd//test\nasd ")
        ]
        []

test_openTag = testParser (openTag "test" (string "{}"))
        [ ("{test {}}", "{}")
        , ("{test {} }", "{}")
        , ("{{test {}}}", "{}")
        ]
        [ "{ test {}}"
        ]

test_namespace = testParser namespace
        [ ("{namespace foo.bar.baz}", Namespace ["foo","bar","baz"] Nothing)
        , ("{namespace foo.bar.baz autoescape=\"false\"}",
                  Namespace ["foo","bar","baz"] (Just NoEscape))
        ]
        []

test_template = testParser template
        [ (joinT [ "{template .foo}"
                 , "foo"
                 , "{/template}"
                 ],
              Template "foo" [ContentText "\nfoo\n"] False Nothing)
        , (joinT [ "{template .foo private=\"true\" autoescape=\"false\"}"
                 , "foo"
                 , "{/template}"
                 ],
              Template "foo" [ContentText "\nfoo\n"] True (Just NoEscape))
        ]
        []

test_sepBySpace = testParser (decimal `sepBySpace` (char ','))
        [ ("1  ,  2  ,  3", [1,2,3])
        ]
        [ "1,2,3,"
        , " 1,2,3 " ]

test_sepBySpaceTrailing = testParser (decimal `sepBySpaceTrailing` (char ','))
        [ ("1  ,  2  ,  3  ,", [1,2,3])
        ]
        [ "1,2,3,,"
        , " 1,2,3, " ]

test_listLike = testParser (listLike (char '(') (char ')') (char ',') decimal)
        [ ("(1,2,3,)", [1,2,3])
        ]
        [ "(1,2(" ]

test_jsListLike = testParser (jsListLike $ decimal)
        [ ("[1,2,3]", [1,2,3])
        , ("[ 1 , 2 , 3 , ]", [1,2,3])
        ]
        [ "[1,2,3,,]" ]

test_quotedString = testParser (quotedString '"')
        [ ("\"abc\"", "abc")
        , ("\"abc\\\"\\b\\n\\'\\\\\"", "abc\"\b\n'\\")
        , ("\"\\uffff\"", "\65535")
        , ("\"\\\"\\\"\\\''\"", "\"\"''")
        ]

        [ "\"abc"
        , "\"\\a\\123\""
        ]
    >> testParser (quotedString '\'')
        [ ("'test'", "test")
        , ("'\\\'\\\"'", "'\"")
        ]

        [ "'"
        ]

test_attributes = testParser (attributes [ ("foo", char 'a')
                                         , ("bar", char 'b')])
        [ (" foo=\"a\" bar=\"b\"", "ab")
        , (" bar=\"b\" foo=\"a\"", "ba")
        , (" bar=\"b\"", "b")
        , (" ", "")
        ]
        [ " bar=\"b\" bar=\"a\""
        , "bar =\"b\""
        , "bar= \"b\""
        , "bar=\"b\""
        ]

test_identifier = testParserNoOut identifier
        [ "abcdef"
        , "AB0_13__"
        , "\220"
        , "__a"
        ]

        [ "1abc"
        , "ac-de"
        ]

test_variable = testParser variable
        [ ("$ij.test[0]['test']", InjectedVar (Location "test"
                [exprInt 0, exprStr "test"]))
        , ("test.ABC", GlobalVar (Location "test"
                [exprStr "ABC"]))
        , ("$local.var['test']", LocalVar (Location "local"
                [exprStr "var", exprStr "test"]))
        , ("$local . 0 [1+2] [0x0]", LocalVar (Location "local"
                [exprInt 0, ExprOp (OpPlus (exprInt 1) (exprInt 2)), exprInt 0]))
        ]

        [ "$test."
        , "$test.0asd.abc"
        , "$test.abc-def"
        , "$test.1+2"
        ]

test_literalMap = testParser literalMap
        [ ("[:]", [])
        , ("['abc':1, (3+4):2]", [ (exprStr "abc", exprInt 1)
                                 , (ExprOp (OpPlus (exprInt 3) (exprInt 4)), exprInt 2)
                                 ])
        , ("[ '1 foo' : 1 ]", [ (exprStr "1 foo", exprInt 1) ])
        ]
        []

test_numLiteral = testParser numLiteral
        [ ("3.3", litFloat 3.3)
        , ("42", litInt 42)
        , ("0", litInt 0)
        , ("-0", litInt 0)
        , ("-0.111", litFloat (-0.111))
        , ("-10", litInt (-10))
        , ("0xFF", litInt 255)
        , ("-0xFF", litInt (-255))
        , ("123123123123123123123", litInt 123123123123123123123)
        ]

        [ "0b123"
        , "1a"
        , "0xff"
        , "0xGG"
        ]

test_constant = testParser constant
        [ ("true", LiteralBool True)
        , ("false", LiteralBool False)
        , ("null", LiteralNull)
        ]
        [ "True" ]

test_funcCall = testParser funcCall
        [ ("test ( 1 , 2,3, )",
                FuncCall "test" [ exprInt 1, exprInt 2, exprInt 3 ])
        ]
        []

test_expr = testParser expr
        [ ("['A':3, 'B':[ 1,2,' test'], 'C':[ true, false, null]]",
                ExprLiteral (LiteralMap
                    [ (exprStr "A", exprInt 3)
                    , (exprStr "B", ExprLiteral (LiteralList
                                    [ exprInt 1 , exprInt 2 , exprStr " test" ]))
                    , (exprStr "C", ExprLiteral (LiteralList
                                    [ ExprLiteral (LiteralBool True)
                                    , ExprLiteral (LiteralBool False)
                                    , ExprLiteral LiteralNull ]))
                    ]))
        , ("[]", ExprLiteral (LiteralList []))
        , ("[:]", ExprLiteral (LiteralMap []))
        , ("1 + 2 == test(1)",
                ExprOp (OpEqual (ExprOp (OpPlus (exprInt 1) (exprInt 2)))
                                (ExprFuncCall (FuncCall "test" [exprInt 1]))))
        , ("1*2 + 3/4%5",
                ExprOp (OpPlus (ExprOp (OpMul (exprInt 1) (exprInt 2)))
                               (ExprOp (OpMod (ExprOp (OpDiv (exprInt 3)
                                                             (exprInt 4)))
                                              (exprInt 5)))))
        , ("1 and 2 and 3",
                ExprOp (OpAnd (exprInt 1) (ExprOp (OpAnd (exprInt 2) (exprInt 3)))))
        , ("1 and 2 ? 3 or 5 : 7 == 8", ExprOp (OpConditional
                (ExprOp (OpAnd (exprInt 1) (exprInt 2)))
                (ExprOp (OpOr (exprInt 3) (exprInt 5)))
                (ExprOp (OpEqual (exprInt 7) (exprInt 8)))))
        , ("1 ? 2?3:4 : 5", ExprOp (OpConditional
                (exprInt 1)
                (ExprOp (OpConditional (exprInt 2) (exprInt 3) (exprInt 4)))
                (exprInt 5)))
        , ("1 ? 2 : 3?4:5", ExprOp (OpConditional
                (exprInt 1)
                (exprInt 2)
                (ExprOp (OpConditional (exprInt 3) (exprInt 4) (exprInt 5)))))
        , ("(1+2)*3 + $data[0] == 3", ExprOp (OpEqual
                (ExprOp (OpPlus
                    (ExprOp (OpMul
                        (ExprOp (OpPlus (exprInt 1) (exprInt 2)))
                        (exprInt 3)))
                    (ExprVar (LocalVar (Location "data" [exprInt 0])))))
                (exprInt 3)))
        ]
        []

nil = CommandText ""
test_contents = testParser (contents [])
        [ (joinT [ "test{nil}test2"
                 , "{nil}"
                 , "test3" ],
                [ ContentText "test"
                , ContentCommand nil
                , ContentText "test2\n"
                , ContentCommand nil
                , ContentText "\ntest3"
                ])
        ]
        []

test_foreachCommand = testParser foreachCommand
        [ (joinT [ "{foreach $test in $data + 5}"
                 , "test"
                 , "{ifempty}"
                 , "test2"
                 , "{/foreach}" ],
                ForeachCommand "test"
                               (ExprOp (OpPlus
                                   (ExprVar (LocalVar (Location "data" [])))
                                   (exprInt 5)))
                               [ContentText "\ntest\n"]
                               (Just [ContentText "\ntest2\n"]))
        , (joinT [ "{foreach $test in $data + 5}"
                 , "test"
                 , "{/foreach}" ],
                ForeachCommand "test"
                               (ExprOp (OpPlus
                                   (ExprVar (LocalVar (Location "data" [])))
                                   (exprInt 5)))
                               [ContentText "\ntest\n"]
                               Nothing)
        ]
        []

test_forCommand = testParser forCommand
        [ (joinT [ "{for $test in range(10)}"
                 , "test"
                 , "{/for}" ],
                ForCommand "test" (exprInt 0) (exprInt 10) (exprInt 1) [ContentText "\ntest\n"])
        , (joinT [ "{for $test in range(2, 10)}"
                 , "test"
                 , "{/for}" ],
                ForCommand "test" (exprInt 2) (exprInt 10) (exprInt 1) [ContentText "\ntest\n"])
        , (joinT [ "{for $test in range(2, 10, 3)}"
                 , "test"
                 , "{/for}" ],
                ForCommand "test" (exprInt 2) (exprInt 10) (exprInt 3) [ContentText "\ntest\n"])
        ]
        []

test_ifCommand = testParser ifCommand
        [ (joinT [ "{if 1 == 2}"
                 , "test"
                 , "{elseif 3 == 3}"
                 , "test2"
                 , "{elseif 4 == 4}"
                 , "test3"
                 , "{else}"
                 , "test4"
                 , "{/if}" ],
                IfCommand [ (ExprOp (OpEqual (exprInt 1) (exprInt 2)),
                                [ContentText "\ntest\n"])
                          , (ExprOp (OpEqual (exprInt 3) (exprInt 3)),
                                [ContentText "\ntest2\n"])
                          , (ExprOp (OpEqual (exprInt 4) (exprInt 4)),
                                [ContentText "\ntest3\n"])
                          ]
                          (Just [ ContentText "\ntest4\n" ]))
        , (joinT [ "{if 1 == 2}"
                 , "test"
                 , "{/if}" ],
                IfCommand [ (ExprOp (OpEqual (exprInt 1) (exprInt 2)),
                                [ContentText "\ntest\n"])
                          ]
                          Nothing)
        ]
        []

test_switchCommand = testParser switchCommand
        [ (joinT [ "{switch 1}"
                 , "{case 1, 2}"
                 , "test1"
                 , "{case 3}"
                 , "test2"
                 , "{default}"
                 , "test3"
                 , "{/switch}" ],
                SwitchCommand (exprInt 1)
                              [ ([exprInt 1, exprInt 2],
                                     [ContentText "\ntest1\n"])
                              , ([exprInt 3],
                                     [ContentText "\ntest2\n"])
                              ]
                              (Just [ContentText "\ntest3\n"]))
        , (joinT [ "{switch 1}"
                 , "{/switch}" ],
                SwitchCommand (exprInt 1) [] Nothing)
        ]
        []

test_callCommand = testParser callCommand
        [ ("{call .test /}", CallCommand (PathRelative "test") Nothing [])
        , ("{call test.abc data=\"all\"/}",
                CallCommand (PathFull ["test", "abc"])
                            (Just CallDataAll)
                            [])
        , ("{call .test data=\"$data\"/}",
                CallCommand (PathRelative "test")
                            (Just (CallDataExpr (ExprVar (LocalVar (Location "data" [])))))
                            [])
        , ("{call .test data=\"1\"/}",
                CallCommand (PathRelative "test")
                            (Just (CallDataExpr (exprInt 1)))
                            [])
        , (joinT [ "{call .test}"
                 , "  {param foo: 1 /}"
                 , "  {param bar}"
                 , "    test"
                 , "  {/param}"
                 , "{/call}" ],
                CallCommand (PathRelative "test")
                            Nothing
                            [ ("foo", ParamExpr (exprInt 1))
                            , ("bar", ParamTemplate [
                                            ContentText "\n    test\n  " ])
                            ])
        ]
        [ "{call .sub.foo}{/call}"
        ]

test_printCommand = testParser (printCommand ["nil"])
        [ ("{print 1 }", PrintCommand (exprInt 1) [])
        , ("{ 2 }", PrintCommand (exprInt 2) [])
        , ("{else}", PrintCommand (ExprVar (GlobalVar (Location "else" []))) [])
        , ("{print 1|noAutoescape |id|escapeHtml|escapeUri|escapeJs }",
              PrintCommand (exprInt 1)
                [ PrintEscape NoEscape
                , PrintId
                , PrintEscape EscapeHtml
                , PrintEscape EscapeUri
                , PrintEscape EscapeJs
                ])
        , ("{print 1|changeNewlineToBr}",
              PrintCommand (exprInt 1) [PrintChangeNewlineToBr])
        , ("{print 1|insertWordBreaks: 10}",
              PrintCommand (exprInt 1) [PrintInsertWordBreaks 10])
        , ("{print 1|truncate:10 |truncate: 10 , false }",
              PrintCommand (exprInt 1)
                  [ PrintTruncate 10 True
                  , PrintTruncate 10 False
                  ])
        ]
        [ "{nil}"
        , "{/param}"
        , "{ print 1}"
        , "{print 1|truncate :10,false}"
        , "{print 1| id}"
        ]

test_literalCommand = testParser literalCommand
        [ ("{literal}{foo}{bar}{nil}{1}/**/{/lit //test{/literal}",
                "{foo}{bar}{nil}{1}/**/{/lit //test")
        ]
        []

test_nestedCommand = testParser (command [])
        [ (joinT [ "{if 1 == 2}"
                 , "  {foreach $test in 3}"
                 , "    test1"
                 , "  {ifempty}"
                 , "    test2"
                 , "  {/foreach}"
                 , "{else}"
                 , "  test3"
                 , "{/if}" ],
                CommandIf (IfCommand [
                    (ExprOp (OpEqual (exprInt 1) (exprInt 2)),
                        [ ContentText "\n  "
                        , ContentCommand (CommandForeach (ForeachCommand
                              "test"
                              (exprInt 3)
                              [ContentText "\n    test1\n  "]
                              (Just [ContentText "\n    test2\n  "])))
                        , ContentText "\n"
                        ])
                    ]
                    (Just [ ContentText "\n  test3\n" ])))
        , (joinT [ "{if 1 == 2}"
                 , "  {if 3 == 4}"
                 , "    test1"
                 , "  {/if}"
                 , "{else}"
                 , "  test2"
                 , "{/if}" ],
                CommandIf (IfCommand [
                    (ExprOp (OpEqual (exprInt 1) (exprInt 2)),
                        [ ContentText "\n  "
                        , ContentCommand (CommandIf (IfCommand
                             [ (ExprOp (OpEqual (exprInt 3) (exprInt 4)),
                                    [ ContentText "\n    test1\n  " ])
                             ]
                             Nothing))
                        , ContentText "\n"
                        ])
                    ]
                    (Just [ ContentText "\n  test2\n" ])))
        ]
        []
