module Lib where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Function ((&))
import Numeric (readOct, readHex)
import Control.Monad.Except
-- import Text.Parsec (parserPlus)

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool
            | Char Char

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String [LispVal]
              | Parser ParseError
              | BadSpecialForm String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname) = message <> ": " <> varname
showError (BadSpecialForm message form) = message <> ": " <> show form
showError (NotFunction message func) = message <> ": " <> show func
showError (NumArgs expected found) = "Expected " <> show expected <> " args: found values " <> unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " <> expected <> ", found " <> show found
showError (Parser parseErr) = "Parse error at " <> show parseErr
showError (Default err) = show err

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError (Parser err)
  Right val -> return val

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseNumber
        <|> try parseChar
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

parseExprWithUnquote :: Parser LispVal
parseExprWithUnquote = parseAtom
                  <|> parseString
                  <|> try parseNumber
                  <|> try parseChar
                  <|> parseQuoted
                  <|> parseQuasiQuoted
                  <|> parseUnquoted
                  <|> parseUnquoteSpliced
                  <|> do char '('
                         x <- try parseList <|> parseDottedList
                         char ')'
                         return x

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val 
eval val@(Number _) = return val 
eval val@(Bool _) = return val 
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError (NotFunction "Unrecognized primitive function args" func))
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("symbol?",  unaryOp symbolP),
    ("bool?", unaryOp boolP),
    ("list?", unaryOp listP),
    ("char?", unaryOp charP),
    ("string?", unaryOp stringP),
    ("symbol->string", unaryOp symbol2string),
    ("string->symbol", unaryOp string2symbol),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=))
  ]

symbolP (Atom _) = Bool True
symbolP _ = Bool False

boolP (Bool _) = Bool True
boolP _ = Bool False

listP (List _) = Bool True
listP _ = Bool False

charP (Char _) = Bool True
charP _ = Bool False

stringP (String _) = Bool True
stringP _ = Bool False

symbol2string (Atom s) = (String s)
symbol2string _ = error "Expecting an Atom"

string2symbol (String s) = (Atom s)
string2symbol _ = error "Expecting a String"

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp func [arg] = return (func arg)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                            then throwError (NumArgs 2 args)
                            else do left <- unpacker (args !! 0)
                                    right <- unpacker (args !! 1)
                                    return (Bool (left `op` right))

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return (show s)
unpackStr (Bool s) = return (show s)
unpackStr notString = throwError (TypeMismatch "string" [notString])

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError (TypeMismatch "bool" [notBool])

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _op [] = throwError (NumArgs 2 [])
numericBinop op singleVal@[_] = throwError (NumArgs 2 singleVal)
numericBinop op params = mapM unpackNum params >>= return . Number . (foldl1 op)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError (TypeMismatch "number" [notNum])

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char contents) = "#\\" ++ [contents]
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List content) = "(" ++ unwordsList content ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

parseList :: Parser LispVal
parseList = fmap List (sepBy parseExpr spaces)

parseDottedList :: Parser LispVal
parseDottedList = do
  hd <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return (DottedList hd tail)

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return (List [Atom "quote", x])

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do _ <- char '`'
                      x <- parseExprWithUnquote
                      return (List [Atom "quasiquote", x])
                      
parseUnquoted :: Parser LispVal
parseUnquoted = do _ <- char ','
                   x <- parseExpr
                   return (List [Atom "unquote", x])

parseUnquoteSpliced :: Parser LispVal
parseUnquoteSpliced = do _ <- string ",@"
                         x <- parseExpr
                         return (List [Atom "unquote-splice", x])

parseChar :: Parser LispVal
parseChar = do 
  _ <- string "#\\"
  s <- many1 letter
  return (case s of
    "space" -> Char ' '
    "newline" -> Char '\n'
    [x] -> Char x)

parseEscaped :: Parser Char
parseEscaped = do
  _ <- char '\\'
  c <- oneOf ['\\', '"', 'n', 'r', 't']
  return $ case c of
    '\\' -> c
    '"' -> c
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _ -> c

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (parseEscaped <|> noneOf ['\\', '"'])
  _ <- char '"'
  return (String x)

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return (case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom)

parseNumber :: Parser LispVal
parseNumber = parsePlain <|> parseRadix

parsePlain :: Parser LispVal
parsePlain = do
  many1 digit >>= return . (Number . read)

parseRadix :: Parser LispVal
parseRadix = char '#' >> (parseBinary <|> parseOctal <|> parseDecimal <|> parseHex)

parseBinary :: Parser LispVal
parseBinary = char 'b' >> many (oneOf "01") >>= return . Number . binToInt

binToInt :: String -> Integer
binToInt s =
  s
  & reverse
  & map to_int
  & zip [0..]
  & map (\(index, bin) -> 2^index * bin)
  & sum
  where to_int '0' = 0
        to_int '1' = 1

getNum :: [(Integer, String)] -> Integer
getNum list = list & head & fst 

parseOctal :: Parser LispVal
parseOctal = do char 'o' >> many (oneOf "01234567") >>= return . Number . getNum . readOct

parseDecimal :: Parser LispVal
parseDecimal = do char 'd' >> many1 digit >>= return . Number . read 

parseHex :: Parser LispVal
parseHex = do char 'x' >> many (oneOf "0123456789abcdef") >>= return . Number . getNum . readHex

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space