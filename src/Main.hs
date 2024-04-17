module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Function ((&))
import Numeric (readOct, readHex)
-- import Control.Monad
-- import Text.Parsec (parserPlus)

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool
            | Char Char

instance Show LispVal where show = showVal

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String ("No match: " ++ show err)
  Right val -> val

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

eval :: LispVal -> LispVal
eval val@(String _) = val 
eval val@(Number _) = val 
eval val@(Bool _) = val 
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func (map eval args)

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number (foldl1 op (map unpackNum params))

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst (head parsed)
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

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