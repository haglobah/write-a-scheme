{-# LANGUAGE ExistentialQuantification #-}

module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Function ((&))
import Numeric (readOct, readHex)
import Control.Monad.Except
import System.IO
import Control.Monad.IO.Class
import Data.IORef
-- import Text.Parsec (parserPlus)

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool
            | Char Char
            | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
            | Func { params :: [String], vararg :: Maybe String,
                     body :: [LispVal], closure :: Env}
            | IOFunc ([LispVal] -> IOThrowsError LispVal)
            | Port Handle

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser ParseError
              | BadSpecialForm String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String

data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

instance Show LispError where show = showError

type ThrowsError = Either LispError
type IOThrowsError = ExceptT LispError IO
type Env = IORef [(String, IORef LispVal)]

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

showError :: LispError -> String
showError (UnboundVar message varname) = message <> ": " <> varname
showError (BadSpecialForm message form) = message <> ": " <> show form
showError (NotFunction message func) = message <> ": " <> show func
showError (NumArgs expected found) = "Expected " <> show expected <> " args: found values " <> unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " <> expected <> ", found " <> show found
showError (Parser parseErr) = "Parse error at " <> show parseErr
showError (Default err) = show err

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ cond prompt action = do
  result <- prompt
  if cond result
    then return ()
    else action result >> until_ cond prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)]))
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "λ> ") . evalAndPrint

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives
                            ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)


main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            _ -> runOne $ args

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError (Parser err)
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

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

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _env val@(String _) = return val 
eval _env val@(Number _) = return val 
eval _env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval _env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do result <- eval env pred
     case result of
        Bool False -> eval env alt
        Bool True -> eval env conseq
        _otherwise -> throwError (TypeMismatch "bool" pred)
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
  load filename >>= fmap last . mapM (eval env)
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _env badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = 
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = last <$> mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env
apply (IOFunc func) args = func args
apply non_func _args = throwError (NotFunction "Wrong expression in function position" (show non_func))

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _xs)]         = return x
car [DottedList (x : _xs) _] = return x
car [badArg]               = throwError (TypeMismatch "pair" badArg)
car badArgList             = throwError (NumArgs 1 badArgList)

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_x : xs)]        = return (List xs)
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return (DottedList xs x)
cdr [badArg]                = throwError (TypeMismatch "pair" badArg)
cdr badArgList              = throwError (NumArgs 1 badArgList)

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return (List [x])
cons [x, List xs] = return (List (x : xs))
cons [x, DottedList xs xlast] = return (DottedList (x : xs) xlast)
cons [x1, x2] = return (DottedList [x1] x2)
cons badArgList = throwError (NumArgs 2 badArgList)

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
     `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- equal [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return (Bool False)

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = fmap List $ load filename

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
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?g", eqv),
    ("equal?", equal)
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
unpackStr notString = throwError (TypeMismatch "string" notString)

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError (TypeMismatch "bool" notBool)

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _op [] = throwError (NumArgs 2 [])
numericBinop op singleVal@[_] = throwError (NumArgs 2 singleVal)
numericBinop op params = mapM unpackNum params >>= return . Number . (foldl1 op)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError (TypeMismatch "number" notNum)

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char contents) = "#\\" ++ [contents]
showVal (Atom name) = name
showVal (Number num) = show num
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List content) = "(" ++ unwordsList content ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = _body, closure = _env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

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