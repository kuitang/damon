-- TODO: Compiler warning about non-exhaustive patterns?
-- TODO: Pretty printing!
--
-- Once a monad, always a monad.
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import Data.Maybe
import Data.IORef

-----------------------------------------------------------------------------
-- PARSER
-----------------------------------------------------------------------------

-- Collect our elementary parsers here. This is also a Monad,
-- as evidenced by the whitespaced-set-off Char.
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- space and skipMany1 are Parsec monads.
spaces :: Parser ()
spaces = skipMany1 space

-- Stuff to store our Lisp values.
-- Remember: Constructors and types have different namespaces. So the
-- String constructor does not conflict with the String type.
--
-- PrimitiveFunc is to store our interpreter-defined functions so they
-- can be passed around. User-defined functions use Func, which is a
-- record type.
--
-- We modify the tutorial to use the type
-- (String, [LispVal] -> ThrowsError LispVal)
-- so we can also store the name of the function (and we populate it
-- with the full entry of primitives).

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Character Char
             | String String
             | Bool Bool
             | PrimitiveFunc (String, [LispVal] -> ThrowsError LispVal)
             | Func { params  :: [String],
                      vararg  :: (Maybe String),
                      body    :: [LispVal],
                      closure :: Env }
             | IOPrimitiveFunc (String, [LispVal] -> IOThrowsError LispVal)
             | Port Handle

-- Patterns can be nested, matching inside -> outside, left -> right.
-- We can have multiple matches, but the first text order is used.

-- Helper functions for list. This just concatenates strings representing the
-- elements of the list, which are obtained by recursing on showVal 

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Skipping ahead to the next section
showVal :: LispVal -> String

-- Base cases.
-- TODO: Tabelize the Characters call... (names and chars lists)
showVal (String contents)    = show contents
showVal (Atom name)          = name
showVal (Number contents)    = show contents
showVal (Character ' ')      = "#\\space"
showVal (Character '\n')     = "#\\newline"
showVal (Character contents) = "#\\" ++ [contents]
showVal (Bool True)          = "#t"
showVal (Bool False)         = "#f"

-- Recursive cases.
showVal (List contents)   = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ 
                                 " . " ++ showVal tail ++ ")"

showVal (PrimitiveFunc (name, _)) = "<primitive func " ++ name ++ ">"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
       Nothing  -> ""
       Just arg -> " . " ++ arg) ++ ")" ++ showVal (List body)  ++ ")"

showVal (IOPrimitiveFunc (name, _)) = "<io primitive func " ++ name ++ ">"
showVal (Port _)                    = "<io port>"

-- Make LispVal an instance of class Show
instance Show LispVal where
  show = showVal

-- Parsec << is equivalent to a space in BNF, i.e. it parses the elements
-- in sequence.
--
-- In general, use do when you need to interleave >>= calls. Otherwise,
-- you'd have to pass unwieldy lambdas. (The >>= function is reminiscent
-- of CPS...)
--
-- The last line constructs a LispVal, and then wraps it in a Parser monad.

-- TODO: What language feature can do this more efficiently?
--  unescape :: String -> Char
--  unescape "\\\"" = '"'
--  unescape "\\n"  = '\n'
--  unescape "\\t"  = '\t'
--  unescape "\\\\" = '\\'
--
--  escapeChar :: String -> Parser Char
--  escapeChar s = liftM unescape $ try $ string s
--
--  nonquoteOrEscape :: Parser Char
--  nonquoteOrEscape = escapeChar "\\\"" <|> escapeChar "\\n" <|> escapeChar "\\t" <|> escapeChar "\\\\" <|> noneOf "\""

-- More elegant solution: http://codereview.stackexchange.com/questions/2406/parsing-strings-with-escaped-characters-using-parsec

codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

escapedChar code replacement = char code >> return replacement
escaped = char '\\' >> choice (zipWith escapedChar codes replacements)

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (escaped <|> noneOf "\"")
                char '"'
                return $ String x

-- TODO: Check up documentation for what are valid character literals.
-- It seems like standard Scheme is just space and newline.
-- http://sicp.ai.mit.edu/Fall-2003/manuals/scheme-7.5.5/doc/scheme_6.html

names = ["space", "newline"]
chars = [' ',     '\n']

namedChar name ch = string name >> return ch
parseCharacter = liftM Character (string "\\#" >> charSpec) where
  charSpec = choice (zipWith namedChar names chars) <|> anyChar

-- An atom is a letter or symbol, followed by any number of letters, digits,
-- or symbols.
--
-- Note that Parser >>= extracts the value of the (successful) parse.
parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest  <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

-- From Control.Monad
-- -- | Promote a function to a monad.
-- liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
-- liftM f m1              = do { x1 <- m1; return (f x1) }
--
-- The output of (many1 digit) is a Parser String, so we convert it to a 
-- LispVal.
parseNumber :: Parser LispVal
parseNumber =
  do
    prefix <- optionMaybe (char '-')
    str    <- many1 digit
    let retnum = return . Number
        val    = read $ str in
        if isNothing prefix
           then retnum val
           else retnum $ -val

-- The following is equivalent (I did in fact test this)
-- parseNumber = (many1 digit) >>= return . Number . read 

-- sepBy is a Parsec combinator which parses zero or more occurences of
-- parseExpr, separated by spaces. (Which is fine since we can have empty list.)
parseList = liftM List $ sepBy parseExpr spaces

-- endBy like sepBy, but now we end on a space.
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- the dot recognizer is broken...
-- ... ok think about this later... (left-factor the grammar? work out by hand)
-- scanToDot = do { char '.'; return ([], True); }
--           <|>
--             do { x <- parseExpr
--                ; spaces
--                ; scanret1 <- scanToDot
--                ; return ((x:(fst scanret1)), snd scanret1)
--                ; }
--           <|>
--             do { return ([], False); }
-- 
-- parseListOrDottedList = do
--   scanret <- scanToDot
-- 
--   if (snd scanret)
--     then do { tail <- spaces >> char '.' >> spaces >> parseExpr
--             ; return $ DottedList (fst scanret) tail }
--     else return $ List (fst scanret)
-- 
-- sugar
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- string parsers (i.e. not char parsers) seem like they consume input.
-- so they need to be wrapped in try.
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseNumber
         <|> parseString
         <|> parseCharacter
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

-- throwError takes an Error value and lifts it into the Left constructor
-- of either.
--
-- parse :: Parser Char -> String -> String -> Either Err String
-- the second argument "lisp" is just for errors.

-- readOrThrow :: Parser a -> String -> ThrowsError a
-- CAREFUL: Bug here when you forgot to replace parseExpr with parser.
readOrThrow parser input = case parse parser "lisp" input of
  Left  err -> throwError $ Parser err
  Right val -> return val

readExpr     :: String -> ThrowsError LispVal
readExpr     = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

-----------------------------------------------------------------------------
-- METACIRCULAR EVALUATOR
-----------------------------------------------------------------------------
-- Primitive evaluator

-- val@(String _) matches against the String constructor but binds val to the
-- _whole_ LispVal, not just the string contents.

-- String, Number, and Bool are evaluated literally. Lists are interpreted:
-- 1. Quoted lists evaluated to the quoted value
-- 2. Unquoted lists are interpreted as function calls, which are evaluated.

-- Function eval is now a monadic map:
-- mapM :: (a -> m b) -> [a] -> m [b]
--
-- i.e. it takes a monad-returning function (in this case eval), applies it to
-- a list of elements, and wraps it up in a Monad. The arguments are now
-- evaluated sequentially, and the result is a Left error Either monad if any
-- failed.
--
-- Remember that the computation only continues if the Right constructor of
-- Either is active. If ever the Left constructor is activated, it is simply
-- propagated through the rest of the pipeline. So the rest of the functions
-- of the form (a -> m b) don't have to worry about error handling -- if they
-- are called, they must be called with the value of the Right part of Either.
--
-- We do end up modifying our functions to return ThrowsError LispVal instead
-- of LispVal. However, we don't deal with error handling; we just upgrade
-- them to throw errors.

-- the threaded env seems inelegant. Any points-free style?
eval :: Env -> LispVal -> IOThrowsError LispVal

-- Easy things.
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _)   = return val
eval env (Atom var)     = getVar env var

-- Basic special forms.
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do result <- eval env pred
     case result of
          Bool True -> eval env conseq
          otherwise -> eval env alt
eval env (List (Atom "cond" : clauses))       = evalCond Nothing env clauses
eval env (List (Atom "case" : key : clauses)) = eval env key >>= evalCase Nothing env clauses
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var

-- Defines.
-- Variable definition (successor to define word is an atom).
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
-- Function definition (this could be replaced with a macro).
eval env (List (Atom "define" : List (Atom var:params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var:params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
-- varargs only, no named parameters. Syntax is (lambda varargs <body>).
-- This was confusing at first. See R5RS Sec. 4.1.4.
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)

-- THE FUNCTION EVALUATOR!
-- func is now dynamic, e.g. it may be a lambda, or a name, or whatever LispVal
-- that is PrimitiveFunc or Func.
--
-- (Well, this non-explicit form makes your compiler harder, but it supports lambda.
-- But note that env -> codegen, apply -> function dispatch, so maybe it won't be
-- that bad.)

eval env (List (func : args)) = do
  func    <- eval env func
  argVals <- mapM (eval env) args
  apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- There must be some way to factor evalCase and evalCond... everything is the
-- same except for the general case.

-- Maybe is an early return.
evalCond :: (Maybe (IOThrowsError LispVal)) -> Env -> [LispVal] -> IOThrowsError LispVal
evalCond (Just ret) _ _ = ret
-- Only match the _last_ clause that is an else.
evalCond Nothing    env ((List (Atom "else" : exprs)):[]) =
  evalCond (Just $ evalExprs env exprs) env []
evalCond Nothing    env ((List (test:exprs)):rest) =
  do result <- eval env test
     case result of
          Bool True -> if null exprs
                          then evalCond (Just $ return result) env []
                          else evalCond (Just $ evalExprs env exprs) env []
          otherwise -> evalCond Nothing env rest

-- syntax note: the following are equivalent
-- \x -> let (Bool y) = x in return y
-- \(Bool y) -> return y
eqvpUnwrapped :: LispVal -> LispVal -> IOThrowsError Bool
eqvpUnwrapped a b = liftThrowsToIO $ eqvp [a, b] >>= \(Bool y) -> return y

evalCase :: (Maybe (IOThrowsError LispVal)) -> Env -> [LispVal] -> LispVal -> IOThrowsError LispVal
evalCase (Just ret) _ _ _ = ret
-- Only match the _last_ clause that is an else.
evalCase Nothing env ((List (Atom "else" : exprs)):[]) key =
  evalCase (Just $ evalExprs env exprs) env [] key
evalCase Nothing env ((List ((List dat):exprs)):rest) key =
  do
    matches <- mapM (eqvpUnwrapped key) dat
    if or matches
       then evalCase (Just $ evalExprs env exprs) env [] key
       else evalCase Nothing env rest key

evalExprs :: Env -> [LispVal] -> IOThrowsError LispVal
evalExprs env [Atom "=>", callback] = throwError $ NotImplemented "lambda callback cond"
evalExprs env (last:[]) = eval env last
evalExprs env (x:xs)      =
  do _ <- eval env x
     evalExprs env xs

-- Helpers to construct function objects in the environment.
-- Note how everything is in terms of LispVal. Even the vararg variable. That is
-- just a Lisp symbol after all.
makeFunc :: (Maybe String) -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing
makeVarArgs    = makeFunc . Just . showVal

-- lookup func primitives returns the value associated with func in the
-- association list named primitives. However, its return value is a Maybe.
-- The maybe function is defined as
--
-- maybe :: b -> (a -> b) -> Maybe a -> b
-- maybe n f Nothing  = n
-- maybe n f (Just x) = f x
--
-- Note f is a function, applied to the Just constructor. This is a bit odd
-- and asymmetric -- nothing would be lost by returning x.
--
-- Nevertheless, f is useful for our purposes. We really want x args, which
-- is equivalent to ($ args) x because currying:
--
-- $ args = \f -> f args
--
-- so ($ args) x = (\f -> f args) x = x args

-- The JIT can hook in here to dispatch and call a native function.
-- (Primitive functions are already compiled by Haskell, but we may as
--  well include a native code library that is linked in.)
--
-- Although instead of evaluating LispVal as func, we'd have to have some
-- other kind of code object.

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc (_, func)) args = liftThrowsToIO $ func args

-- Step by step:
--
-- 1. e1 = bindVars $ closure $ zip params arg (IO Env)
-- 2. e2 = liftIO $ e1                         (IOThrowsError Env)
--    e2 is now a monad containing the function's _lexical_ context -- it is
--    an environment consisting of closure and the bound parameters.
-- 3. e3 = bindVarArgs varags                  (Env -> IOThrowsError Env)
-- 4. e4 = e2 >>= e3                           (IOThrowsError Env)
-- 5. e5 = e4 >>= evalBody                     (IOThrowsError LispVal)

apply (Func params varargs body closure) args =
  if varargs == Nothing && num params /= num args -- arg length mismatch
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip params args) 
          >>= bindVarArgs varargs
          >>= evalBody
  where remainingArgs = drop (length params) args
        num           = toInteger . length
        evalBody      :: Env -> IOThrowsError LispVal
        evalBody env  = liftM last $ mapM (eval env) body
        -- bind list of remaining args to vararg name
        bindVarArgs   :: (Maybe String) -> Env -> IOThrowsError Env
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing      -> return env

-- TO UNDERSTAND: Curried definition? (???)
-- you can't curry apply (...) = func; probably because of the destructuring?
apply (IOPrimitiveFunc (_, func)) args = func args

-----------------------------------------------------------------------------
-- PRIMITIVE FUNCTIONS
-----------------------------------------------------------------------------

-- NOTE: radix not supported.
-- For now, eqv? and equal? are the same -- equality by value, strong type.
-- In the future, may optimize eq? eqv? to check pointers only.
--
-- In the tutorial, the only difference in equal? was the weak typing.
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("modulo", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("eq?", eqvp),
              ("eqv?", eqvp),
              ("equal?", eqvp),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("pair?", unaryOp listp),
              ("symbol->string", unaryOp symbolToString),
              ("string->symbol", unaryOp stringToSymbol),
              ("number->string", numberToString),
              ("string->number", stringToNumber),
              ("=", transNumBoolBinop (==)),
              ("<", transNumBoolBinop (<)),
              (">", transNumBoolBinop (>)),
              (">=", transNumBoolBinop (>=)),
              ("<=", transNumBoolBinop (<=)),
              ("string=?", transStrBoolBinop (==)),
              ("string<?", transStrBoolBinop (<)),
              ("string>?", transStrBoolBinop (>)),
              ("string<=?", transStrBoolBinop (<=)),
              ("string>=?", transStrBoolBinop (>=))
              ]

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

-- Apply a Haskell function to (wrapped) Lisp values.
-- Note: foldl1 is basically reduce:
--
-- foldl1 f (x:xs) = foldl1 f x xs
-- foldl1 _ []     = error "Prelude.foldl1: empty list"
--
-- This lets us apply binops to lists, i.e. (+ 1 2 3 4).

-- Recall singleVal@[_] captures the pattern to the right of @ (singleton
-- list) but returns the entire list in singleVal.

-- Unpackers
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-----------------------------------------------------------------------------
-- Primitive Func Implementation

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v]    = return $ f v

symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

symbolToString (Atom x)   = String x
stringToSymbol (String x) = Atom x

-- Support arbitrary number of arguments _in the args list_. Specific
-- implementations may have limits.
stringToNumber [String s] = let parsed = reads s in
                                   if null parsed
                                     then throwError $ TypeMismatch "number" $ String s
                                     else return $ Number $ fst $ parsed !! 0
stringToNumber ((String s):(Number r):[])  = throwError $ NotImplemented "string->number with radix"
stringToNumber args@(a:b:c:xs) = throwError $ NumArgs 2 args
stringToNumber bad = throwError $ TypeMismatch "(String, [Number])" $ List bad

numberToString [Number n] = return $ String $ show n
numberToString ((Number n):(Number r):[])  = throwError $ NotImplemented "number->string with radix"
numberToString args@(a:b:c:xs) = throwError $ NumArgs 2 args
numberToString bad = throwError $ TypeMismatch "(Number, [Number])" $ List bad

-- Make a binary relation transitive.
-- TODO: Make Monad (for early return)
transBinOp :: (a -> a -> Bool) -> ([a] -> Bool)
transBinOp op = g Nothing where
  g (Just False) _       = False
  g Nothing     [a, b]   = a `op` b
  g Nothing     (a:b:xs) =
    let abval = a `op` b in
        if abval
           then abval && g Nothing (b:xs) -- still true; keep recurring
           else g (Just False) []         -- false; stop early.
  g _ _                  = error "transBinOp: singleton list"

-- Now wrap that into LispVals
transUnpackedBinOp unpacker op args = mapM unpacker args >>= return . Bool . transBinOp op

transNumBoolBinop  = transUnpackedBinOp unpackNum
transStrBoolBinop  = transUnpackedBinOp unpackStr
transBoolBoolBinop = transUnpackedBinOp unpackBool

-- Helper built-in functions in terms of wrapped variables
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x             -- (car '(a b)) = a ; (car '(a)) = a
car [DottedList (x:xs) _] = return x                          -- (car '(a b . c)) = a
car [badArg]              = throwError $ TypeMismatch "pair" badArg -- (car 'a) -> error
car badArgList            = throwError $ NumArgs 1 badArgList -- (car 'a 'b) -> error

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x                 -- (cdr '(a . b)) = b (NOT a (dotted) list!)
cdr [DottedList (_:xs) x] = return $ DottedList xs x -- (cdr '(a b . c)) = (b . c)
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg -- (cdr 'a) -> error
cdr badArgList            = throwError $ NumArgs 1 badArgList       -- (cdr 'a 'b) -> error

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]   -- (cons 'a '()) = '(a)
cons [x, List xs] = return $ List (x:xs) -- (cons 'a '(b c)) = '(a b c)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast -- (cons 'a '(b . c)) = '(a b . c)
cons [x1, x2] = return $ DottedList [x1] x2 -- second arg is not list -> make dotted list.
cons badArgList = throwError $ NumArgs 2 badArgList

eqvp :: [LispVal] -> ThrowsError LispVal
eqvp [(Bool a1), (Bool a2)]                 = return $ Bool $ a1 == a2
eqvp [(Number a1), (Number a2)]             = return $ Bool $ a1 == a2
eqvp [(String a1), (String a2)]             = return $ Bool $ a1 == a2
eqvp [(Atom a1), (Atom a2)]                 = return $ Bool $ a1 == a2
eqvp [(DottedList xs x), (DottedList ys y)] = eqvp [List $ xs ++ [x], List $ ys ++ [y]]
eqvp [(List a1), (List a2)]                 = return $ Bool $ (length a1 == length a2) &&
                                                              (all eqvpPair $ zip a1 a2)
  where eqvpPair (x1, x2) = case eqvp [x1, x2] of
                              Right (Bool val) -> val
                              Left err -> False
eqvp [_, _]                                 = return $ Bool False
eqvp badArgList                             = throwError $ NumArgs 2 badArgList

-----------------------------------------------------------------------------
-- IO Primitive Func Implementation

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args 

-- 1. openFile ...   -> IO Handle
-- 2. liftIO   ...   -> IOThrowsError Handle
-- 3. liftM Port ... -> IOThrowsError LispVal
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

-- According to R5RS, the return type is unspecified. We choose useful ones.
-- R5RS says the following (our rational in parentheses)
-- 1. No effect if file already closed. (Haskell's hClose has same semantics.)
-- 2. Return type is unspecified. (We choose useful values.)
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

-- Monad comments:
-- 1. hGetLine port      -> IO String
-- 2. liftIO ...         -> IOThrowsError String
-- 3. readExpr           -> ThrowsError LispVal
-- 4. liftThrowsToIO ... -> IOThrowsError LispVal
readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrowsToIO . readExpr

-- Monad comments:
-- 1. hGetLine port      -> IO ()
-- 2. liftIO ...         -> IOThrowsError ()
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

-- helper function
-- This _only_ loads the statements; it does not execute them.
-- Implementing the (load ...) syntax requires a special form, because
-- each line of the file can introduce new bindings.
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrowsToIO . readExprList

-- implementation function
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename


-----------------------------------------------------------------------------
-- ERROR HANDLER
-----------------------------------------------------------------------------
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | NotImplemented String
               | Default String

preErr = (++) "ERROR: "
showError :: LispError -> String
showError (UnboundVar message varname)   = preErr $ message ++ ": " ++ varname
showError (BadSpecialForm message form)  = preErr $ message ++ ": " ++ show form
showError (NotFunction message func)     = preErr $ message ++ ": " ++ show func
showError (NumArgs expected found)       = preErr $ "Expected " ++ show expected ++
                                           " args, but found values " ++ unwordsList found
showError (TypeMismatch expected found)  = preErr $ "Invalid type: expected " ++ expected ++ 
                                           " but found " ++ show found
showError (NotImplemented feature)       = preErr $ feature ++ " is not yet implemented."
showError (Parser parseErr)              = preErr $ "Parse error at " ++ show parseErr

instance Show LispError where show = showError
instance Error LispError where
  noMsg  = Default "An error has occured"
  strMsg = Default

-- Type signature is Either a b  However, constructors can be curried just
-- like functions, so ThrowsError a = Either LispError a. Right value will
-- be the correct (right!) value, if it returns.
type ThrowsError = Either LispError

-----------------------------------------------------------------------------
-- ENVIRONMENT SUPPORT
-----------------------------------------------------------------------------
-- Declare an IORef holding a map from Strings to mutable LispVal.
-- Both the environment (binding of names to values) and values themselves
-- are mutable in Scheme.
type Env = IORef [(String, IORef LispVal)]

-- a wrapped empty environment.
nullEnv :: IO Env
nullEnv = newIORef []

-- Make the startup environment

-- flip can be used to curry the first argument instead of the second:
-- flip f x y = f y x
-- So (flip f x) = \lambda y -> f y x

primitiveBindings :: IO Env

primitiveBindings = do
  let wrapFunc ctor (name, func) = (name, ctor (name, func))
      wrapped   = map (wrapFunc PrimitiveFunc) primitives
      ioWrapped = map (wrapFunc IOPrimitiveFunc) ioPrimitives
  e <- nullEnv
  bindVars e $ wrapped ++ ioWrapped

-- STUPID POINTS FREE STYLE!
--
-- Points free style seems to be the opposite of legibility.
-- (Too elegant to be easy to read.)
--
-- primitiveBindings = nullEnv
--                     >>= (flip bindVars $ (map (wrapFunc PrimitiveFunc) primitives) ++
--                                         (map (wrapFunc IOPrimitiveFunc) ioPrimitives))
--   where wrapFunc ctor (name, func) = (name, ctor (name, func))

-- ErrorT adds error handling to other monads. Until now, we have essentially
-- been running the ThrowsError lambda monads as a subroutine under the IO
-- monad. The only interaction to IO monad has been through trapError, which
-- switches to showing the error message instead of the value.
--
-- We now want to interleave the monads.
--
-- From the documentation
-- newtype ErrorT e m a = { runErrorT :: m (Either e a) }
--
-- where e is the error type, m is the inner monad, and a is an action that
-- can throw an error e. (i.e. to wrap ThrowsError String), a = String.
--
-- This syntax means (essentially) ErrorT e m a is isomorphic to m (Either e a).
type IOThrowsError = ErrorT LispError IO

-- We cannot mix actions of different types in the same do-block, so do-blocks
-- of IOThrowsError must lift LispError throws.
liftThrowsToIO :: ThrowsError a -> IOThrowsError a
liftThrowsToIO (Left err)  = throwError err
liftThrowsToIO (Right val) = return val

-- Environment support
-- Is a given variable bound (defined) in the environment?
-- You should think of env being passed to the RHS, used as the final argument
-- in lookup.
--
-- Maybe takes a callback, so (const True) simply signals that the maybe call
-- returns True if lookup returned a value.
--
-- Note: readIORef :: IORef a -> IO a
--       liftIO    :: IO a    -> m a (m must be a MonadIO, e.g. you can't ever leave IO)
--
--       In particular, m = IOThrowsError LispVal in this context. This is because
--       readIORef returns IO LispVal, but in one do-block we can only have a single
--       monad, i.e. IOThrowsError.
--
--       TO UNDERSTAND liftIO -- what are restriction

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var =
  do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val =
  do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting unbound variable" var)
          (liftIO . (flip writeIORef val))
          (lookup var env)
    return val -- just for convenience

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val =
  do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar envRef var val
       else liftIO $ do
            valRef <- newIORef val
            env    <- readIORef envRef
            writeIORef envRef ((var, valRef):env)
            return val

-- Error-free code!
--
-- The whole Monad pipeline is IO Env (as it must be).
--
-- newIORef :: a -> IO (IORef a)
--
-- The inner function type signatures are
--
-- addBinding :: (String, LispVal) -> IORef (String, IORef LispVal)
--               (var   , val)
-- extendEnv  :: [(String, LispVal)] -> IORef [(String, IORef LispVal)] -> Env
--               bindings               env
--
-- However, recall Env = IORef [(String, IORef LispVal)]
--
-- We need to do the following:
-- 1. Convert (String, LispVal) bindings to (String, IORef LispVal) to make
--    mutable.
--
-- 2. Take the current value of env, and add the new bindings.
--
--    mapM :: (a -> m b) -> [a] -> m [b]
--    (mapM addBinding bindings) in 
--
--    in context: a = (String, LispVal), m = IORef, b = (String, IORef LispVal)
--    therefore, return is IORef [(String, IORef LispVal)], i.e. Env.
--
--    liftM :: (Monad m) => (a1 -> r) -> m a1 -> m r
--    liftM (++ env) envRet
--
--    in context: a1 -> r = [(String, IORef LispVal)] -> [(String, IORef LispVal)] 
--    (list concat), so return is Env.
--
--    Therefore, extendEnv returns Env 
--
-- 3. Modify env to point to this value with the new bindings.
--
--    (readIORef envRef >>= extendEnv bindings) is an IO Env.
--    The final >>= applies newIORef to the naked Env. But its the newIORef
--    constructor, so its IO Env once again.
--
-- That wasn't so confusing. Just remember that do block = Monad pipeline, and in
-- one monad pipeline, you can have only one outer monad. (But the type parameter
-- of the monad can change, on >>= calls.)
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
  readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, val)  = do ref <- newIORef val
                                    return (var, ref)

-----------------------------------------------------------------------------
-- REPL
-----------------------------------------------------------------------------

-- To show user the prompt
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- Get rid of error. We are guaranteed only to call this when we have a
-- Right branch; see runIOThrows action.
-- TODO: More structured form of assertion?
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _           = error "I should never have been called!"

-- General pontification on monads
--
-- Monads are just a computational pattern that obeys an algebraic
-- structure, namely the >> and >>=, or threading, functions. The
-- threading functions enforce a _sequential_ pipeline of what are
-- essentially function compositions of the monad. The monad itself
-- (the wrapper) is threaded through each function, while the wrapped
-- value is only threaded through the >>= (<-) functions.
--
-- The composition of >> and >>= functions (left-associative), or
-- equivalently, a do block, must take the type signature m a -> m b.
-- That is, no value can escape from a monad in a function that is
-- composed from >> and >>=.
--
-- But if we know the structure of the monad m, we _can_ write a function
-- to escape a value from its monad. After all, a monad is just another
-- ADT. This is seen in our extractValue function. Of course, when we
-- start working with the concrete representation of a monad, we are now
-- responsible for its semantics. In this case, trapError ensures that
-- the Right branch is populated. But this is a bit of a hack because
-- our overall computation is String -> String, which allows this in-line
-- reporting.
--
-- In a future version, we could just use Left for the handler and
-- write out-of-line exception handling code elsewhere (e.g. write a
-- different extractValue, the left case being the equivalently of a
-- catch block).

-- class Monad m => MonadError e m | m -> e where
--   throwError :: e -> m a
--   catchError :: m a -> (e -> m a) -> m a
--
-- The documentation says "The strategy of combining computations
-- that can throw exceptions by bypassing bound functions from the
-- point an exception is thrown to the point that it is handled."
--
-- Note the (e -> m a) -> m a part of the type signature. This means
-- the error handler must return a monad of the same type as the outer
-- computation, i.e. error signaling must be done in line. We use
-- String for both results and errors, so this is fine.
--
-- Actually, this is not true: because the return type is m a, the error
-- handler is free just to note the error, e.g. f `catchError` Left, in
-- the example.
--
-- The point is not to register a mandatory exception handler, but just
-- to stop the subsequent computations. Our use case is actually a bit
-- of a hack.
--
-- In context, e = String, a = String as well (since the overall eval
-- pipeline is String -> String; we can ignore the intermediate
-- LispVal) from the perspective of this function. All manipulation will
-- be threaded into a big monad pipeline, and of course intermediate
-- values of the monad pipeline can wrap different types.

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

-- "get rid of" the ThrowsError monad. That is, regardless of which execution
-- path the monad pipeline takes, return an IO String monad (so in any case
-- we get something to display).
--
-- runErrorT :: m (Either e a)
--
-- Let's analyze: e = LispError, m = IO, a = String. So runErrorT constructs a
-- IO (Either LispError String), i.e. IO (ThrowsError String).
--
-- From the documentation, "runErrorT removes the ErrorT wrapper" -- think of it
-- as a form of _nominal destructuring_.
--
-- Let's take this step by step:
--
-- action                (IOThrowsError String)
-- e1 = trapError action (IOThrowsError String)
--                       Normal path:    e1 contains the string result expression.
--                       Exception path: e1 contains the error message.
-- e2 = runErrorT e1     (IO (Either LispError String), i.e. IOThrowsError String, i.e. _deconstructed_ ErrorT String IO String, so we need runErrorT to access)
-- e3 = return . extractValue ((Either LispError String) -> IO String, i.e.
--                             (ThrowsError String) -> IO String)
-- e4 = e2 >>= e3        (IO String)
--
-- EDIT: Removed dependence on string.
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- showResOrErr :: (Show e, MonadError e m) => m LispVal -> String
--
-- Let's take this step by step:
--
-- e0                    (IOThrowsError e)
-- e1 = liftM show e0    (IOThrowsError String)
-- e1 = runIOThrows e1   (IO String)
showResOrErr :: (Show e) => IOThrowsError e -> IO String
showResOrErr = runIOThrows . liftM show

-- Let's take this step by step:
--
-- expr                  (String)
-- e1 = readExpr expr    (ThrowsError LispVal)
-- e2 = liftThrowsToIO e1    (IOThrowsError LispVal)
-- e3 = e2 >>= eval env  (IOThrowsError LispVal, because eval env is LispVal -> IOThrowsError LispVal)
-- e4 = showResOrErr e3  (IO String)

parseExprString, evalExprString :: Env -> String -> IO String
parseExprString _        = showResOrErr . liftThrowsToIO . readExpr
evalExprString  env expr = showResOrErr $ (liftThrowsToIO $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
  parsed <- parseExprString env expr
  evaled <- evalExprString env expr
  putStrLn $ "PARSE: " ++ parsed
  putStrLn $ "EVAL:  " ++ evaled

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

-- You can enter the IO monad, but you can never leave.
-- Other monads, you can only leave if allowed to.

stdlibFilename = "stdlib.scm"
loadLib :: Env -> String -> IOThrowsError LispVal
loadLib env filename = eval env (List [Atom "load", String filename])

-- Evaluate a file with given arguments.
--
-- Recall: In a do block, we can use an "imperative do" form where we don't
-- write _in_. It is equivalent to writing the in and indenting.
--
-- Remember, a do-block is essentially CPS, there is a correspondence b/t
-- sequece and next continuation. (To clarify. Basically, what you wnt to
-- say is that in CPS, you have no expression outside the continuation, e.g.
-- the form is (f (g (h ...))) <- a bunch of right). So we may as well
-- omit the ), which amounts to the in indentation.
--
-- (Wow, that was unclear. Monads hurt your brain.)
runOne :: [String] -> IO ()
runOne args = do
  let argsGlob    = [("args", List $ map String $ tail args)]
      loadExpr    = (List [Atom "load", String (args !! 0)])

  -- Append a variable "args" to the environment, which contains the command
  -- line arguments (as strings).
  env <- primitiveBindings >>= flip bindVars argsGlob

  -- Construct (but do not run) a monad representing an errorable evaluation
  -- of loadExpr in envrionment env.
  let loadLibEnv  = liftM show $ loadLib env stdlibFilename
      evalAndShow = liftM show $ eval env loadExpr

  -- BEGIN ERRORABLE PART.
  -- Note: We should really fine-tune error handling to distinguish between
  -- error in the library vs. others.
  --
  -- That's just a matter of not using trapError, but just catchError
  -- directly, with a richer exception hierarchy. But that's just
  -- engineering; you already know the monad stuff now!
  --
  -- Load the standard library and print the result of evaluating
  -- loadExpr to stderr.
  --
  -- If an error resulted, an error is printed.
  -- If no error resulted, the return value of the last statement of the
  -- program is printed.
  --
  -- stdout only occurs on program output, accomplished via calling (write).
  runIOThrows (loadLibEnv >> evalAndShow) >>= hPutStrLn stderr

-- TODO: Load stdlib, but get more proper exception catching first.
runRepl :: IO ()
runRepl = primitiveBindings 
        >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do args <- getArgs
          if null args
             then runRepl
             else runOne args

