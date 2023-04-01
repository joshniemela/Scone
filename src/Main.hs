{-# LANGUAGE OverloadedStrings #-}
import Data.Void
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Except




-- Define all types for the language
data SExpr = Atom Text
           | List [SExpr]
           | Nil
           | String Text
           | Markup Text
           | Number Integer
           | Bool Bool
           | Function Fn

-- Define every possible type in the AST for the formatter, this includes extra types for the formatter such as Comment
data AST = Comment Text
         | SExpr SExpr
         | Newline
         deriving (Show, Eq)

type Parser = Parsec Void Text

parens = between (char '(' <* space) (space <* char ')')

-- These are the characters that are allowed at the start of an atom
allowedFirstChars :: [Char]
allowedFirstChars = ['a'..'z'] ++ ['A'..'Z'] ++ "+-*/$%&<=>?^_~"

allowedFirst :: Parser Char
allowedFirst = oneOf allowedFirstChars


-- Basic types
parseNumber :: Parser SExpr
parseNumber = Number <$> L.decimal

parseAtom :: Parser SExpr
parseAtom = do
    first <- allowedFirst
    rest <- many $ oneOf $ allowedFirstChars ++ ['0'..'9']
    return $ Atom $ T.pack $ first : rest

parseList :: Parser SExpr
parseList = List <$> parens (parseSExpr `sepEndBy` space)

parseSExpr :: Parser SExpr
parseSExpr = do
    expr <- choice [parseQuoted, parseNumber, parseAtom, parseList, parseMarkupBlock]
    lookAhead $ choice [space, eof]
    -- If the code is list of length 1  then return the first element
    case expr of
        List [x] -> return x
        List [] -> return $ Atom "nil"
        _ -> return expr

-- Parsers in markup mode
parseCode :: Parser SExpr
parseCode = char ':' >> parseSExpr

reservedChars :: [Char]
reservedChars = [':', ']', '[', '#', '(', ')', '\'', '"']

escapeChars :: Parser Char
escapeChars = do
    _ <- char '\\'
    oneOf reservedChars

-- TODO: leaves newline and whitespaces at the end
parseMarkup :: Parser SExpr
parseMarkup = do
    space
    contents <- manyTill (escapeChars <|> L.charLiteral) (lookAhead (oneOf reservedChars <|> (eof >> return ' ') ))
    -- Strip last spaces and newlines
    return $ Markup $ T.pack $ reverse $ dropWhile (\x -> x == ' ' || x == '\n') $ reverse contents

parseQuoted :: Parser SExpr
parseQuoted = do
    _ <- char '\''
    expr <- parseSExpr
    return $ List [Atom "quote", expr]


-- Parsers in code mode
parseMarkupBlock :: Parser SExpr
parseMarkupBlock = do
    _ <- char '['
    contents <- manyTill (parseCode <|> parseMarkup) (char ']')
    return $ Content contents

parseContent :: Parser SExpr
parseContent = do
    exprs <- manyTill (parseCode <|> parseMarkup) eof
    -- remove empty markup
    return $ Content $ filter (\x -> case x of
        Markup "" -> False
        _ -> True) exprs


primitives :: [(Text, [SExpr] -> ThrowsError SExpr)]
primitives = [
    ("+", numBinop (+)), 
    ("-", numBinop (-)), 
    ("*", numBinop (*)),
    ("/", numBinop div),
    ("mod", numBinop mod),
    ("quotient", numBinop quot),
    ("remainder", numBinop rem)
    ]


eval :: SExpr -> ThrowsError SExpr
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Markup _) = return val
eval (Comment _) = return $ Atom "nil"
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func

-- eval content should return a list of evaluated expressions and concat them to Markup separated by spaces
eval (Content exprs) = foldl1 (\x y -> do
    x' <- x
    y' <- y
    return $ Markup $ T.concat [markup x', " ", markup y']) $ map eval exprs
    where
        markup (Markup x) = x
        markup x = T.pack $ show x

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: Text -> [SExpr] -> ThrowsError SExpr
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" $ show func) ($ args) (lookup func primitives)


data Error = NumArgs Int [SExpr]
           | TypeMismatch String SExpr
           | BadSpecialForm String SExpr
           | NotFunction String String
           | UnboundVar String String
           | Default String

showError :: Error -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                    ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                         ++ ", found " ++ show found

unwordsList :: [SExpr] -> String
unwordsList = unwords . map show

instance Show Error where show = showError

type ThrowsError = Either Error

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val



numBinop :: (Int -> Int -> Int) -> [SExpr] -> ThrowsError SExpr
numBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
numBinop op singleVal@[] = throwError $ NumArgs 2 singleVal


unpackNum :: SExpr -> ThrowsError Int
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- Test random string
main :: IO ()
main = do
    content <- readFile "test.phl"
    case parse parseContent "test.phl" (T.pack content) of
        Left err -> putStrLn $ errorBundlePretty err
        Right expr -> print $ eval expr
