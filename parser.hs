import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
		| List [LispVal]
		| DottedList [LispVal] LispVal
		| Number Integer
		| String String
		| Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

main :: IO ()
main = do
	args <- getArgs
	putStrLn (readExpr (args !! 0))

-- bind operator >>, cascade computations in order
-- example: (parse >> symbol)
readExpr input = case parse parseExpr "list" input of
	-- Either structure. Left: error, Right: normal value
	Left err -> "No match: " ++ show err
	Right val -> "Found value"


parseExpr :: Parser LispVal
parseExpr = parseAtom
	<|> parseString
	<|> parseNumber

-- Custom space Parse
spaces :: Parser ()
spaces = skipMany1 space -- Pasing an action 'space' to the 'skipMany1' action

parseString :: Parser LispVal
parseString = do
		char '"'
		x <- many (noneOf "\"")
		char '"'
		return $ String x

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = first:rest
	return $ case atom of
		"#t" -> Bool True
		"#f" -> Bool False
		_ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
