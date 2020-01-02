module Parser where

import Lib
import ADT
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
    
type Parser = Parsec Void String
    
sc :: Parser ()
sc = L.space
    space1                        
    (L.skipLineComment ";")       
    (L.skipBlockComment "#|" "|#")
    
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
    
symbol :: String -> Parser String
symbol = L.symbol sc
    
specialChar :: Parser Char
specialChar = oneOf "#?!.+-*/<=>:$%^&_~"
    
noSpace :: Parser ()
noSpace = return ()
    
parseNum :: Parser Expr
parseNum = Num <$> try (L.signed noSpace $ lexeme L.decimal)
    
parseName :: Parser Expr
parseName = Name <$> (lexeme $ (:) <$> (letterChar <|> specialChar) <*> many (letterChar <|> digitChar <|> specialChar))
    
parseString :: Parser Expr
parseString = do
    name <- (lexeme $ between (char '"') (char '"') $ many $ noneOf "\"")
    return $ Name ("\"" ++ name ++ "\"")
    
parseQuote :: Parser Expr
parseQuote = do
    symbol "'"
    qt <- parseExpr
    return $ List [Name "quote", qt]
            
parseQuasiquote :: Parser Expr
parseQuasiquote = do
    symbol "`"
    quasiqt <- parseExpr
    return $ List [Name "quasiquote", quasiqt]
    
parseUnquote :: Parser Expr
parseUnquote = do
    symbol ","
    unqt <- parseExpr
    return $ List [Name "unquote", unqt]
    
parseList :: Parser Expr
parseList = List <$> (lexeme $ between (symbol "(") (symbol ")") $ many parseExpr)
    
parseExpr :: Parser Expr
parseExpr = parseNum 
        <|> parseName 
        <|> parseString
        <|> parseQuote
        <|> parseQuasiquote
        <|> parseUnquote 
        <|> parseList