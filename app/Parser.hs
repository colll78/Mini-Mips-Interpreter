module Parser(
    OpCode(..),
    BCValue(..),
    ByteCode(..),
    readExpr,
    parseExpr
)
where

import Control.Monad
import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import Prelude hiding (LT, GT)

data OpCode = ADD
            | MULTIPLY
            | GT
            | GEQ
            | LT
            | LEQ
            | EQUAL
            deriving(Show, Eq)

data BCValue = Variable String
            | BCCharacter Char
            | BCBool Bool
            deriving (Eq)

data ByteCode = Load Integer -- Load from literal
            | ALoad BCValue -- Load from local variable 
            | Store BCValue
            | BinaryOp OpCode
            | Loop Integer
            | Label String
            | Jump OpCode String 
            | Channel String
            | SendChannel
            | ReceiveChannel
            | Function String
            | Call String
            | Spawn
            | Return
            deriving(Eq)

showVal :: BCValue -> String
showVal (Variable contents) = "'" ++ contents ++ "'"
showVal (BCBool True) = "#t"
showVal (BCBool False) = "#f"
showVal (BCCharacter v) = show v

showByteCode :: ByteCode -> String
showByteCode (ALoad val) =  "Load Variable: " ++ show val
showByteCode (Load val) =  "Load Number: " ++ show val
showByteCode (Store var) =  "Store: " ++ show var
showByteCode (BinaryOp binOp) = show binOp
showByteCode (Loop val) =  "Loop: " ++ show val
showByteCode (Jump cmp var) =  "Jump to " ++ show var ++ " if " ++ show cmp
showByteCode (Label var) =  "Label: " ++ show var 
showByteCode (Channel chan) =  "Channel: " ++ show chan 
showByteCode SendChannel =  "Send Channel" 
showByteCode ReceiveChannel =  "Receive Channel" 
showByteCode (Function var) = "Function: " ++ show var
showByteCode (Call var) = "Call function: " ++ show var
showByteCode Spawn = "Spawn"
showByteCode Return = "Return"



instance Show BCValue where show = showVal
instance Show ByteCode where show = showByteCode

parseVariable :: Parser BCValue
parseVariable = do
                char '\''
                x <- many (noneOf "\'")
                char '\''
                return $ Variable x

parseNumber:: Parser Integer
parseNumber = read <$> many1 digit

lexeme ::Parser a -> Parser a
lexeme p = p <* spaces

identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

keyword :: String -> Parser String
keyword k = try $ do
    i <- identifier
    guard (i == k)
    return k

parseLoadInst :: Parser ByteCode
parseLoadInst = do
                    first <- keyword "LOAD_VAL"
                    Load <$> parseNumber

parseALoadInst :: Parser ByteCode
parseALoadInst = do
                    first <- keyword "READ_VAR"
                    ALoad <$> parseVariable

parseStoreInst :: Parser ByteCode
parseStoreInst = do
                    first <- keyword "WRITE_VAR"
                    Store <$> parseVariable

parseBinaryOpInst :: Parser ByteCode
parseBinaryOpInst = choice [BinaryOp ADD <$ keyword "ADD"
                    ,BinaryOp MULTIPLY <$ keyword "MULTIPLY"]

parseLoopInst :: Parser ByteCode
parseLoopInst = do
                    first <- keyword "LOOP"
                    Loop <$> parseNumber

parseReturnInst :: Parser ByteCode
parseReturnInst = do
                    first <- keyword "RETURN_VALUE"
                    return Return

parseJumpInst :: Parser ByteCode
parseJumpInst = do
                    first <- keyword "JUMP"
                    cmp <- keyword "LT" <|> keyword "GT" <|> keyword "GEQ" <|> keyword "EQUAL" <|> keyword "LEQ"
                    label <- identifier
                    return $ case cmp of 
                        "LT" -> Jump LT label
                        "GT" -> Jump GT label
                        "GEQ" -> Jump GEQ label
                        "EQUAL" -> Jump EQUAL label
                        "LEQ" -> Jump LEQ label
    
parseLabelInst :: Parser ByteCode
parseLabelInst = do
                    first <- identifier
                    char ':'
                    return $ Label first

parseMkChanInst :: Parser ByteCode
parseMkChanInst = keyword "CHANNEL" >> (identifier >>= \chan -> return (Channel chan))

parseSendChannelInst :: Parser ByteCode
parseSendChannelInst = keyword "SEND_CHANNEL" >> return SendChannel

parseReceiveChannelInst :: Parser ByteCode
parseReceiveChannelInst = keyword "RECV_CHANNEL" >> return ReceiveChannel

parseFunctionInst :: Parser ByteCode 
parseFunctionInst = keyword "FUNCTION" >> (identifier >>= \fid -> return (Function fid))

parseCallInst :: Parser ByteCode
parseCallInst = keyword "CALL" >>  (identifier >>= \fid -> return (Call fid))

parseSpawnInst :: Parser ByteCode
parseSpawnInst = keyword "SPAWN" >> return Spawn

parseExpr :: Parser ByteCode
parseExpr = parseLoadInst
            <|> parseALoadInst
            <|> parseStoreInst
            <|> parseBinaryOpInst
            <|> parseLoopInst
            <|> parseReturnInst
            <|> parseJumpInst
            <|> parseMkChanInst
            <|> parseSendChannelInst
            <|> parseReceiveChannelInst
            <|> parseFunctionInst
            <|> parseCallInst
            <|> parseLabelInst

readExpr :: String -> Either String ByteCode
readExpr input = case parse parseExpr "Mist" input of
    Left err ->  Left $ show err
    Right val -> Right val
