import Control.Monad
import Text.ParserCombinators.Parsec
import System.Environment


spacesCustom :: Parser ()
spacesCustom = skipMany1 space

{- 

    State Monad: get
    do { (a,b) <- get; return a }   ===   get >>= \(a,b) -> return a

    State Monad: put
    do { put (1,5) ; (a,b) <- get; return a }   ===   put (1,5) >> get >>= \(a,b) -> return a 

    parseOp :: Parser BCValue
    parseOp = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let v = first:rest
              return $ case v of
                         "#t" -> BCBool True
                         "#f" -> BCBool False
                         _    -> BCBool False
-}