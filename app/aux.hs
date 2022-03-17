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
    
    
    let result = (`execState` initialEnvironment) $ do
            push 10
            push 20
            val <- pop
            store (Variable "x") val
            nval <- aload (Variable "x")
            push nval
            add
            get

    let result2 = (`evalState` initialEnvironment) $ do
            push 10
            push 20
            val <- pop
            store (Variable "x") val
            nval <- aload (Variable "x")
            push nval
            add
            pop

    let result3 = (`evalState` initialEnvironment) $ do
            push 10
            push 20
            val <- pop
            store (Variable "x") val
            nval <- aload (Variable "x")
            push nval

    let test_program = [Load 1, Store (Variable "x"), Load 2, Store (Variable "y"), ALoad (Variable "x"), Load 1, BinaryOp ADD, ALoad (Variable "y"), BinaryOp MULTIPLY, Return] 

    let result4 = runState (eval test_program) initialEnvironment
    print result         
    print result2
    print result3
    print result4

-}