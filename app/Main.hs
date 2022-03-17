{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import System.Environment
import Data.Either
import System.IO
import Parser
import Interpreter
import Data.Map qualified as Map
-- import Control.Monad.Trans.State
import Control.Monad.State ( StateT(runStateT), MonadState(put, get), liftIO, evalState, evalStateT, State(..), modify, execState, runState)
import Control.Monad.Except

compileCode :: [String] -> IO()
compileCode code = if null code
                    then putStrLn "Done"
                    else do
                            putStrLn (head code)
                            compileCode (tail code)

{-
runByteCode :: [ByteCode] -> Integer
runByteCode program = evalState (eval program) (Environment {insts = program, stack = [], vlookups = Map.empty})
-}
runByteCode :: [ByteCode] -> IO (Either String Integer)
runByteCode program = runExceptT $ evalStateT (eval program) (Environment {insts = program, stack = [], vlookups = Map.empty})

main :: IO ()
main = do
    (program_file:rest) <- getArgs
   -- print (readExpr program_file)
    
    --let (Right val) = (readExpr program_file)
    --print val
     
    file_contents <- lines <$> readFile program_file
    -- print (map readExpr file_contents)
    let parsed_result = rights (map readExpr file_contents)

    -- print parsed_result
    -- result <- runExceptT $ runStateT (eval parsed_result) (Environment {insts = parsed_result, stack = [], vlookups = Map.empty})
    result <- runByteCode parsed_result
    case result of
            Left err ->  print err
            Right val -> print val 


    -- print $ runState (eval parsed_result) (Environment {insts = parsed_result, stack = [], vlookups = Map.empty})
    -- print (runByteCode parsed_result)