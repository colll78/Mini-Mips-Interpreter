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

{-|
   The 'runByteCode' function interprets bytecode.
   It takes one argument, of type '[ByteCode]'
-}
runByteCode :: [ByteCode] -> IO (Either String Integer)
runByteCode program = runExceptT $ evalStateT (eval program) (Environment {insts = program, stack = [], vlookups = Map.empty})

{-
Pure version
runByteCode :: [ByteCode] -> Integer
runByteCode program = evalState (eval program) (Environment {insts = program, stack = [], vlookups = Map.empty})
-}


main :: IO ()
main = do
    -- program file: The path of the file containing the bytecode that we want to interpret (ie: "loop_test.txt")
    (program_file:rest) <- getArgs
       
    file_contents <- lines <$> readFile program_file
    let parsed_result = rights (map readExpr file_contents)

    result <- runByteCode parsed_result
    case result of
            Left err ->  print err
            Right val -> print val 

