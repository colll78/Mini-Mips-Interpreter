{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}


module Interpreter(
    Environment(..),
    pop,
    push,
    store,
    aload,
    pushpop,
    eval,
    initialEnvironment
)
where

import Control.Concurrent (forkIO, threadDelay, ThreadId)
-- import Control.Concurrent.Forkable
import Control.Monad.State ( StateT(runStateT), MonadState(put, get), evalStateT, State(..), modify)
import Control.Monad.Except
import Data.Map qualified as Map
import Data.List
import Control.Concurrent.Async
import Data.Either
import Parser


data Environment = Environment { insts ::  [ByteCode]
                                ,stack :: [Integer]
                                ,vlookups :: Map.Map String Integer
                                } deriving (Eq, Show)


initialEnvironment = Environment {insts = [], stack = [], vlookups = Map.empty}
errEnvironment = Environment {insts = [], stack = [-1], vlookups = Map.empty}

pop_n :: Int -> StateT Environment (ExceptT String IO) [Integer]
pop_n n = do
    Environment {..} <- get
    if length stack > n then
        let (popped, rest) = genericSplitAt n stack in
            put Environment {insts=insts, stack = rest, ..} >> return popped
        else throwError "Error: attempt to pop empty stack "

pop :: StateT Environment (ExceptT String IO) Integer
pop = do
    Environment {..} <- get
    case stack of
        [] -> throwError $ "Error: attempt to pop empty stack "
        (x : xs) -> put Environment {insts=insts, stack = xs, ..} >> return x

push :: Integer -> StateT Environment (ExceptT String IO) ()
push val = modify $ \Environment{insts=instructions, stack = xs, vlookups=looks} -> (Environment {insts=instructions, stack = val : xs, vlookups=looks})

pushpop::Integer -> StateT Environment (ExceptT String IO) Integer
pushpop val = push val >> pop

aload :: BCValue -> StateT Environment (ExceptT String IO) Integer
aload var = let Variable vname = var in get >>= \Environment{..} -> maybe
              (throwError
                 $ "Error: attempt to read unstored variable " ++ show vname)
              return (Map.lookup vname vlookups)

store :: BCValue -> Integer -> StateT Environment (ExceptT String IO) ()
store var val = let Variable vname = var in modify $
    \Environment{insts=instructions, stack = st, vlookups=ltable} -> (Environment {insts=instructions, stack = st, vlookups=Map.insert vname val ltable})

opCode :: OpCode -> StateT Environment (ExceptT String IO) ()
opCode op = do
    val1 <- pop
    val2 <- pop
    let op_fn = case op of
                ADD -> (+)
                MULTIPLY -> (*)
                __ -> (-)
    push (op_fn val1 val2)

compareOp :: OpCode -> StateT Environment (ExceptT String IO) Bool
compareOp op = do
    val1 <- pop
    val2 <- pop
    let cmpr = case op of
            Parser.LT -> (<)
            Parser.GT -> (>)
            EQUAL -> (==)
            GEQ -> (>=)
            LEQ -> (<=)
    return (cmpr val2 val1)

eval :: [ByteCode] -> StateT Environment (ExceptT String IO) Integer
eval program = do
    let inst = head program
    case inst of
        Return -> pop
        Jump cmp_op label -> do
            result <- compareOp cmp_op
            if result
                then do
                    Environment {..} <- get
                    let loop_insts = dropWhile (\inst -> inst /= Label label) insts
                    when (null loop_insts) (throwError $ "Error: attempt to jump to nonexistent label " ++ label)
                    eval (tail loop_insts)
                else eval (tail program)
        Call f_start -> do
            Environment {..} <- get
            let loop_insts = dropWhile (\inst -> inst /= Function f_start) insts
            when (null loop_insts) (throwError $ "Error: call to nonexistent function " ++ f_start)
            fun_result <- liftIO $ runExceptT (evalStateT (eval loop_insts)  Environment {..})
            (case fun_result of
                    Left err -> throwError ("Error in function " ++ f_start)
                    Right val -> push val)
            eval (tail program)
        _ -> (case inst of
                    Load val -> push val
                    ALoad var -> aload var >>= \vval -> push vval
                    Label var -> return ()
                    Function var -> return ()
                    Store var -> do
                        vval <- pop
                        store var vval
                    BinaryOp opc -> opCode opc
                    Call var -> return()
                    _ -> return ()) >> eval (tail program)

   