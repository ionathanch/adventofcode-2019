{-# LANGUAGE MonadFailDesugaring #-}

module IntCode
    ( IntCodeState(..)
    , initState
    , getProgram
    , execUntilHalt
    ) where

import Data.List (sort)
import Data.IntMap (IntMap, Key, findWithDefault, insert, fromList, toList)
import Control.Monad.State (State, get, put, modify, runState)

type Program = IntMap Int
type Pointer = Int
type Base = Int

data IntCodeState = IntCodeState {
    program :: Program,
    pointer :: Pointer,
    base :: Base,
    inputs :: [Int]
}

data Result = Output Int | Continue | Halted

(%) = mod
(//) = div

(!) :: Program -> Key -> Int
infixl 5 ! -- just below arithmetic
(!) = flip $ findWithDefault 0

noop :: State IntCodeState ()
noop = return ()

updateProgram :: Key -> Int -> State IntCodeState ()
updateProgram key val =
    modify $ \st -> st { program = insert key val (program st) }

updatePointer :: Pointer -> State IntCodeState ()
updatePointer ptr =
    modify $ \st -> st { pointer = ptr }

updateBase :: Base -> State IntCodeState ()
updateBase delta =
    modify $ \st -> st { base = base st + delta }

readInput :: State IntCodeState Int
readInput = do
    st <- get
    put st { inputs = tail $ inputs st }
    return . head $ inputs st

exec :: State IntCodeState Result
exec = do
    IntCodeState program pointer base _ <- get
    let instr = program ! pointer
        opcode = instr % 100
        ptrInc
            | opcode `elem` [1, 2, 7, 8] = 4
            | opcode `elem` [5, 6]       = 3
            | opcode `elem` [3, 4, 9]    = 2
            | opcode == 99               = 1
        nextPtr = pointer + ptrInc
        getLoc i mode = case mode of
            0 -> program ! pointer + i
            1 -> pointer + i
            2 -> base + (program ! pointer + i)
        loc1 = getLoc 1 $ instr // 100   % 10
        loc2 = getLoc 2 $ instr // 1000  % 10
        loc3 = getLoc 3 $ instr // 10000 % 10
        val1 = program ! loc1
        val2 = program ! loc2
        val3 = program ! loc3
    updatePointer nextPtr
    case opcode of
        1 -> updateProgram loc3 (val1 + val2) >> return Continue
        2 -> updateProgram loc3 (val1 * val2) >> return Continue
        3 -> readInput >>= updateProgram loc1 >> return Continue
        4 -> return $ Output val1
        5 -> (if (val1 /= 0) then updatePointer val2 else noop)   >> return Continue
        6 -> (if (val1 == 0) then updatePointer val2 else noop)   >> return Continue
        7 -> updateProgram loc3 (if (val1 <  val2) then 1 else 0) >> return Continue
        8 -> updateProgram loc3 (if (val1 == val2) then 1 else 0) >> return Continue
        9 -> updateBase val1 >> return Continue
        99 -> return Halted

initState :: [Int] -> IntCodeState
initState list = IntCodeState (fromList $ zip [0..] list) 0 0 []

getProgram :: IntCodeState -> [Int]
getProgram = map snd . sort . toList . program

execUntilHalt :: IntCodeState -> IntCodeState
execUntilHalt st =
    let (result, st') = runState exec st
    in case result of
        Halted -> st'
        _ -> execUntilHalt st'