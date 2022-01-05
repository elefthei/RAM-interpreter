{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import           Lens.Simple
import           Data.Map                hiding ( map
                                                , filter
                                                , find
                                                )
import           Data.List               hiding ( lookup
                                                , insert
                                                )
import           Prelude                 hiding ( Char
                                                , lookup
                                                )
import           Control.Monad                  ( )
import           Control.Monad.State.Strict
import           Data.Maybe                     ( fromJust )
import           Control.Monad.Extra

data Opcode = Add | Tail | Clr | Assign | Gotoa | Gotob | Jmpa | Jmpb | Continue deriving (Show, Enum)
type Instruction = (Int, Int, Opcode, Char, Int)
type Char = Int

newtype RamState = RamState { _registers :: Map Int [Char] }

-- Ram Monad
newtype Ram a = Ram (StateT RamState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadState RamState)

$(makeLenses ''RamState)

runRam :: Ram a -> RamState -> IO (a, RamState)
runRam (Ram s) = runStateT s

evalRam :: Ram a -> RamState -> IO a
evalRam r s = fst <$> runRam r s

type Program = [Instruction]

reg_update :: Int -> ([Char] -> [Char]) -> Ram ()
reg_update reg f = modify . over registers $ adjust f reg

reg_get :: Int -> Ram (Maybe [Char])
reg_get reg = do
  state <- get
  return . lookup reg $ view registers state

interpret :: Program -> Int -> Ram (Either Int ())
interpret prog ip = do
  regs <- toList . view registers <$> get
  liftIO . putStrLn $ "IP: " ++ show ip
  liftIO . putStrLn $ "Instruction: " ++ show (prog !! ip)
  liftIO . putStrLn . unlines $ ("Registers" : map show regs)
  case prog !! ip of
    (_, _, Add, c, r) -> do
      reg_update r (++ [c])
      step
    (_, _, Tail, _, r) -> do
      reg_update r (tail)
      step
    (_, _, Clr, _, r) -> do
      reg_update r (const [])
      step
    (_, d, Assign, _, s) -> do
      Just v <- reg_get s
      reg_update d (const v)
      step
    (_, _, Gotoa, _, l) -> goto l above
    (_, _, Gotob, _, l) -> goto l below
    (_, r, Jmpa , c, l) -> do
      Just v <- reg_get r
      case v of
        (h : _) | h == c -> goto l above
        (_)              -> step
    (_, r, Jmpb, c, l) -> do
      Just v <- reg_get r
      case v of
        (h : _) | h == c -> goto l below
        (_)              -> step
    (_, _, Continue, _, _) -> return $ Right ()
 where
  step  = return . Left $ ip + 1
  above = reverse . filter (\p -> fst p < ip)
  below = filter (\p -> fst p > ip)
  goto line mod =
    return
      . Left
      . fst
      . fromJust
      . find (\(_, (l, _, _, _, _)) -> l == line)
      . mod
      . zip [0 ..]
      $ prog

eval :: Int -> [[Char]] -> [(Int, Int, Int, Int, Int)] -> Ram [Char]
eval nregs inputs instrs = do
  let prog = fmap parse instrs                                             -- Create a program
  forM_ [1 .. nregs]        (\r -> modify . over registers $ insert r [])  -- Set registers to []
  forM_ (zip [1 ..] inputs) (\(n, inp) -> reg_update n (const inp))        -- Set input registers starting at 1
  loopM (interpret prog) 0                                                 -- Run the interpreter
  fromJust <$> reg_get 1                                                   -- Get the 1st register
  where parse (l1, r1, op, r2, l2) = (l1, r1, toEnum (op - 1), r2, l2)

test_concat =
  [ (-1, 3, 4, 0, 1)
  , (-1, 4, 4, 0, 2)
  , (0 , 4, 8, 1, 1)
  , (-1, 4, 8, 2, 2)
  , (-1, 0, 6, 0, 3)
  , (1 , 0, 1, 1, 3)
  , (-1, 0, 2, 0, 4)
  , (-1, 0, 5, 0, 0)
  , (2 , 0, 1, 2, 3)
  , (-1, 0, 2, 0, 4)
  , (-1, 0, 5, 0, 0)
  , (3 , 1, 4, 0, 3)
  , (-1, 0, 9, 0, 0)
  ]

test_concat_inp = [[1, 2, 1, 2], [2, 1, 2, 1, 1, 2, 2]]
test_concat_p = 4

main :: IO ()
main = do
  putStrLn "Program"
  putStrLn . unlines . map show $ test_concat
  putStrLn "Inputs"
  putStrLn . unlines . map show $ test_concat_inp
  result <- evalRam (eval test_concat_p test_concat_inp test_concat)
                    (RamState empty)
  putStrLn "Result"
  putStrLn . show $ result
