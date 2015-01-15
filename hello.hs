module Main where
import System.Environment

-- `IO ()` is a monad type with "unit type ()". We specified that
-- main returns an IO () object. This can be accomplished in two ways:
-- 1. Lift an ordinary value into the IO monad, using the Monad's return
--    (which is essentially boxing).
-- 2. Combine existing IO actions, using a do-block.
--
-- For reference, we have
-- class  Monad m  where
--     (>>=)  :: m a -> (a -> m b) -> m b
--     (>>)   :: m a -> m b -> m b
--     return :: a -> m a
--     fail   :: String -> m a
-- 
--         -- Minimal complete definition:
--         --      (>>=), return
--     m >> k  =  m >>= \_ -> k
--     fail s  = error s
--
-- We have the following type signatures:
--  getArgs  :: IO [String]
--  putStrLn :: String -> IO ()
--
-- A do-block contains two types of actions:
-- 1. name <- action1
-- 2. action2
--
-- These are just sugar for >>= and >> respectively.

-- Sugar free verion for demonstration.

saccharineMain :: IO ()
saccharineMain = getArgs >>= (\x -> putStrLn ("Hello saccharine, " ++ x !! 0))

-- Think of: getArgs is a IO [String] monad. The anonymous function takes
-- [String] but returns IO (). The "threading" operator takes the IO [String]
-- monad from the left and the [String] -> IO () function from the right and
-- returns IO (), exactly what main needs.
--
-- Of course, for these semantics to make sense, the >>= and >> operators must
-- "accumulate" state in their return values, which is finally displayed in
-- main.

-- TODO: Figure out why the prompt shows up before input for putStrLn but
-- not for putStr. According to Prelude, putStrLn is just
-- putStrLn s = do putStr s
--                 putStr "\n"
--
-- Methinks putStr "\n" flushes the buffer.

saccharinePrompt :: IO ()
saccharinePrompt =
  putStrLn "Enter your name" >>
  getLine >>= (\name -> putStrLn ("Hello saccharinePrompt, " ++ name))

saccharineReadShow = do
  putStrLn "Enter a number"
  n1 <- getLine
  putStrLn "Enter another number"
  n2 <- getLine
  putStrLn $ "Your sum is " ++ (show $ read n1 + read n2)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0)
  putStrLn "Enter your name "
  name <- getLine
  putStrLn ("Hello, " ++ name)
  saccharineMain
  saccharinePrompt
  saccharineReadShow

