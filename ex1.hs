import TreeUtils
import System.Environment

main = do
 args <- getArgs
 if (length args) /= 1
 then putStrLn "Argument Error. "
 else do
  ar <- genRandArray (read (head args))
  putStrLn (show ar)