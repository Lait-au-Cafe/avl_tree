import TreeUtils
import System.Environment
import Data.Time

main = do
-- t1 <- getCurrentTime
 args <- getArgs
 if (length args) /= 1
 then putStrLn "Argument Error. "
 else do
  let
   l = read (head args) :: Int
  tl <- testTree l l
  let
   ave = average tl
   var = variance tl
  putStrLn ("Average:" ++ (show ave))
  putStrLn ("Variance:" ++ (show var))
--  t2 <- getCurrentTime
--  putStrLn (show (diffUTCTime t2 t1))