import TreeUtils
import System.Environment

data Results a = Value Int | Node a [Results a] deriving Show

main = do
 args <- getArgs
 if (length args) /= 2
  then putStrLn "Argument Error. "
  else do
   let n = read (args!!0)
   let k = read (args!!1)
   if 0 < n && n < 6
    then do
     re <- execute n k
     putStrLn (show (createHistogram re))
    else
     putStrLn "Input number must be less than 6 and larger than 0. "
-- 配列が少しでも長いとメモリを大量に食うので, 長さは最長でも5としている


-- リスト[1..n]と初期値を受け取って, [1..n]をシャッフルして得られる配列全体を格納できる木構造を作る関数
prepare :: (Eq a) => [a] -> [Results a]
prepare [] = [Value 0]
prepare xs = [Node x (prepare (filter (/= x) xs)) | x <- xs]


-- リストを受け取ってそれをprepareで作った木構造に格納する関数
applyResult :: Eq a => [a] -> [Results a] -> [Results a]
applyResult [] [Value val] = [Value (val+1)]
applyResult _  [Value val] = [Value val]
applyResult [] rs = rs
applyResult (x:xs) rs = 
 ((takeWhile (not . seive) rs) 
 ++ [Node x (applyResult xs $ getChildren $ head (filter seive rs))] 
 ++ (tail $ dropWhile (not . seive) rs))
  where seive = searchPath x

-- prepareで作った木構造にリストを入れる際に各ノードで次に進むべき子を判断する関数
searchPath :: Eq a => a -> Results a -> Bool
searchPath _ (Value _) = False
searchPath n (Node x rs)
 | x == n = True
 | otherwise = False

-- ある結果のIndexを返す
getIndex :: Results a -> Maybe a
getIndex (Value _) = Nothing
getIndex (Node x rs) = Just x

-- ある結果の子の結果のリストを返す
getChildren :: Results a -> [Results a]
getChildren (Value _) = []
getChildren rs@(Node x rss) = rss

-- 結果から頻度をリストとして取り出す
getResult :: [Results a] -> [Int]
getResult [Value val] = [val]
getResult rs = concat [getResult $ getChildren r | r <- rs]

-- 結果から頻度値に対応するリストを取り出す
getArrays :: [Results a] -> [[Maybe a]]
getArrays [] = [[]]
getArrays rs = concat [map ((:) (getIndex r)) (getArrays $ getChildren r) | r <- rs]

-- getArraysから得た結果からMaybeをはがす
procArrays :: [[Maybe a]] -> [[a]]
procArrays = map convArrays

-- procArraysの補助関数
convArrays :: [Maybe a] -> [a]
convArrays [] = []
convArrays (x:xs) = case x of
 Nothing -> convArrays xs
 Just t -> t:(convArrays xs)

 
-- 結果を格納した木構造をヒストグラムに変換する
createHistogram :: [Results a] -> [([a], Int)]
createHistogram rs = zip (procArrays $ getArrays rs) (getResult rs)


-- 配列の長さと試行回数を受け取って, 乱数列の分布を返す関数(kには0以上の数字)
execute :: Int -> Int -> IO [Results Int]
execute n 0 = return (prepare [1..n])
execute n k = do
  ar <- genRandArray n
  re <- execute n (k-1)
  return (applyResult ar re)



