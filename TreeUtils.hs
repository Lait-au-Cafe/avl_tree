module TreeUtils
( Tree
, genRandArray
, testTree
, testAVLTree
, createTree
, createAVLTree
, exportTree
, average
, variance
) where


import System.Random
import Data.List

data Tree a = Null | Node a (Tree a) (Tree a)
instance Show a => Show (Tree a) where
 show t = intercalate "," (encodeTree t)

-- Mathematicaで表示できる形式にエンコード
encodeTree :: Show a => Tree a -> [String]
encodeTree (Node val tl@(Node lval _ _) tr@(Node rval _ _)) = [(show val) ++ "->" ++ (show lval)] ++ [(show val) ++ "->" ++ (show rval)] ++ (encodeTree tl) ++ (encodeTree tr)
encodeTree (Node val tl@(Node lval _ _) Null) = [(show val) ++ "->" ++ (show lval)] ++ (encodeTree tl)
encodeTree (Node val Null tr@(Node rval _ _)) = [(show val) ++ "->" ++ (show rval)] ++ (encodeTree tr)
encodeTree _ = []
 
-- 内容をファイルに書き込む
-- ToExpression[Import[NotebookDirectory[] <> "graph.dat", "Text"]]
exportTree :: (Show a, Ord a) => Tree a -> IO()
exportTree t = writeFile "./graph.dat" ("TreePlot[{" ++ (show t) ++ "}, Automatic, " ++ (takeWhile (/='-') (show t)) ++ ", VertexLabeling -> True, DirectedEdges -> True, ImageSize -> Large]")


-- 与えられた二分木に与えられた要素を追加
-- 追加されるノードはキーであることを想定
addNode :: Ord a => Tree a -> a -> Tree a
addNode Null x = Node x Null Null-- 何も考えずにリーフを作って追加
addNode (Node val tl tr) x
 | x > val   = Node val tl (addNode tr x) -- 右側に再帰
 | x < val   = Node val (addNode tl x) tr -- 左側に再帰

-- 配列の長さ(ノードの数)と試行回数
testTree :: Int -> Int -> IO [Int]
testTree _ 0 = return []
testTree n k = do
 t <- genRandTree n
 xs <- testTree n (k-1)
 return ((getTreeHeight t):xs)
 
 
testAVLTree :: Int -> Int -> IO [Int]
testAVLTree _ 0 = return []
testAVLTree n k = do
 t <- genRandAVLTree n
 xs <- testAVLTree n (k-1)
 return ((getTreeHeight t):xs)
  
 
-- 与えられたAVL木に与えられた要素を追加
addAVLNode :: Ord a => Tree a -> a -> Tree a
addAVLNode t val = balanceTree (addNode t val)
 
-- 与えられたリストから二分木を作成
createTree :: Ord a => [a] -> Tree a
createTree = foldl addNode Null

-- 与えられたリストからAVL木を作成
createAVLTree :: Ord a => [a] -> Tree a
createAVLTree = foldl addAVLNode Null

-- 木の高さを取得
getTreeHeight :: Tree a -> Int
getTreeHeight Null = 0
getTreeHeight (Node _ tl tr) = 1 + (max (getTreeHeight tl) (getTreeHeight tr))

-- AVL木になるようにバランシング
balanceTree :: Tree a -> Tree a
balanceTree Null = Null
balanceTree (Node val tl@(Node vall tll tlr) tr)
 | (getTreeHeight tlr) - (getTreeHeight tr) > 0 = rotateTree (Node val (rotateTree tl False) tr) True
 | (getTreeHeight tll) - (getTreeHeight tr) > 0 = rotateTree (Node val tl tr) True
balanceTree (Node val tl tr@(Node valr trl trr))
 | (getTreeHeight trl) - (getTreeHeight tl) > 0 = rotateTree (Node val tl (rotateTree tr True)) False
 | (getTreeHeight trr) - (getTreeHeight tl) > 0 = rotateTree (Node val tl tr) False
balanceTree (Node val tl tr) = Node val (balanceTree tl) (balanceTree tr)

-- AVL木用. バランシング時の要素の移動
rotateTree :: Tree a -> Bool -> Tree a -- True:時計回り, False:反時計回り
rotateTree (Node val tl@(Node vall tll tlr) tr) True  = Node vall tll (Node val tlr tr)
rotateTree (Node val tl tr@(Node valr trl trr)) False = Node valr (Node val tl trl) trr

-- 与えられた範囲の乱数を生成
genRandRange :: (Int, Int) -> IO Int
genRandRange (x,y) = getStdRandom $ randomR (x,y)

-- 与えられた長さの乱数列を生成
genRandArray :: Int -> IO [Int]
genRandArray n = shuffle [1..n] n

-- 与えられた要素からなる二分木を生成
genRandTree :: Int -> IO (Tree Int)
genRandTree n = do
 ar <- genRandArray n
 return (createTree ar)

-- 与えられた要素からなるAVL木を生成
genRandAVLTree :: Int -> IO (Tree Int)
genRandAVLTree n = do
 ar <- genRandArray n
 return (createAVLTree ar)

-- 与えられたリストの要素のうち二つの位置を交換した配列を返す
swap :: [a] -> Int -> Int -> [a]
swap xs a b
 | a < b = (take (a-1) xs) ++ (take 1 (drop (b-1) xs)) ++ (take (b-a-1) (drop (a) xs)) ++ (take 1 (drop (a-1) xs)) ++ (drop b xs)
 | a > b = (take (b-1) xs) ++ (take 1 (drop (a-1) xs)) ++ (take (a-b-1) (drop (b) xs)) ++ (take 1 (drop (b-1) xs)) ++ (drop a xs)
 |otherwise = xs

-- 与えられたリストの先頭からn個をシャッフルする(フィッシャー・イェーツのシャッフル)
shuffle :: [Int] -> Int -> IO [Int]
shuffle xs n 
 | n < 1 = return xs
　| otherwise = do
  r <- genRandRange (n, 1)
  list <- shuffle xs (n-1)
  return (swap list n r)
 
-- 平均を計算
average :: [Int] -> Float
average xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

-- 分散を計算
variance :: [Int] -> Float
variance xs = sqave - avesq
 where
  sqave = (fromIntegral (foldr (\x->(\y->y+x*x)) 0 xs)) / (fromIntegral (length xs))
  avesq = (\x->x*x) (average xs)