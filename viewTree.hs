import System.Environment
import Numeric
import Data.List
import Data.Char
import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString as Bs

main = do
	args <- getArgs
	fileContents <- Bs.readFile (head args)
	putStrLn . dot . buildTree . leafList . count $ fileContents

count::Bs.ByteString->M.Map Word8 Int
count = Bs.foldl akk M.empty
	where akk m c = if c `M.member` m
		then M.insert c (m M.!c + 1) m
		else M.insert c 1 m

data HuffmanTree = EmptyTree
	| HuffmanNode Int HuffmanTree HuffmanTree
	| HuffmanLeaf Int Word8
	deriving(Eq, Show)

instance Ord HuffmanTree where
	EmptyTree <= _ = True
	_ <= EmptyTree = False
	HuffmanNode n _ _ <= HuffmanNode n' _ _ = n <= n'
	HuffmanNode n _ _ <= HuffmanLeaf n' _ = n <= n'
	HuffmanLeaf n _ <= HuffmanNode n' _ _ = n <= n'
	HuffmanLeaf n _ <= HuffmanLeaf n' _ = n <= n'

combine::HuffmanTree->HuffmanTree->HuffmanTree
combine t@(HuffmanNode n _ _) t'@(HuffmanNode n' _ _) = HuffmanNode (n+n') t t'
combine t@(HuffmanLeaf n _) t'@(HuffmanNode n' _ _) = HuffmanNode (n+n') t t'
combine t@(HuffmanNode n _ _) t'@(HuffmanLeaf n' _) = HuffmanNode (n+n') t t'
combine t@(HuffmanLeaf n _) t'@(HuffmanLeaf n' _) = HuffmanNode (n+n') t t'

leafList::M.Map Word8 Int->[HuffmanTree]
leafList = M.foldWithKey addLeaf []
	where addLeaf k a ls = HuffmanLeaf a k : ls

buildTree::[HuffmanTree]->HuffmanTree
buildTree [t] = t
buildTree (t1:t2:ts) = buildTree (sort ((combine t1 t2):ts))
-- buildTree::[HuffmanTree]->[HuffmanTree]->HuffmanTree
-- buildTree [] [t] = t
-- buildTree (a:b:xs) [] = buildTree xs [combine a b]
-- buildTree (a:[]) [] = buildTree [] [a]
-- buildTree (l:ls) (t:ts) = 

dot::HuffmanTree->String
dot t = "digraph Huffman_Tree {\n"
	++ "node [shape=record];\n"
	++ "edge [label=0];\n"
	++ dot' t "ND"
	++ "}"

dot'::HuffmanTree->String->String
dot' (HuffmanNode a b c) n = n ++ " [label=" ++ show a ++ "] [shape=egg];\n"
	++ n ++ " -> " ++ n ++ "0;\n"
	++ dot' b (n ++ "0")
	++ n ++ " -> " ++ n ++ "1 [label=1];\n"
	++ dot' c (n ++ "1")

dot' (HuffmanLeaf a b) n = n ++ " [label=\"{0x" 
	++ (map toUpper $ showIntAtBase 16 intToDigit b "|")
	++ show a ++ "}\"] [color=red];\n"
