{-
  Counts word colocations in text documents.
  (c) Maciej Pacula 2011
      Raphael Javaux 2011
-}

module Main where

import Control.Monad
import Data.Char
import qualified Data.HashTable as H
import Data.Int
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO

type Word = String
type Sentence = [Word]
type Paragraph = [Sentence]

-- | Keeps a pair of words.
data Pair = Pair !Word !Word
    deriving (Eq, Ord)

-- | Build a pair of word so that the pair is unique for any combinaisons of the
-- same words (pair "Hello" "World" == pair "World" "Hello").
pair w1 w2 = Pair (min w1 w2) (max w1 w2)

data CountPair = CountPair Pair Int64
    deriving (Eq)

instance Show CountPair where
    show (CountPair (Pair w1 w2) c) = show c ++ "\t" ++ w1 ++ " " ++ w2

instance Ord CountPair where -- Sort CountPair by count
    CountPair _ c1 `compare` CountPair _ c2 = c1 `compare` c2

type CountTable = H.HashTable Pair Int64
type CountMap = M.Map Pair Int64

-- | Builds a pair with a counter from an association list.
countPair (p, c) = CountPair p c

-- | Gives all pairs of words for a sentence and a set of words in surrounding
-- sentences. Pairs might not be unique.
sentencePairs :: Sentence -> [Word] -> [Pair]
sentencePairs sentence context =
  [pair a b | a <- sentence, b <- context]

-- | Updates the 'HashTable' with new pairs in the paragraph.
-- You don't have to look at the previous line, this will cause a duplicate
-- pair.
paragraphPairs :: CountTable -> Paragraph -> IO ()
paragraphPairs pairsTable (x:xs@(y:_)) = do
    forM_ (sentencePairs x y) $ \p -> do
        res <- H.lookup pairsTable p
        case res of
             Just v  -> H.update pairsTable p (v+1) >> return ()
             Nothing -> H.insert pairsTable p 1
    paragraphPairs pairsTable xs -- Next paragraph
paragraphPairs _     _            = return ()

-- | Updates the 'Map' with new pairs in the paragraph.
paragraphPairs' :: CountMap -> Paragraph -> CountMap
paragraphPairs' pairsMap (x:xs@(y:_)) =
    let pairs = sentencePairs x y
        pairsMap' = foldl' insertPair pairsMap pairs
        insertPair accMap pair = M.insertWith' (+) pair 1 accMap
    in paragraphPairs' pairsMap' xs -- Next paragraph
paragraphPairs' pairsMap _            = pairsMap

-- | Divides the content in lines, remove repeated words in a same sentence.
sentences :: String -> [Sentence]
sentences = map (uniqueWords . trim) . lines
  where
    uniqueWords = nub' . words
    nub' = S.toList . S.fromList -- O(log² n * n) instead of O(n²) for nub
    trim = reverse . dropSpaces . reverse . dropSpaces
    dropSpaces = dropWhile isSpace

-- | Divides sentences in paragraphs. Removes empty sentences.
paragraphs :: [Sentence] -> [Paragraph]
paragraphs [] = []
paragraphs ss  =
    let (p, ss') = break emptySentence ss
    in p : paragraphs (dropWhile emptySentence ss')
  where
    emptySentence = (== [])

-- | Counts words pairs using a mutable 'HashTable'.
documentCounts :: String -> IO [CountPair]
documentCounts content = do
    table <- H.new (==) hashPair
    forM_ (paragraphs $ sentences content) $ \p ->
        paragraphPairs table p
    
    counts <- H.toList table
    return $ sort $ map countPair counts
  where
    -- | Gives the hashcode of a pair.
    hashPair (Pair w1 w2) = H.hashString $ w1 ++ w2

-- | Counts words pairs using a 'Map'.
documentCounts' :: String -> [CountPair]
documentCounts' content =
    let ps = paragraphs $ sentences content
        countsMap = foldl' paragraphPairs' M.empty ps
    in sort $ map countPair $ M.toList $ countsMap

-- | Counts colocations in text from stdin and output sorted counts to
-- stdout.
main :: IO ()
main = do input <- hGetContents stdin
--          counts <- documentCounts input -- Uses HashTable
          let counts = documentCounts' input -- Uses Map
          forM_ counts print