{-# LANGUAGE OverloadedStrings #-}
module Levenshtein (textLevenshteinDistance) where

import Control.Monad.ST

import Data.Array.IArray ((!))
import Data.Array.MArray
import Data.Array.ST
import Data.STRef

import Data.Text (Text)
import qualified Data.Text as T

toSameLetter :: Char -> Char
toSameLetter 'ö' = 'ø'
toSameLetter 'ä' = 'æ'
toSameLetter 'w' = 'v'
toSameLetter a   = a

-- | Remove whitespace from both ends and ensure case insensitivity.
normalize :: Text -> Text
normalize = T.map toSameLetter . T.strip . T.toLower

{-# SPECIALIZE INLINE textRunWithPosM_ :: (Char -> Int -> ST s ()) -> Text -> ST s () #-}
-- | Fold over a text with the position of the character.
textRunWithPosM_ :: Monad m => (Char -> Int -> m ()) -> Text -> m ()
textRunWithPosM_ f = go 0
    where
        go n t = n `seq` case T.uncons t of
            Nothing -> return ()
            Just (c, t') -> f c n >> go (n + 1) t'

------------------------
-- * Levenshtein distance
------------------------

-- | Compute the levenshtein distance of two texts (number of changes to get from a to b)
textLevenshteinDistance :: Text -> Text -> Int
textLevenshteinDistance a b = textLevenshteinDistance' (normalize a) (normalize b)

-- | 'textLevenshteinDistance' without normalization.
textLevenshteinDistance' :: Text -> Text -> Int
textLevenshteinDistance' "" x = T.length x
textLevenshteinDistance' x "" = T.length x
textLevenshteinDistance' a b  | a == b = 0
                             | leftLen /= rightLen, right `T.isInfixOf` left = leftLen - rightLen
                             | otherwise = runSTUArray helperST ! rightLen
    where
        aLen = T.length a
        bLen = T.length b
        (left, leftLen, right, rightLen) = if aLen > bLen then (a, aLen, b, bLen) else (b, bLen, a, aLen)
        helperST :: ST s (STUArray s Int Int)
        helperST = do
            distance <- newListArray (0, rightLen) [1..rightLen + 1]
            textRunWithPosM_ (helperSTOuter distance right) left
            return distance
        helperSTOuter :: STUArray s Int Int -> Text -> Char -> Int -> ST s ()
        helperSTOuter distance right2 leftC leftPos = do
            diagonal <- newSTRef leftPos
            writeArray distance 0 (leftPos + 1)
            textRunWithPosM_ (helperSTInner distance diagonal leftC) right2
        helperSTInner :: STUArray s Int Int -> STRef s Int -> Char -> Char -> Int -> ST s ()
        helperSTInner distance diagonal leftC rightC rightPos = do
            let cost :: Int
                cost | rightC == leftC = 0
                     | otherwise       = 1
            rightVal     <- readArray distance rightPos
            nextRightVal <- readArray distance $ rightPos + 1
            diagonal'    <- readSTRef diagonal
            writeArray distance (rightPos + 1) $ min (nextRightVal + 1) (min (rightVal + 1) (diagonal' + cost))
            writeSTRef diagonal nextRightVal
