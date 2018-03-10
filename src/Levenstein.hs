{-# LANGUAGE OverloadedStrings #-}
module Levenstein where

import Control.Monad.ST

import Data.Array.IArray ((!))
import Data.Array.MArray
import Data.Array.ST
import Data.STRef

import Data.Text (Text)
import qualified Data.Text as T

-- | Remove whitespace from both ends and ensure case insensitivity.
normalize :: Text -> Text
normalize = T.strip . T.toLower

{-# SPECIALIZE INLINE textRunWithPosM_ :: (Char -> Int -> ST s ()) -> Text -> ST s () #-}
-- | Fold over a text with the position of the character.
textRunWithPosM_ :: Monad m => (Char -> Int -> m ()) -> Text -> m ()
textRunWithPosM_ f = go 0
    where
        go n t = n `seq` case T.uncons t of
            Nothing -> return ()
            Just (c, t') -> f c n >> go (n + 1) t'

------------------------
-- * Levenstein distance
------------------------

-- | Compute the levenstein distance of two texts (number of changes to get from a to b)
textLevensteinDistance :: Text -> Text -> Int
textLevensteinDistance a b = textLevensteinDistance' (normalize a) (normalize b)

-- | 'textLevensteinDistance' without normalization.
textLevensteinDistance' :: Text -> Text -> Int
textLevensteinDistance' "" x = T.length x
textLevensteinDistance' x "" = T.length x
textLevensteinDistance' a b  | a == b = 0
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
