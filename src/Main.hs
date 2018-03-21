module Main where

import Prelude hiding (interact)

import Data.Attoparsec.ByteString
import qualified Data.Csv as Csv
import Data.Csv.Parser
import Data.Foldable
import Data.List
import Data.String

import Data.ByteString (ByteString, interact)

import qualified Data.ByteString.Char8 as BC
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp

import Levenstein

import System.Environment

parseCsv :: ByteString -> (Csv.Header, Csv.Csv)
parseCsv bs = let
    opts = DecodeOptions $ fromIntegral $ fromEnum ','
    p = (,) <$> header (decDelimiter opts) <*> csv opts
    in case parseOnly p bs of
        Left err -> error err
        Right rs -> rs

csvText :: (Csv.Header, Csv.Csv) -> ([Text], [[Text]])
csvText (a, b) = (map T.decodeUtf8 $ toList a, map (map T.decodeUtf8 . toList) $ toList b)

type Matrix a = [[a]]

addMatrix :: Num a => [Matrix a] -> Matrix a
addMatrix = foldr1 addMatrices
    where
        addMatrices :: Num a => Matrix a -> Matrix a -> Matrix a
        addMatrices = zipWith (zipWith (+))

mapMatrix :: (a -> b) -> Matrix a -> Matrix b
mapMatrix = map . map

avgMatrix :: Integral a => [Matrix a] -> Matrix Double
avgMatrix xs = mapMatrix (\ x -> fromIntegral x / fromIntegral (length xs)) $ addMatrix xs

toAvgDist :: [Text] -> [[Text]] -> Text
toAvgDist langs xs = pack $ "as.dist(matrix(c("
    ++ intercalate ", " [show d | ds <- avgMatrix [distMatrix ys | ys <- xs], d <- ds]
    ++ "), ncol = "
    ++ show (length $ head xs)
    ++ ", nrow = "
    ++ show (length $ head xs)
    ++ ", dimnames = list(c("
    ++ intercalate ", " ["\"" ++ unpack l ++ "\"" | l <- langs]
    ++ "), c("
    ++ intercalate ", " ["\"" ++ unpack l ++ "\"" | l <- langs]
    ++ "))))"

toDist :: [Text] -> Text
toDist xs = pack $ "as.dist(matrix(c("
    ++ intercalate ", " [show $ textLevensteinDistance a b | a <- xs, b <- xs]
    ++ "), ncol = "
    ++ show (length xs)
    ++ ", nrow = "
    ++ show (length xs)
    ++ ", dimnames = list(c("
    ++ intercalate ", " ["\"lang" ++ show n ++ "\"" | (n, _) <- zip [1 :: Int ..] xs]
    ++ "), c("
    ++ intercalate ", " ["\"lang" ++ show n ++ "\"" | (n, _) <- zip [1 :: Int ..] xs]
    ++ "))))"

encodeCsv :: [Text] -> ByteString
encodeCsv xs = T.encodeUtf8 $ T.unlines
    [pack ("dmat" ++ show n ++ " <- ") `mappend` s | (n, s) <- zip [1 :: Int ..] xs]

onCsv :: (Csv.Header, Csv.Csv) -> ByteString
onCsv = T.encodeUtf8 . (uncurry toAvgDist) . csvText

distMatrix :: [Text] -> Matrix Int
distMatrix wordList = [[textLevensteinDistance a b | b <- wordList] | a <- wordList]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--csv"] -> interact (onCsv . parseCsv)
        _ -> do
            putStrLn "Listening on http://localhost:3000/"
            runSettings defaultSettings $ \ req mkResp -> case pathInfo req of
                [s] | s == fromString "result" -> do
                    let wordList = [T.decodeUtf8 val | (qName, Just val) <- queryString req, "word" `isPrefixOf` BC.unpack qName, not $ BC.null val]
                        matrix = distMatrix wordList
                        html = mkPage "Output distances" $ resultPage wordList matrix
                    mkResp $ responseString html
                _ -> do
                    let html = mkPage "Input words" inputForm
                    mkResp $ responseString html

responseString :: String -> Response
responseString s = responseBuilder ok200 [(hContentType, fromString "text/html")] (fromString s)

inputForm :: String
inputForm = "<form action=\"/result\" method=\"GET\">"
    ++ concat [mkInput i ++ "<br/>" | i <- [0..9]]
    ++ "<input type=\"submit\" value=\"Submit\">"
    ++ "</form>"
    where
        mkInput :: Int -> String
        mkInput i = "<input type=\"text\" name=\"word" ++ show i ++ "\">"

resultPage :: [Text] -> [[Int]] -> String
resultPage wordList matrix = "<a href=\"/\">Input words</a><table>"
    ++ concat ["<tr>" ++ concat row ++ "</tr>" | row <- headRow : zipWith mkRow wordList matrix]
    ++ "</table>"
    where
        headRow :: [String]
        headRow = mkCell "" : map (mkCell . unpack) wordList
        mkRow :: Text -> [Int] -> [String]
        mkRow w dst = mkCell (unpack w) : map (mkCell . show) dst
        mkCell s = "<td>" ++ s ++ "</td>"

mkPage :: String -> String -> String
mkPage title body = "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>"
    ++ title
    ++ "</title></head><body>"
    ++ "<h1>"
    ++ title
    ++ "</h1>"
    ++ body
    ++ "</body></html>"
