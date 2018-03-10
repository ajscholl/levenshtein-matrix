module Main where

import Data.List
import Data.String

import qualified Data.ByteString.Char8 as BC
import Data.Text (Text, unpack)
import qualified Data.Text.Encoding as T

import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp

import Levenstein

main :: IO ()
main = do
    putStrLn "Listening on http://localhost:3000/"
    runSettings defaultSettings $ \ req mkResp -> case pathInfo req of
        [s] | s == fromString "result" -> do
            let wordList = [T.decodeUtf8 val | (qName, Just val) <- queryString req, "word" `isPrefixOf` BC.unpack qName, not $ BC.null val]
                matrix = [[textLevensteinDistance a b | b <- wordList] | a <- wordList]
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
