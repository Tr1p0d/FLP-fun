{-# LANGUAGE OverloadedStrings #-}


import Control.Applicative (many, (<|>), (<*))
import Control.Monad ((>>=), (>=>))
import Data.Attoparsec.Text as DAT ( double, parseOnly, string, Parser, double, takeWhile, anyChar, char, endOfInput,
  endOfLine)
import Data.List (group, genericLength)
import qualified Data.Map as M --(Map, fromList, lookup, empty, elem) 
import qualified Data.Text.IO as T (hGetContents)
import System.Environment (getArgs)
import System.Exit ( exitFailure )
import System.IO (Handle, openFile, IOMode ( ReadMode), withFile, hPutStrLn, stdout, stderr, stdin, hGetContents )

import Types

probDB :: Parser LanStatistics
probDB = do
  s <- many $ symbol <* endOfLine
  d <- many $ diagram <* endOfLine
  t <- many $ trigram <* endOfLine
  return $ LanStatistics (Symbols M.empty (M.fromList s)) (Diagrams (constructProbMap d) (M.fromList d)) (Trigrams (constructProbMap t) (M.fromList t))
 where
  constructProbMap a = foldr insertProb M.empty a

symbol :: Parser (Symbol, Prob)
symbol = do 
  s <- anyChar
  char ' '
  p <- double
  return (s, p) 

diagram :: Parser (Diagram, Prob)
diagram = do
  a <- anyChar
  b <- anyChar
  char ' '
  p <- double
  return ((a,b), p) 
  
trigram :: Parser (Trigram, Prob)
trigram = do
  a <- anyChar
  b <- anyChar
  c <- anyChar
  char ' '
  p <- double
  return ((a,b,c), p) 

--parseDB :: Handle -> IO LanStatistics
parseDB h = T.hGetContents h >>= (\x -> return $ parseOnly probDB x)

main :: IO ()
main = getArgs >> withFile "english.db" ReadMode ( parseDB >=> processParserOutput )
 where
  processParserOutput :: Either String LanStatistics -> IO ()
  processParserOutput (Left y) = hPutStrLn stdout ( "cannot parse db : " ++ y ) >> exitFailure
  processParserOutput (Right x) = print . getDiagrams $ x


{- Analytic functions -}

makeDiagrams :: String -> [Diagram]
makeDiagrams (x:y:[]) = [(x,y)]
makeDiagrams (x:y:xs) = (x,y) : makeDiagrams (y:xs)

makeDiagramsProb :: [Diagram] -> Diagrams
makeDiagramsProb di = Diagrams (makeMapProb di) (makeMapChar di)

makeTrigrams :: String -> [Trigram]
makeTrigrams (x:y:z:[]) = [(x,y,z)]
makeTrigrams (x:y:z:xs) = (x,y,z) : makeTrigrams (y:z:xs)

makeTrigramsProb :: [Trigram] -> Trigrams
makeTrigramsProb tr = Trigrams (makeMapProb tr) (makeMapChar tr)

makeMapChar a = foldr insertChar M.empty a
insertChar t map 
    | t `M.member` map = M.adjust (+1) t map
	| True = M.insert t 1 map 

makeMapProb a = foldr insertProb M.empty $ M.toList $ makeMapChar a
insertProb (val ,key) map = M.insertWith (++) key [val] map
   





