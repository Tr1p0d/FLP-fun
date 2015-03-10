{-# LANGUAGE OverloadedStrings #-}


import Control.Applicative (many, (<|>), (<*))
import Control.Monad ((>>=), (>=>))
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
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
  return $ LanStatistics (Symbols (constructProbMap s) (M.fromList s)) (Diagrams (constructProbMap d) (M.fromList d)) (Trigrams (constructProbMap t) (M.fromList t))
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
main = getArgs >> withFile "english.db" ReadMode (parseDB >=> processParserOutput )
 where
  processParserOutput :: Either String LanStatistics -> IO ()
  processParserOutput (Left y) = hPutStrLn stdout ( "cannot parse db : " ++ y ) >> exitFailure
  processParserOutput (Right dbStat) = do
	 input <- hGetContents stdin
 	 inputStat <- return $ analyzeInput input
	 decrypt input M.empty dbStat inputStat

  decrypt :: String -> M.Map Symbol Symbol -> LanStatistics -> LanStatistics -> IO ()
  decrypt cryptext key 
    dbStat@(LanStatistics (Symbols dpts dstp) _ _) 
	inStat@(LanStatistics (Symbols ipts istp) _ _) =
	print . substituteSymbols dpts ipts . substituteDiagrams dbStat inStat . substituteTrigrams dbStat inStat $ key

{- Decrypt functions -}

substituteTrigrams :: LanStatistics -> LanStatistics -> Key ->  Key
substituteTrigrams 
  (LanStatistics (Symbols dbpts dbstp)  _ (Trigrams dbpttr dbtrtp))
  (LanStatistics (Symbols inpts instp)  _ (Trigrams inpttr intrtp)) key = substituteTr (mapHead dbpttr) (mapHead inpttr) key	
 where
  substituteTr :: Trigram -> Trigram -> Key -> Key
  substituteTr (x1,x2,x3) (y1,y2,y3) key = M.insert x3 y3 . M.insert x2 y2 . M.insert x1 y1 $ key

substituteDiagrams :: LanStatistics -> LanStatistics -> Key ->  Key
substituteDiagrams 
  (LanStatistics (Symbols dbpts dbstp)  (Diagrams dbpttr dbtrtp) _ )
  (LanStatistics (Symbols inpts instp)  (Diagrams inpttr intrtp) _ ) key = substituteDia (mapHead dbpttr) (mapHead inpttr) key	
 where
  substituteDia :: Diagram -> Diagram -> Key -> Key
  --substituteDia (x1,x2) (y1,y2) key = M.insertWith (flip const) x2 y2 . M.insert x1 y1 $ key
  substituteDia (x1,x2) (y1,y2) key = foldr (M.insertWith (flip const)) key selectedDia
   where
    selectedDias =  [ | ]

substituteSymbols :: M.Map Prob [Symbol] -> M.Map Prob [Symbol] -> Key -> Key
substituteSymbols dbpts inpts key 
  | M.null dbpts && M.null inpts = key
  | M.null dbpts && (not . M.null $ inpts)  = error $ "input is greater than db" ++ show inpts
  | (not . M.null $ dbpts) && M.null inpts  = error $ "db is greater than input" ++ show dbpts
  | True = substituteSymbols (mapTail dbpts) (mapTail inpts) (M.insertWith (flip const) ( mapHead dbpts ) (mapHead inpts ) key)


{- Analytic functions -}

analyzeInput :: String -> LanStatistics
analyzeInput s = LanStatistics
  (makeSymbols s)
  (makeDiagramsProb . makeDiagrams $ s)
  (makeTrigramsProb . makeTrigrams $ s)

makeSymbols :: String -> Symbols
makeSymbols s = Symbols (makeMapProb s) (makeMapChar s)

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
   





