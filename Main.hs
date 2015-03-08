{-# LANGUAGE OverloadedStrings #-}


import Control.Applicative (many, (<|>), (<*))
import Control.Monad ((>>=), (>=>))
import Data.Attoparsec.Text as DAT ( double, parseOnly, string, Parser, double, takeWhile, anyChar, char, endOfInput,
  endOfLine)
import Data.List (group, genericLength)
import qualified Data.Map as M (Map, fromList, lookup) 
import qualified Data.Text.IO as T (hGetContents)
import System.Environment (getArgs)
import System.Exit ( exitFailure )
import System.IO (Handle, openFile, IOMode ( ReadMode), withFile, hPutStrLn, stdout, stderr, stdin, hGetContents )

import Types

{-probDB :: Parser LanguageStatistics
probDB = do
  s <- many $ symbol <* endOfLine
  d <- many $ diagram <* endOfLine
  t <- many $ trigram <* endOfLine
  return $ LanguageStatistics (fromList $ map letterValue s, fromList $ map diagramValue d, fromList $ map trigramValue t)

symbol :: Parser LetterProb
symbol = do 
  s <- anyChar
  char ' '
  n <- double
  return $ LetterProb (n, s) 

diagram :: Parser DiagramProb
diagram = do
  a <- anyChar
  b <- anyChar
  char ' '
  n <- double
  return $ DiagramProb (n, (a,b)) 
  
trigram :: Parser TrigramProb
trigram = do
  a <- anyChar
  b <- anyChar
  c <- anyChar
  char ' '
  n <- double
  return $ TrigramProb (n, (a,b,c)) 

--parseDB :: Handle -> IO LanguageStatistics
parseDB h = T.hGetContents h >>= (\x -> return $ parseOnly probDB x)

main :: IO ()
main = getArgs >> withFile "english.db" ReadMode ( parseDB >=> processParserOutput )
 where
  processParserOutput :: Either String LanguageStatistics -> IO ()
  processParserOutput (Right x) = decrypt stdin x
  processParserOutput (Left y) = hPutStrLn stdout ( "cannot parse db : " ++ y ) >> exitFailure

decrypt :: Handle -> LanguageStatistics -> IO ()
decrypt h ls = undefined
-}


