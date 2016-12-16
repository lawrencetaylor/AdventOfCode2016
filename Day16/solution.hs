import Data.String
import Data.List as List
import Data.List.Split as List

mirror :: Char -> Char
mirror '0' = '1'
mirror '1' = '0'

reverseAndMirror :: [Char] -> [Char]
reverseAndMirror =  List.map mirror . List.reverse

expanded str = str ++ "0" ++ (reverseAndMirror str)

expand :: Int -> [Char] -> [Char]
expand limit str
  | (List.length str) >= limit = List.take limit str
  | otherwise  = expand limit (str ++ "0" ++ (reverseAndMirror str))

toCheckSumCharacter :: [Char] -> Char
toCheckSumCharacter [a,b]
  | a == b = '1'
  | otherwise = '0'

nextCandidate :: [Char] -> [Char]
nextCandidate = List.map toCheckSumCharacter . List.chunksOf 2 

cLeng s = rem (List.length s) 2

checkSum :: [Char] -> [Char]
checkSum s
  | rem (List.length s) 2 == 1 = s
  | otherwise = checkSum (nextCandidate s)

code :: Int ->  [Char] -> [Char]
code limit = checkSum . (expand limit)

main = do
  print ("Part One: " ++ (code 272 "11110010111001001"))
  print ("Part Two: " ++ (code 35651584 "11110010111001001"))
  putStrLn "Hello, World!"