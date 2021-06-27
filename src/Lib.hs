module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Fizzbuzz = Fizz | Buzz | FizzBuzz | Other Int deriving (Show, Eq)
{-
sealed class Fizzbuzz {
   object Fizz : Fizzbuzz()
   object Buzz : Fizzbuzz()
   object FizzBuzz : Fizzbuzz()
   data class Other(val value: Int) : Fizzbuzz()
}
-}

fizzbuzz :: Int -> Fizzbuzz
fizzbuzz n | n `mod` 3 == 0 && n `mod` 5 == 0 = FizzBuzz
           | n `mod` 3 == 0 = Fizz
           | n `mod` 5 == 0 = Buzz
           | otherwise = Other n


render :: Fizzbuzz -> String
-- render Fizz = "Fizz"
-- render Buzz = "Buzz"
-- render FizzBuzz = "Fizzbuzz"
render (Other n) = show n
render f = show f


pureProgram :: Int -> [String]
pureProgram n = calculateFizzbuzzAndRenders (generateList n)

calculateFizzbuzzAndRender :: Int -> String
calculateFizzbuzzAndRender = render . fizzbuzz

calculateFizzbuzzAndRenders :: [Int] -> [String]
calculateFizzbuzzAndRenders = fmap calculateFizzbuzzAndRender

generateList :: Int -> [Int]
generateList n = [1 .. n]

input :: IO String
input = getLine

input2 :: String
input2 = "15"

output :: String -> IO ()
output = putStrLn

parse :: String -> Maybe Int
parse = readMaybe

parseIO :: IO String -> IO (Maybe Int)
parseIO = fmap parse

fizzbuzzMaybe :: Maybe Int -> Maybe String
fizzbuzzMaybe = fmap calculateFizzbuzzAndRender

pureProgramMaybe :: Maybe Int -> Maybe [String]
pureProgramMaybe = fmap pureProgram

pureProgramMaybeIO :: IO (Maybe Int) -> IO (Maybe [String])
pureProgramMaybe = fmap pureProgramMaybe

pureProgram2 :: String -> Maybe [String]
pureProgram2 = fmap pureProgram . parse

pureProgram3 :: IO String -> IO String
pureProgram3 = fmap (collapseMaybeStrings . pureProgram2)

collapseStrings :: [String] -> String
collapseStrings = unlines

collapseMaybeStrings :: Maybe [String] -> String
collapseMaybeStrings Nothing = "Error"
collapseMaybeStrings (Just s) = collapseStrings s

-- f :: () -> String

program :: IO ()
program = (=<<) output (pureProgram3 input)
-- (=<<) is flatMap
