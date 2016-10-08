module Main where

-- How you are supposed to do this exercise ?
-- 1) Recursion : This is a powerful thing.
--    In haskell, you would see recursion at a lot of places.
--    and it is easier to do as well..
-- 2) writing sub-functions :

-- See this example ;
-- I have to write a function that give a number N
--   gives me a list that has N "Hello" strings.
-- Types says it all :)
sayNHello :: Int -> [String]
-- I will create a sub-function.. we use "where" for that..
sayNHello n = sayNHello' 0 n []
  where
    -- this is how you write sub-function
    -- we do not need to write types here as compiler derives it for us..
    -- but just for sanity
    sayNHello' :: Int -> Int -> [String] -> [String]
    sayNHello' i n resultSoFar =
      if i < n
      -- keep appending to the list if we are not done yet..
      then sayNHello' (i + 1) n ("Hello" : resultSoFar)
      else resultSoFar -- otherwise we are done..

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = n `mod` 10 : toDigitsRev (n `div` 10) 

-- You need to map over every element of the list..
-- But you can use recursion for it as well..
-- Otherwise you can write to the top of file after `module ..`
-- import qualified Data.List as DL
-- and use DL.map..
-- but remember it is all about writing Recursion at this point..
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs) = x:(2 * y):doubleEveryOther xs
doubleEveryOther x = x

sumDigits :: [Integer] -> Integer
sumDigits [x] = x
sumDigits (x:xs) = x + sumDigits xs

validate :: Integer -> Bool
validate n = undefined

main :: IO ()
main = do
  putStrLn "Hello world"
  -- show takes a type and convert to string
  putStrLn $ show 1
  putStrLn $ show [1 , 2 , 3]
  putStrLn $ show True
  -- Exercise :
  -- call all above functions here..
  putStrLn $ show $ sayNHello 5
  putStrLn $ show $ toDigits 12345
  putStrLn $ show $ toDigitsRev 12345
  putStrLn $ show $ doubleEveryOther [1 , 2 , 3 , 4 , 5]
  putStrLn $ show $ sumDigits [1 , 2 , 3]


-- syntax :
ifSyntax num = if num > 0
               then num - 1
               else num + 1
