-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage x = putStrLn (show x)

-- Write division here
division :: Double -> Double -> Maybe Double
division x y
    | x == y    = Nothing
    | otherwise = Just (x / y)

-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- Write factList here
factList :: Int -> [Int]
factList n = [factorial i | i <- [1..n]]

-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

main :: IO ()
main = do
    -- testing printAMessage
    printAMessage "Hello, Haskell!"
    -- testing division
    print(division 10 5)
    -- testing factorial
    print(factorial 4)
    -- testing factList
    print(factList 4)
    -- testint merge
    let list1 = [1, 3, 5]
    let list2 = [2, 4, 6]
    print (merge list1 list2)