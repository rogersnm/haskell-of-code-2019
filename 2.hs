import Data.List

valueData :: [Int]
valueData = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,6,23,1,23,6,27,1,13,27,31,2,13,31,35,1,5,35,39,2,39,13,43,1,10,43,47,2,13,47,51,1,6,51,55,2,55,13,59,1,59,10,63,1,63,10,67,2,10,67,71,1,6,71,75,1,10,75,79,1,79,9,83,2,83,6,87,2,87,9,91,1,5,91,95,1,6,95,99,1,99,9,103,2,10,103,107,1,107,6,111,2,9,111,115,1,5,115,119,1,10,119,123,1,2,123,127,1,127,6,0,99,2,14,0,0]
replace :: [a] -> (Int, a) -> [a]
replace xs (i, e) =  before ++ [e] ++ after
    where 
        (before, _:after) = splitAt i xs

getValueData :: [Int] -> Int -> Int -> [Int]
getValueData (x:y:z:xs) newy newz = (x:newy:newz:xs)


parseValues :: Int -> Int -> Int -> Int
parseValues op x y
    | op == 1 = x + y
    | op == 2 = x * y

getNewArrayToPass :: [Int] -> [Int]
getNewArrayToPass (_:_:_:_:xs) = xs

parseAllData :: [Int] -> [Int]
parseAllData allData = go 0 allData
    where go index totalData 
            | index >= (length totalData) - 1  = totalData
            | op == 99 = totalData
            | otherwise = go (index + 4) newData 
            where op  = totalData !! (index)
                  xpos = totalData !! (index + 1)
                  ypos = totalData !! (index + 2)
                  pos = totalData !! (index + 3)
                  xval = totalData !! xpos
                  yval = totalData !! ypos
                  newData = replace totalData (pos, parseValues op xval yval)


allInputs = [getValueData valueData x y | x <- [0..99], y <- [0..99]]

value = find (\xs -> (head (parseAllData xs)) == 19690720) allInputs
