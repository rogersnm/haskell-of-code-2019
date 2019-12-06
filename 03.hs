import Data.List


splitOnComma :: String -> [String]
splitOnComma string = map (filter (/= ',')) (groupBy (\a b -> b /= ',') string)

data Direction = U | D | L | R
    deriving (Show, Eq)
type ParsedStringKey = (Direction, Int)
type ParsedString = [ParsedStringKey]
parseInputKey :: String -> ParsedStringKey

stringToDirection :: Char -> Direction
stringToDirection 'U' = U
stringToDirection 'D' = D
stringToDirection 'L' = L
stringToDirection 'R' = R

testData = "R75,D30,R83,U83,L12,D49,R71,U7,L72,U62,R66,U55,R34,D71,R55,D58,R83"
testData2 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51,U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
parseInputKey string = (stringToDirection $ head string, read (tail string) :: Int)

parseInput :: String -> ParsedString
parseInput string = map parseInputKey $ splitOnComma string

type Location = (Int, Int)
type Path = [Location]

getNextLocation :: Direction -> Location -> Location
getNextLocation U (x, y) = (x, y + 1)
getNextLocation D (x, y) = (x, y - 1)
getNextLocation L (x, y) = (x + 1, y)
getNextLocation R (x, y) = (x - 1, y)

createPath :: ParsedString -> Path
createPath path = go (head path) path (0,0) []
    where go (dir, 0) [] _ total = total
          go (dir, 0) (x:xs) currentLocation total = go x xs currentLocation total
          go (dir, num) xs currentLocation total = go (dir, num - 1) xs newLocation (total ++ [newLocation])
              where newLocation = getNextLocation dir currentLocation

path1 = createPath $ parseInput testData
path2 = createPath $ parseInput testData2
-- test = 
joinPointsTuple = filter (\((x, y), (x1, y1)) -> x == x1 && y == y1) [(x, y) | x <- path1, y <- path2]
joinPoints = map fst joinPointsTuple

getManhattenDistance :: Location -> Int
getManhattenDistance (x, y) = x + y

getMin = foldr1 min

shortestDistance = getMin $ map (abs . getManhattenDistance) joinPoints
