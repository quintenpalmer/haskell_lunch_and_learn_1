module Main where

import Data.List.Split (splitOn)
import Data.Text (pack, unpack, strip)
import Safe (readMay, headMay)

main :: IO ()
main = do
    rawLines <- readFile "example.csv"
    let lines = process rawLines
    putStrLn "-----------"
    putStrLn lines
    putStrLn "-----------"
    print (parseEmployees lines)

process :: String -> String
process lines = unpack (strip (pack lines))

testLines :: String
testLines = "#name|age|position|months at company\nQuinten Palmer|25|Software Developer|5\nJames Garfield|33|Software Developer|2"

data Employee = Employee
    { name :: String
    , age :: Int
    , position :: Position
    , months :: Int
    }
    deriving Show

data Position
    = SoftwareDeveloper
    | Product
    deriving Show

data ParseError
    = BadLine String
    | BadRead String
    deriving Show

tryRead :: String -> Either ParseError Position
tryRead "Software Developer" = Right SoftwareDeveloper
tryRead "Product" = Right Product
tryRead x = Left (BadRead ("Could not read " ++ x))

parseEmployees :: String -> Either ParseError [Employee]
parseEmployees lineString = do
    let lines = splitOn "\n" lineString
    case lines of
        (first:rest) -> case first of
            '#':_ -> mapM parseEmployee rest
            _ -> mapM parseEmployee lines
        [] -> Right []

parseEmployee :: String -> Either ParseError Employee
parseEmployee line =
    case splitOn "|" line of
        (actualName:maybeAge:maybePos:maybeMonths:[]) -> do
            actualAge <- (readEither maybeAge) :: Either ParseError Int
            actualPos <- (tryRead maybePos) :: Either ParseError Position
            actualMonths <- (readEither maybeMonths) :: Either ParseError Int
            Right (Employee actualName actualAge actualPos actualMonths)
        _ -> Left (BadLine ("Not enough parameters in line: " ++ (show line)))

readEither :: Read a => String -> Either ParseError a
readEither x = case readMay x of
    Just result -> Right result
    Nothing -> Left (BadRead ("Could not read " ++ x))
