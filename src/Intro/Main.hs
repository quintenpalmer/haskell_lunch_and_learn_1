module Main where

import Prelude hiding (Maybe(..))

fact :: Int -> Int
fact x
    | x <= 1 = 1
    | x == 2 = 2
    | x >= 3 = x * (fact (x - 1))

data IntLinkedList
    = IntNode Int IntLinkedList
    | IntEmpty
    deriving Show

ilen :: IntLinkedList -> Int
ilen IntEmpty = 0
ilen (IntNode _ rest) = 1 + (ilen rest)

data Maybe a
    = Just a
    | Nothing
    deriving Show

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = case y of
    0 -> Nothing
    _ -> Just (div x y)

businessDiv :: Int -> Int -> Int
businessDiv x y = case safeDiv x y of
    Nothing -> 0
    Just answer -> answer

data GenLinkedList a
    = GenNode a (GenLinkedList a)
    | GenEmpty
    deriving Show

len :: GenLinkedList a -> Int
len GenEmpty = 0
len (GenNode _ rest) = 1 + (len rest)

main :: IO ()
main = do
    putStrLn "1 through 10 looks like:"
    printWithTab [1..10]

    putStrLn "2 times 5 is:"
    printWithTab (2 * 5)

    putStrLn "1 through 6 all doubled looks like:"
    printWithTab (map (* 2) [1..6])

    putStrLn "my computation of factorial looks like:"
    printWithTab (fact 5)

    putStrLn "and the computation spelled out is:"
    printWithTab (5 * 4 * 3 * 2 * 1)

    putStrLn "the factorials of numbers 1 though 6 are:"
    printWithTab (map fact [1..6])

    let myIntList = (IntNode 4 (IntNode 2 (IntNode 1 IntEmpty)))
    putStrLn "this is my test int list:"
    printWithTab myIntList
    putStrLn "and its length is:"
    printWithTab (ilen myIntList)

    let myList = (GenNode "hi" (GenNode "there" (GenNode "you" GenEmpty)))
    putStrLn "this is my test general list:"
    printWithTab myList
    putStrLn "and its length is:"
    printWithTab (len myList)

    putStrLn "business divide 20 / 5"
    print (businessDiv 20 5)

    putStrLn "business divide 20 / 0"
    print (businessDiv 20 0)

printWithTab :: Show a => a -> IO ()
printWithTab x = putStrLn (withTab x)

withTab :: Show a => a -> String
withTab x = "    " ++ (show x)
