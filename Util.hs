module Util where

--- utils
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] = const True 
isPrefixOf pre = (==pre) . take (length pre)

stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix pre txt
    | pre `isPrefixOf` txt = drop (length pre) txt
    | otherwise = txt


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x ys = case break (==x) ys of
    (ys1, []) -> [ys1]
    (ys1, _:ys2) -> ys1 : splitOn x ys2
