module Skellect.ListZipper (ListZipper
                           ,before, selection, after, size
                           ,empty ,fromList, fromLists ,toList
                           ,next, prev) where

data ListZipper a = ListZipper [a] a [a] | EmptyListZipper
    deriving (Show, Eq)

selection :: ListZipper a -> Maybe a
selection (ListZipper _ s _) = Just s
selection EmptyListZipper    = Nothing

before, after :: ListZipper a -> [a]
before (ListZipper b _ _) = reverse b
before EmptyListZipper    = []
after  (ListZipper _ _ a) = a
after  EmptyListZipper    = []

empty :: ListZipper a
empty =  EmptyListZipper

size :: ListZipper a -> Int
size EmptyListZipper = 0
size lz              = (+1) $ sum $ map length [before lz, after lz]

fromList :: [a] -> ListZipper a
fromList []     = EmptyListZipper
fromList (x:xs) = ListZipper [] x xs

fromLists :: [a] -> a -> [a] -> ListZipper a
fromLists b = ListZipper (reverse b)

toList :: ListZipper a -> [a]
toList EmptyListZipper    = []
toList (ListZipper b s a) = reverse b ++ (s : a)

prev, next :: ListZipper a -> ListZipper a
prev (ListZipper (x:xs) s ys) = ListZipper xs x (s : ys)
prev z = z

next (ListZipper xs s (y:ys)) = ListZipper (s : xs) y ys
next z = z
