module Skellect.ListZipper (ListZipper(..)
                           ,fromList
                           ,fromLists
                           ,toList
                           ,next
                           ,prev) where

data ListZipper a =
    ListZipper {before    :: [a]
               ,selection ::  a
               ,after     :: [a]} |
    EmptyListZipper deriving (Show, Eq)

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
