module Skellect.SelectionList (SelectionList(..)
                              ,fromList
                              ,next
                              ,prev) where

data SelectionList a =
    SelectionList { before    :: [a]
                  , selection ::  a
                  , after     :: [a] } |
    EmptySelectionList

instance Show a => Show (SelectionList a) where
    show EmptySelectionList    = "EmptySelectionList"
    show (SelectionList b s a) = concat ["SelectionList"
                                        ,show b
                                        ," ", show s, " "
                                        ,show a]

instance Eq a => Eq (SelectionList a) where
    (==) x y = case (x, y) of
        (EmptySelectionList, EmptySelectionList) -> True
        (SelectionList b s a, SelectionList b' s' a') ->
            b == b' && s == s' && a == a'
        _ -> False

fromList :: [a] -> SelectionList a
fromList [] = EmptySelectionList
fromList (x:xs) = SelectionList [] x xs

next :: SelectionList a -> SelectionList a
next (SelectionList b s (a:as)) =
    SelectionList (b ++ [s]) a as
next s = s

prev :: SelectionList a -> SelectionList a
prev sl@(SelectionList b s a) = case b of
    [] -> sl
    _  -> SelectionList (init b) (last b) (s : a)
prev sl = sl
