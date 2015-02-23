module Skellect.SelectionSeq (SelectionSeq(..)
                             ,fromList
                             ,next
                             ,prev) where

import Data.Sequence (ViewL((:<), EmptyL), viewl
                     ,ViewR((:>), EmptyR), viewr
                     ,Seq
                     ,(<|), (|>))

import qualified Data.Sequence as S (fromList)

data SelectionSeq a =
    SelectionSeq { before    :: Seq a
                 , selection :: a
                 , after     :: Seq a } |
    EmptySelectionSeq

instance Show a => Show (SelectionSeq a) where
    show EmptySelectionSeq    = "EmptySelectionSeq"
    show (SelectionSeq b s a) = concat ["SelectionSeq"
                                        ,show b
                                        ," ", show s, " "
                                        ,show a]

instance Eq a => Eq (SelectionSeq a) where
    (==) x y = case (x, y) of
        (EmptySelectionSeq, EmptySelectionSeq) -> True
        (SelectionSeq b s a, SelectionSeq b' s' a') ->
            b == b' && s == s' && a == a'
        _ -> False

fromList :: [a] -> SelectionSeq a
fromList []     = EmptySelectionSeq
fromList (x:xs) = SelectionSeq (S.fromList []) x (S.fromList xs)

next :: SelectionSeq a -> SelectionSeq a
next sl@(SelectionSeq b s a) = case viewl a of
    s' :< a' -> SelectionSeq (b |> s) s' a'
    EmptyL -> sl
next sl = sl

prev :: SelectionSeq a -> SelectionSeq a
prev sl@(SelectionSeq b s a) = case viewr b of
    b' :> s' -> SelectionSeq b' s' (s <| a)
    EmptyR -> sl
prev sl = sl
