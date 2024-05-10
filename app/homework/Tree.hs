module Tree where
    data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show, Eq, Ord)

    instance Functor BinTree where
        fmap _ Empty = Empty
        fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

    instance Applicative BinTree where
        pure a = Node a Empty Empty
        Empty <*> _ = Empty
        _ <*> Empty = Empty
        (Node f lf rf) <*> (Node a la ra ) = Node (f a) (lf <*> la) (rf <*> ra)

                    
    exampleTree :: BinTree String
    exampleTree =
        Node "q"
            (Node "qw"
                (Node "qweq" Empty Empty)
                (Node "qweqwe" Empty Empty)
            )
            (Node "EQWEQWEQW"
                (Node "EQWEQWE" Empty Empty)
                (Node "EWQDEADqioejnwdaw" Empty Empty)
            )

    increase :: Num a => BinTree a -> BinTree a
    increase = fmap (+1)

    strToInt :: BinTree String -> BinTree Int
    strToInt = fmap length

    addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
    addMaybes mx my = (+) <$> mx <*> my

    tree1 :: BinTree Int
    tree1 = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)

    tree2 :: BinTree Int
    tree2 = Node 10 (Node 5 Empty Empty) (Node 15 Empty Empty)

    mergeTrees :: (a -> b -> c) -> BinTree a -> BinTree b -> BinTree c
    mergeTrees _ Empty Empty = Empty
    mergeTrees f (Node x xl xr) (Node y yl yr) = Node (f x y) (mergeTrees f xl yl) (mergeTrees f xr yr)


    concatTree :: BinTree a -> BinTree a -> BinTree a
    concatTree Empty tree = tree
    concatTree tree Empty = tree
    concatTree (Node x l r) (Node y l1 r1) = Node x ( concatTree l (Node y l1 r1)) r

