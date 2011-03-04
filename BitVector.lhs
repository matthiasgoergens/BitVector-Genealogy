From http://itasoftware.com/careers/work-at-ita/hiring-puzzles.html?catid=114

BitVector Genealogy

The BitVectors are an ancient and immortal race of 10,000, each with a
10,000 bit genome. The race evolved from a single individual by the
following process: 9,999 times a BitVector chosen at random from
amongst the population was cloned using an error-prone process that
considers each bit independently, and flips it with 20% probability.

Write a program to guess the reproductive history of BitVectors from
their genetic material. The randomly-ordered file
bitvectors-genes.data.gz contains a 10,000 bit line for each
individual. Your program's output should be, for each input line, the
0-based line number of that individual's parent, or -1 if it is the
progenitor. Balance performance against probability of mistakes as you
see fit.

To help you test your program, here is a much smaller 500 x 500 input
dataset:bitvectors-genes.data.small.gz, along with its solution file:
bitvectors-parents.data.small.

> {-# OPTIONS -XNoMonomorphismRestriction -XScopedTypeVariables #-}
> module BitVector where
> import Random hiding (next)
> import Control.Monad
> import Data.List
> import Data.Function
> import Data.Ratio
> import Test.QuickCheck hiding (choose,(==>))
> import Test.SmallCheck 
> import qualified Test.SmallCheck as TSC
> import qualified Data.Map as M
> import qualified Data.Set as S

> type Allel = Bool
> type Genom = [Allel]

> (|==>) = (TSC.==>)

> gen :: Int -> IO Genom
> gen n = liftM (map (==(1::Int))) . sequence . replicate n . randomRIO $ (0,1)

> data Edge v = Edge v v deriving Show
> unedge (Edge v w) = [v,w]
> class Normalform a where
>     nf :: a -> a
> instance Ord v => Normalform (Edge v) where
>     nf (Edge v w) = Edge (min v w) (max v w)
> instance Eq v => Eq (Edge v) where
>     (Edge v w) == (Edge v' w')
>         = (v,w) == (v',w') ||
>           (v,w) == (w',v')
> instance Ord v => Ord (Edge v) where
>     compare = (compare `on` (sort.unedge))
> data Graph v = Graph (S.Set v) (S.Set (Edge v)) deriving (Show,Eq)

> annotGrow :: Graph Genom -> IO (Graph Genom)
> annotGrow (Graph v e) = do a <- chooseS v
>                            b <- mutateN a
>                            return $ Graph (S.insert b v) (S.insert (nf $ Edge a b) e)

> checkSort :: Graph Genom -> Float
> checkSort (Graph v e) = ((/) `on` fromIntegral)
>                         found
>                         (S.size e)
>
>     where pairs v' = S.fromList . map nf $ zipWith Edge (init v') (tail v')
>           vs = map (\i -> (sortBy (compare `on` cycled i)) . S.toList $ v) [0 .. length (S.findMin v) - 1]
>           found = S.size $ S.intersection (S.unions . map pairs $ vs) (S.map nf e)
> cycles v = init $ zipWith (++) (tails v) (inits v)
> cycled i v = drop i v ++ take i v

> prop_cycles_length (l::[Int]) = length (cycles l) == length (l)
> prop_cycles_elements (l::[Int]) = (not . null $ l) ==> elem l (cycles l)
> prop_cycles_equal (l::[Int]) = all (sort l==) . map sort . cycles $ l

> mutateN = mapM (mutate1 1 5)


> grow :: [Genom] -> IO [Genom]
> grow s = liftM (:s) $ mutateN =<< choose s
> mutate1 :: Int -> Int -> Allel -> IO Allel
> mutate1 p q i = do x <- randomRIO (0, q - 1)
>                    return $ (if x < p
>                              then not
>                              else id) i

> choose :: [a] -> IO a
> choose l = liftM (l!!) . randomRIO $ (0, length l - 1)
> chooseS = choose . S.toList

> growN 0 start = return start
> growN n start = growN (n-1) =<< grow start

> annotGrowN 0 start = return start
> annotGrowN n start = annotGrowN (n-1) =<< annotGrow start

> make n
>     = do let l = replicate n 0
>          l <- gen n
>          growN n [l]
> annotMake n 
>     = do l <- gen n
>          annotGrowN n $ Graph (S.singleton l) (S.empty)
>          
>                  

mapM (choose [0 1]) [0..]


> shuffle :: [a] -> IO [a]
> shuffle l = do rs <- sequence . replicate (length l) $ randInt
>                return . map snd . sortBy (compare `on` fst) . zip rs $ l

> randInt :: IO Int
> randInt = randomIO
>                   

> display = mapM putStrLn . map (concat . map show)
> n = 500

> reps :: Monad m => Int -> m a -> m [a]
> reps num op = sequence . replicate num $ op

> rep n m = liftM avg $ reps m (liftM checkSort . annotMake $ n)
>     where avg x = (sum x / fromIntegral (length x))

> v (Graph v' _) = v'
> maim = do return ()
>           g <- annotMake n
>           -- display . S.toList . v $ g
>           print . checkSort $ g
>           -- rep n n >>= print
>           
>           -- display =<< shuffle =<< make n

> data STree a = STree a [STree a]

> instance Serial a => Serial (STree a) where
>     series = cons2 STree
> toTree :: (Ord a) => STree a -> Tree a
> toTree (STree a l) = Tree a (toForrest l)
> toForrest l = Forrest . (M.fromList . map op) $ l
>     where op (STree a l') = (a, toForrest l')

> data Forrest a = Forrest (M.Map a (Forrest a)) deriving (Show, Eq)
> data Tree a = Tree a (Forrest a) deriving (Show, Eq)
> unForrest (Forrest f) = f

> node (Tree a _) = a
> distance :: Genom -> Genom -> Ratio Integer
> distance a b = (toInteger . length . filter id $ zipWith (==) a b)
>                % (toInteger $ (min `on` length) a b)

> distT a b = distance a (node b)
> distTT = distance `on` node

> reconstruct :: [Genom] -> Tree Genom
> reconstruct (a:as) = undefined -- Tree a

 add1 :: Genom -> Tree Genom -> Tree Genom
 add1 n t@(Tree g f) = case next t
                       of [] -> Tree g (Forrest (M.fromList [(n, M.empty)]))
                          (m:_) -> add1L n 
                   -- | Tree g (Tree n [] : l)
                   -- | Tree 

findClosest :: Genom -> Tree Genom -> Tree Genom
findClosest n t@(a, l) = minimumBy (compare `on` distT n) (t:l)

> move :: (Ord a) => Tree a -> a -> Tree a
> move (Tree a (Forrest f)) g
>     = Tree g
>       (Forrest (M.insert a
>                 (Forrest $ M.delete g f)
>                 (unForrest (f M.! g))))

> instance (Ord a, Serial a) => Serial (Tree a) where
>     series = cons2 Tree
> instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
>     arbitrary = do a <- arbitrary
>                    f <- arbitrary
>                    return $ Tree a f
>     shrink (Tree a f)
>         = (do a' <- shrink a
>               return (Tree a' f))
>           ++ (shrink f >>= (return . Tree a))

> instance (Ord k, Serial k, Serial v) => Serial (M.Map k v) where
>     series = cons1 M.fromList
> instance (Ord a, Serial a) => Serial (Forrest a) where
>     series = cons1 Forrest
> instance (Ord a, Arbitrary a) => Arbitrary (Forrest a) where
>     arbitrary = do l <- arbitrary
>                    return (Forrest $ M.fromList l)
>     shrink (Forrest f) = do f' <- shrink . M.toList $ f
>                             return . Forrest . M.fromList $ f'
> elementsT :: Ord a => Tree a -> [a]
> elementsT (Tree a f) = a : elementsF f
> elementsF :: Ord a => Forrest a -> [a]
> elementsF (Forrest f)
>     = M.keys f ++ (concat . map elementsF . M.elems $ f)
> next :: Ord a => Tree a -> [a]
> next (Tree _ (Forrest f)) = M.keys f

> prop_next (t :: Tree Integer)
>               = (on S.isSubsetOf S.fromList)
>                 (next t) 
>                 (elementsT t)

> nodup :: (Ord a) => [a] -> Bool
> nodup eT = ((==) `on` length)
>            (S.toList . S.fromList $ eT)
>            eT
> prop_Move1 (t :: Tree Integer)
>     = let et = elementsT t
>       in nodup et ==>
>          all (((==) `on` (sort . elementsT)) t)
>              . map (move t) . next $ t
>
> prop_Move2 (t :: Tree Integer)
>     = let et = elementsT t
>       in nodup et ==>
>       all (t==) . map (flip move (node t) . move t) . next $ t


a - b - c
 \
  d
=>
  d
 /
a   b
 \ /
  x
  |
  c

This approach is limited to small neighbourhood.  Too far away, and
all differences blur into Binomial(n, 0.5).

Try just sorting!