import Data.Monoid

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)

{- 1. **Functor for Sequence**

   Write a `Functor` instance for `Sequence`:
   ```haskell
   instance Functor Sequence where
       fmap :: (a -> b) -> Sequence a -> Sequence b
   ``` -}

instance Functor Sequence where
 fmap _ Empty=Empty
 fmap f(Single x)=Single(f x)
 fmap f(Append l r)=Append(fmap f l)(fmap f r)

 {- 2. **Foldable for Sequence**

   Write a `Foldable` instance for `Sequence` by implementing `foldMap`:
   ```haskell
   instance Foldable Sequence where
       foldMap :: Monoid m => (a -> m) -> Sequence a -> m
   ```
   Your traversal order should be *left-to-right*: the left subsequence of an `Append` is processed before the right. Once the instance is in place, use library functions from `Foldable` to define:
   - `seqToList :: Sequence a -> [a]` — returns all elements in left-to-right order
   - `seqLength :: Sequence a -> Int` — returns the number of elements in the sequence. -}

instance Foldable Sequence where
 foldMap _ Empty=mempty
 foldMap f(Single x)=f x
 foldMap f(Append l r)=foldMap f l<>foldMap f r

seqToList::Sequence a->[a]
seqToList=foldMap(\x->[x])

seqLength::Sequence a->Int
seqLength s=getSum(foldMap(\_->Sum 1)s)

{-  3. **Semigroup and Monoid for Sequence**

   Define `Semigroup` and `Monoid` instances for `Sequence a`:
   ```haskell
   instance Semigroup (Sequence a) where
       (<>) :: Sequence a -> Sequence a -> Sequence a

   instance Monoid (Sequence a) where
       mempty :: Sequence a
   ``` -}

instance Semigroup(Sequence a)where
 (<>)=Append

instance Monoid(Sequence a)where
 mempty=Empty

 
{- 4. **Tail Recursion and Sequence Search**

   Write a function
   ```haskell
   tailElem :: Eq a => a -> Sequence a -> Bool
   ```
   that searches for an element in a `Sequence` using tail recursion with an explicit stack (a list of `Sequence a` values)
    to manage subsequences still to be inspected.  -}

tailElem::Eq a=>a->Sequence a->Bool
tailElem x s=go[s]where
 go[] =False
 go(Empty:xs)=go xs
 go(Single y:xs)|x==y=True|otherwise=go xs
 go(Append l r:xs)=go(l:r:xs)

{-  5. **Tail Recursion and Sequence Flatten**

   Write a tail-recursive function
   ```haskell
   tailToList :: Sequence a -> [a]
   ```
   that converts a `Sequence a` to a list `[a]` in left-to-right order. -} 

tailToList::Sequence a->[a]
tailToList s=go[s][]where
 go[] acc=reverse acc
 go(Empty:xs) acc=go xs acc
 go(Single x:xs) acc=go xs(x:acc)
 go(Append l r:xs) acc=go(l:r:xs) acc

{-  5. **Tail Recursion and Reverse Polish Notation**

   A *Reverse Polish Notation* (RPN) expression is a sequence of tokens:
   ```haskell
   data Token = TNum Int | TAdd | TSub | TMul | TDiv
   ```
   Evaluation uses a stack: numbers are pushed; operators pop two values, apply the operation, and push the result back.

   Write a tail-recursive function
   ```haskell
   tailRPN :: [Token] -> Maybe Int
   ```
   that processes the token list using a list as the operand stack accumulator. 
   Return `Nothing` for malformed 
   expressions (too few operands, tokens remaining after the final result) or division by zero. -}

data Token=TNum Int|TAdd|TSub|TMul|TDiv

tailRPN::[Token]->Maybe Int
tailRPN t=go t[]where
 go[] [r]=Just r
 go[] _=Nothing
 go(TNum n:ts) st=go ts(n:st)
 go(TAdd:ts)(x:y:rs)=go ts((y+x):rs)
 go(TSub:ts)(x:y:rs)=go ts((y-x):rs)
 go(TMul:ts)(x:y:rs)=go ts((y*x):rs)
 go(TDiv:ts)(0:_:_)=Nothing
 go(TDiv:ts)(x:y:rs)=go ts((y`div`x):rs)
 go _ _=Nothing

{-  6. **Expressing functions via `foldr` and `foldl`**

    Without using explicit recursion, implement the following functions using `foldr` and/or `foldl`:

    (a) `myReverse :: [a] -> [a]` — reverses a list. Use `foldl`. 

    (b) `myTakeWhile :: (a -> Bool) -> [a] -> [a]` — returns the longest prefix of elements satisfying the predicate 
    (e.g. `myTakeWhile even [2,4,3,6] = [2,4]`). Use `foldr`. 

    (c) `decimal :: [Int] -> Int` — interprets a list of digits as a decimal number, e.g. `decimal [1,2,3] = 123`. -}

--a)

myReverse::[a]->[a]
myReverse=foldl(flip(:))[]

--b)

myTakeWhile::(a->Bool)->[a]->[a]
myTakeWhile p=foldr(\x acc->if p x then x:acc else [])[]

--c)c


{- 7. **Run-length encoding via folds**

   *Run-length encoding* compresses a list by replacing consecutive runs of the same element with a pair of the element and its count.

   (a) Implement `encode :: Eq a => [a] -> [(a, Int)]` using `foldr`. For example:
   ```haskell
   encode "aaabccca" = [('a',3),('b',1),('c',3),('a',1)]
   ```

   (b) Implement `decode :: [(a, Int)] -> [a]` using `foldr` (and `replicate`). For example:
   ```haskell
   decode [('a',3),('b',1),('c',3),('a',1)] = "aaabccca"
   ```
 -}

decimal::[Int]->Int
decimal=foldl(\a x->a*10+x)0

 --a)


encode::Eq a=>[a]->[(a,Int)]
encode=foldr step[]where
 step x[]=[(x,1)]
 step x((y,n):ys)|x==y=(y,n+1):ys|otherwise=(x,1):(y,n):ys

 --b)

decode::[(a,Int)]->[a]
decode=foldr(\(x,n) acc->replicate n x++acc)[]