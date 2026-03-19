   {-# LANGUAGE BangPatterns #-}
{- 1. **Goldbach Pairs**
   Write a function `goldbachPairs :: Int -> [(Int, Int)]` that, given an even integer `n ≥ 4`, returns all pairs `(p, q)` 
   satisfying:
   - `p` and `q` are both prime numbers
   - `p + q == n`
   - `p ≤ q`

   Use a list comprehension to generate the result. Define a helper `isPrime :: Int -> Bool` using Exercise 3.
    -}

goldbachPairs::Int->[(Int,Int)]
goldbachPairs n|n<4||odd n=[]|otherwise=[(p,q)|p<-[2..n`div`2],let q=n-p,isPrime p,isPrime q]

{- 2. **Coprime Pairs**
   Write a function `coprimePairs :: [Int] -> [(Int, Int)]` that takes a list of positive integers and returns all unique pairs
    `(x, y)` (with `x < y`) for which `gcd x y == 1`. You may use Haskell's built-in `gcd`. -}
coprimePairs::[Int]->[(Int,Int)]
coprimePairs xs=[(x,y)|x<-xs,y<-xs,x<y,gcd x y==1]

{- 3. **Sieve of Eratosthenes**
   The *Sieve of Eratosthenes* is an ancient algorithm for finding all primes up to a given limit. It works as follows: starting from the list `[2..n]`, take the first element `p` — it must be prime — then remove all multiples of `p` from the rest of the list and repeat.

   Implement this as a recursive function `sieve :: [Int] -> [Int]`, where each recursive step uses a list comprehension to filter out multiples of the head. Then define:
   ```haskell
   primesTo :: Int -> [Int]
   primesTo n = sieve [2..n]
   ```
   Finally, use `primesTo` to define `isPrime :: Int -> Bool` that checks whether a given positive integer is prime.sieve :: [Int] -> [Int] -}
   
sieve::[Int]->[Int]
sieve[]=[]
sieve(p:xs)=p:sieve [x|x<-xs,x`mod`p/=0]

primesTo::Int->[Int]
primesTo n=sieve[2..n]

isPrime::Int->Bool
isPrime n|n<2=False|otherwise=n`elem`primesTo n

{- 4. **Matrix Multiplication**
   Represent a matrix as `[[Int]]` (a list of rows). Write
   ```haskell
   matMul :: [[Int]] -> [[Int]] -> [[Int]]
   ```
   using nested list comprehensions. If the first matrix has dimensions `m × p` and the second `p × n`, then the entry at row `i`, column `j` of the product is:
   ```
   sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1] ]
   ```
   The outer comprehension should range over row indices `i` and column indices `j`. -}
matMul::[[Int]]->[[Int]]->[[Int]]
matMul a b=[[sum[a!!i!!k*b!!k!!j|k<-[0..p-1]]|j<-[0..n-1]]|i<-[0..m-1]]
 where
  m=length a
  p=length(head a)
  n=length(head b)

{- 5. **Permutations**
   Write a function
   ```haskell
   permutations :: Int -> [a] -> [[a]]
   ```
   that generates all k-element permutations (ordered selections without repetition) from a given list.
   For example, for `k = 2` and list `[1,2,3]` the result should be `[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]`. -}
permutations::Eq a=>Int->[a]->[[a]]
permutations 0 _=[[]]
permutations _ []=[]
permutations k xs=[x:ys|x<-xs,ys<-permutations(k-1)(remove x xs)]

remove::Eq a=>a->[a]->[a]
remove _ []=[]
remove y(x:xs)|x==y=xs|otherwise=x:remove y xs

{- 6. **Hamming Numbers**
   A *Hamming number* is a positive integer whose only prime factors are 2, 3, and 5 — numbers of the form 2^a × 3^b × 5^c with a, b, c ≥ 0. The sequence begins: 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, …

   (a) Write a helper
   ```haskell
   merge :: Ord a => [a] -> [a] -> [a]
   ```
   that merges two sorted (potentially infinite) lists into one sorted list, eliminating duplicates.

   (b) Using `merge`, define the infinite list
   ```haskell
   hamming :: [Integer]
   ```
   as a single definition.  -}

merge::Ord a=>[a]->[a]->[a]
merge xs[] =xs
merge[] ys=ys
merge(x:xs)(y:ys)|x<y=x:merge xs(y:ys)|x>y=y:merge(x:xs)ys|otherwise=x:merge xs ys

hamming::[Integer]
hamming=1:merge(map(2*)hamming)(merge(map(3*)hamming)(map(5*)hamming))


{- 7. **Integer Power with Bang Patterns**
   Write a recursive function `power :: Int -> Int -> Int` that computes `power b e = b ^ e` using an accumulator. Use bang patterns on the accumulator to ensure strict evaluation. -}

power::Int->Int->Int
power b e=go 1 e where go !a 0=a;go !a n=go(a*b)(n-1)


{- 8. **Running Maximum: `seq` vs. Bang Patterns**
   Implement two versions of a function `listMax :: [Int] -> Int` that returns the maximum element of a non-empty list using a helper with an accumulator:
   - The first version uses `seq` to force evaluation of the accumulator in the helper function.
   - The second version uses bang patterns on the accumulator argument of the helper function. -}

listMaxSeq::[Int]->Int
listMaxSeq[] =error"empty list"
listMaxSeq(x:xs)=go x xs where go a[] =a;go a(y:ys)=let a'=max a y in a'`seq`go a' ys

listMaxBang::[Int]->Int
listMaxBang[] =error"empty list"
listMaxBang(x:xs)=go x xs where go !a[] =a;go !a(y:ys)=go(max a y)ys


{- 9. **Infinite Prime Stream**
   The `primesTo` function from Exercise 3 only generates primes up to a fixed bound. Using lazy evaluation we can instead define an *infinite* stream of all primes.

   (a) Define
   ```haskell
   primes :: [Int]
   ```
   as an infinite list of all prime numbers, by applying the same sieve idea from Exercise 3 to the infinite list `[2..]`. Your `sieve` function should be unchanged — only the input changes.

   (b) Use `primes` to give a new definition of `isPrime :: Int -> Bool` that does not require an explicit upper bound. -}
primes::[Int]
primes=sieve[2..]

isPrimeInf::Int->Bool
isPrimeInf n|n<2=False|otherwise=head(dropWhile(<n)primes)==n


{- 10. **Strict Accumulation and Space Leaks**
    Computing the mean of a list requires knowing both the sum and the length. Write a function
    ```haskell
    mean :: [Double] -> Double
    ```
    using a tail-recursive helper. Do *not* use any library functions for the recursion.

    (a) Write a first version with no strictness annotations. 

    (b) Fix the space leak using bang patterns. Is a bang pattern on the pair itself sufficient, or do the components also need to be forced individually?

    (c) Generalise your strict solution to compute both the mean and the *variance* σ² = (Σxᵢ²)/n − μ² in a single pass. 
    Apply bang patterns appropriately to all three components. -}
mean1::[Double]->Double
mean1 xs=let(s,n)=go xs (0,0) in s/fromIntegral n where go [] (s,n)=(s,n);go (y:ys) (s,n)=go ys (s+y,n+1)

mean::[Double]->Double
mean xs=let(s,n)=go xs 0 0 in s/fromIntegral n where go [] !s !n=(s,n);go (y:ys) !s !n=go ys (s+y) (n+1)

meanVariance::[Double]->(Double,Double)
meanVariance xs=let(s,s2,n)=go xs 0 0 0;u=s/fromIntegral n;v=s2/fromIntegral n-u*u in(u,v) where go [] !s !s2 !n=(s,s2,n);go (y:ys) !s !s2 !n=go ys (s+y) (s2+y*y) (n+1)