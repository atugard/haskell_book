import Data.Char

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]

seqn :: Monad m => [m a] -> m [a]
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

--1.7 Exercises 
  --3. Write a product function.
    --product' :: Num a => [a] -> a
    --product' = foldr (*) 1

--4.8 Exercises
  --1. Using library functions, define a function halve :: [a] -> ([a],[a]) st
  --   halve[1,2,3,4,5,6] = ([1,2,3],[4,5,6])
  --
halve :: [a] -> ([a],[a])

halve x = splitAt (quot (length x) 2) x

  --2. Define a function third :: [a] -> a that returns the third element in a list that contains at least this many elements using:
    --  a. head and tail
    --  b. list indexing !!;
    --  c. pattern matching.
    --
thirdA, thirdB, thirdC :: [a] -> a

thirdA x = head (tail (tail x))

thirdB x = x !! 2

thirdC (_:(_:(x:_))) = x

  --3. Consider a function safetail :: [a] -> [a] that behaves in the same way
  --   as tail except that it maps the empty list to itself rather than producing
  --   an error. Using tail and the function null :: [a] -> Bool that decides if 
  --   a list is empty or not, define safetail using:
  --   a. a conditional expression;
  --   b. guarded equations.
  --   c. pattern matching.

safetailA, safetailB, safetailC :: [a] -> [a]

safetailA x = if null x then x else tail x

safetailB x | null x    = x 
            | otherwise = tail x

safetailC [] = []
safetailC (_:xs) = xs

  --4. In a similar way to && in section 4.4, show how the disjunction operator ||
  --   can be defined in four different ways using patern matching.
(|||) :: Bool -> Bool -> Bool
True  ||| True  = True
False ||| True  = True
True  ||| False = True
False ||| False = False
  --5. Without using any other library functions or operators, show how the meaning
  --   of  the following pattern matching definition for logical conjunction && can 
  --   be formalized using conditional expressions:
  --   True && True = True
  --   _    && _    = False
(<||>) :: Bool -> Bool -> Bool
a <||> b = 
  if a == True && b == True 
     then True 
     else False

  --6. Do the same for the following alternative definition, and note the difference
  --   in the number of conditional expressions that are required:
  --   True && b = b
  --   False && _ = False
(<<||>>) :: Bool -> Bool -> Bool
a <<||>> b = 
  if a == True 
     then b 
     else  False
  --It's clearly a terrible idea to do the literal translation so I will leave it at this.

  --7. Show how the meaning of the following curried function definition could be 
  --   formalized in terms of lambda expressions
  --   mult :: Int -> Int -> Int -> Int
  --   mult x y z = x * y * z
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

  --8. The Luhn algorithm is used to check bank card numbers for simple errors such as mistypign a digit, and proceeds as follows: 
  --   * consider each digit as a separate number;
  --   * moving left, double every other number from the second last;
  --   * subtract 9 from each number that is now greater than 9;
  --   * add all the resulting numbers together;
  --   * if the total is divisible by 10, the card number is valid.
  --
  --   Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9 if the result is greater than 9. For example:
  --   luhnDouble 3 = 6
  --   luhnDouble 6 = 3 = 6*2 - 9
  --
  --   Using luhnDouble and the integer remainder function mod, define a function luhn :: Int -> Int -> Int -> Int -> Bool that decides
  --   if a four-digit bank card number is valid. For example:
  --   luhn 1 7 8 4 = True
  --   luhn 4 7 8 3 = False 

luhnDouble :: Int -> Int
luhnDouble x | a > 9     = a-9
             | otherwise = a
             where a = x * 2
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod (sum [luhnDouble a, b, luhnDouble c, d]) 10 == 0

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x'] --god damn it is beautiful

lowers :: String -> Int
lowers xs = length [x | x <- xs,  isAsciiLower x]

uppers :: String -> Int
uppers xs = length [x | x <-xs, isAsciiUpper x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
--ord and chr come from Data.Prelude
let2int :: Char -> Int
let2int c | isLower c =  ord c - ord 'a'
          | isUpper c = ord c - ord 'A' + 26
          | otherwise = error "Input must be a lower or upper case letter."

int2let :: Int -> Char
int2let n | 0<=n  && n<=25  =  chr (ord 'a' + n)
          | 26<=n && n<=51 =  chr (ord 'A' - 26 + n)
          | otherwise = error "Number doesn't map back to letters."

shift :: Int -> Char -> Char
shift n c | isLower c || isUpper c = int2let ((let2int c + n) `mod` 52)
          | otherwise              = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1] ++ 
        [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]



percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a' .. 'z']]
  where n = lowers xs + uppers xs


-- Decoding the caeser cypher is possible via a minimization of the chi-square statistic, which is
-- \sum_{i=0}^{n-1} (os_i - es_i)^2/es_i, where 
-- os are a list of observed frequencies
-- es are a list of expected frequencies
-- and n is the length of the two lists.
-- which we implement as follows:

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es] --luol dang.

--We define a function that rotates the elements of a list n places to the left, wrapping around the start of the list, assuming
--n is between zero and the length of the list:
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n<-[0..25]]
    table' = freqs xs

--5.7 Exercises

  --1. Using a list comprehension, give an expression that calculates the sum 1^2 + 2^2 + ... 100^2 of the first one hundred integer squares.
  --   sum [x^2 | x <- [1..100]]
  --2. Suppose that a coordinate grid of size m x n is given by the list of all pairs (x,y) of integers such that 0 <= x <= m and 0 <= y <= n.
  --   Using a list comprehension, define a function grid :: Int -> Int -> [(Int, Int)] that retuns a coordinate grid of a given size. 
  --   For example:
  --     grid 1 2 = [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)]
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]] 

  --3. Using a list comprehension and the function grid above, define a function square :: Int -> [(Int, Int)] that returns a coordinate square
  --   of size n, excluding the diagonal from (0, 0) to (n,n). For example:
mySquare :: Int -> [(Int, Int)]
mySquare n = [p | p <- grid n n, fst p /= snd p]

  --4. In a similar way to the function length, show how the library function replicate :: Int -> a -> [a] that produces a list of identical elements can be defined
  --   using a list comprehension. For example:
  --   replicate 3 True = [True, True, True]
  --   myReplicate :: Int -> a -> [a]
  --   myReplicate n x = [x | k <- [1..n]]

  --5. A triple (x,y,z) of positive integers is Pythagorean if it satisfies the equation x^2 + y^2 = z^2. Using a list comprehension with three generators,
  --   define a function pyths :: Int -> [(Int, Int, Int)] that returns the list of all such triples whose components are at most a given limit. For example:
  --   pyths 10 = [(3,4,5), (4,3,5),(6,8,10),(8,6,10)]
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

  --6. A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself. Using a list comprehension and the function factors,
  --   define a function perfects :: Int -> [Int] that returns the list of all perfect numbers up to a given limit. For example:
  --   perfects 500 = [6,28,496]
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum [x' | x' <- factors x, x' /= x]]

  --7. Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two generators can be re-expressed using two comprehensions with single generators.
    --   [(x,y) | x <- [1,2], y <- [3,4]] = [(1,3), (1,4), (2, 3) (2, 4)], where we fix one value and vary second, then fix the next and vary 
exercise577 = concat [p | p <- [[(x,3),(x,4)] | x <- [1,2]]]

  --8. Redefine the function positions using the function find .
myPositions :: Eq a => a -> [a] -> [Int]
myPositions x xs = find x $ zip xs [0..] 

  --9. The scalar product of two lists of integers xs and ys of length n is given by the sum of the products of corresponding integers:
  --   \sum_{i=0}^{n-1}(xs_i * ys_i)
  --   In a similar manner to chisqr, show how a list comprehension can be used to define  function scalarproduct :: [Int] -> [Int] -> Int that returns
  --   the scalar product of two lists. For example:
  --   scalarproduct [1,2,3] [4,5,6] = 32
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [ x * y | (x, y) <- zip xs ys]

  --10. Modify the Caesar cipher program to also handle upper-case letters
  --    Done above. Just had to add conditional expressions and adjust the mapped to range. I also added another 25 entries to the table, which
  --    represent the distribution of upper case characters.

--6.8 Exercises
  --1. Counts down forever.
myFac :: Int -> Int
myFac 0 = 1
myFac n | n<0       = error "argument to myFac must be non-negative"
        | otherwise = n * myFac (n-1)
  --2. Define a recursive function sumdown :: Int -> Int that retuns the sum of the non-negative integers from a given value down to zero.
    -- For example, sumdown 3 should return the result 3+2+1+0 = 6
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

  --3. Define the exponentiation operator myExp fo rnon-negative integers using the same pattern of recursion as the multiplication operator *, and show how the expression 2 `myExp` 3 is evaluated using your definition.
myExp :: Int -> Int -> Int
myExp _ 0 = 1
myExp n m = n * myExp n (m-1)
  --   2 `myExp` 3 = 2 * (2 `myExp` 2) = 2 * (2 * (2 `myExp` 1)) = 2 * (2 * (2 * (2 `myExp` 0))) = 2 * (2 * (2 * 1)) = 2 * (2 * 2) = 2 * 4 = 8 
  
  --4. Define a recursive function euclid:: Int -> Int -> Int that implements Euclid's algorithm for calculating the greatest common divisor of two non-negative integers: if the two numbers are equal, this number is the result;
  --   otherwise, the smaller number is substracted from the larger, and the same process is then repeated. For example:
  --   euclid 6 27 = 3
euclid :: Int -> Int -> Int
euclid m n | m == n        = m                 --this case prevents against ending up with something like euclid m 0 or euclid 0 n, 
           | min m n  == m = euclid m (n-m)    --because in that case we would have had at the previous step that m==n.
           | otherwise     = euclid (m-n) n
  --5. It's clear from Lisp practice, so will not do this one.
  --6. Without looking at the definitions of the standard prelude, define the following library functions on lists using recursion.
  --   a. Decide if all logical values in a list are True:
  --      myAnd :: [Bool] -> Bool
  --   b. Concatenate a list of lists:
  --      myConcat :: [[a]] -> [a]
  --   c. Produce a list with n identical elements:
  --      myReplicate :: Int -> a -> [a]
  --   d. Select the nth element of a list:
  --      myListref :: [a] -> Int -> a
  --   e. Decide if a value is an element of a list:
  --      myElem :: Eq a => a -> [a] -> Bool
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x:(myReplicate (n-1) x)

myListref :: [a] -> Int -> a
myListref (x:xs) 0 = x
myListref [] _ = error "index too large!"
myListref (x:xs) n = myListref xs (n-1)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) | x == y    = True
                | otherwise = myElem x ys
  --7. Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted lists to give a single sorted list. For example:
  --   merge [2,5,6] [1,3,4] = [1,2,3,4,5,6]

myMerge :: Ord a => [a] -> [a] -> [a]
myMerge [] ys = ys
myMerge xs [] = xs
myMerge (x:xs) (y:ys) | x <= y     = x:myMerge xs (y:ys) 
                      | otherwise  = y:myMerge (x:xs) ys
  --8. Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort, in which the empty list and singleton lists are already
  --   sorted, and any other list is sorted by merging together the two lists that result from sorting the two halves of the list separately.
myMergeSort :: Ord a => [a] -> [a] 
myMergeSort [] = []
myMergeSort [x] = [x]
myMergeSort x = myMerge (myMergeSort first) (myMergeSort second) 
  where first  = take l x
        second = drop l x
        l      = quot (length x) 2
