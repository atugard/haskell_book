import           Data.Char
import           System.IO

---------------------------------------------------------------------------------------------------------------------------------------------------------
divides :: Int -> Int -> Bool
divides m n = mod n m == 0

--rmdups removes duplicates from a list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups xs = recursively xs []
  where 
    recursively [] res                    = reverse res 
    recursively (y:ys) res | myElem y res = recursively ys res  
                           | otherwise    = recursively ys (y:res)
--This has the benefit that it removes the duplicates in place...
--An option that might be quicker is to quick sort the list, then just check if two adjacent elements are equal... 
--but we'd lose the placement of our elements. often in implementations we care about the order, so 
--this might be better at the cost of time complexity... 

--Just to get an idea: 
--map f (x:xs) = f x : map f xs 
--filter p xs = [x | x <- xs, p x]
---------------------------------------------------------------------------------------------------------------------------------------------------------
data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ $ int2nat (n-1)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
---------------------------------------------------------------------------------------------------------------------------------------------------------
data List a = Nil | Cons a (List a)
lenList :: List a -> Int 
lenList Nil = 0
len (Cons _ xs) = 1 + lenList xs 
---------------------------------------------------------------------------------------------------------------------------------------------------------
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occursTree :: Eq a => a -> Tree a -> Bool 
occursTree x (Leaf y) = x == y --base case
occursTree x (Node l y r) = x == y || occursTree x l || occursTree x r

flattenTree :: Tree a -> [a]
flattenTree (Leaf x) = [x]
flattenTree (Node l x r) = flattenTree l ++ [x] ++ flattenTree r

--A search tree flattens to a sorted list. From that we know if the value at the node is less than our sought value, it could only be in left subtree, 
--and vice versa. We get the following:

--occursSearchTree :: Ord a => a -> Tree a -> Bool
--occursSearchTree x (Leaf y) = x == y
--occursSearchTree x (Node l y r) | x == y = True
--                                | x < y  = occursSearchTree x l
--                                | x > y  = occursSearchTree x r
---------------------------------------------------------------------------------------------------------------------------------------------------------
  
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

---------------------------------------------------------------------------------------------------------------------------------------------------------
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
  --mult :: Int -> Int -> Int -> Int
  --mult = \x -> \y -> \z -> x * y * z

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

---------------------------------------------------------------------------------------------------------------------------------------------------------

factors :: Int -> [Int]
factors n = [x | x <- [1..n], x `divides` n]

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
shift n c | isAlpha c = int2let ((let2int c + n) `mod` 52)
          | otherwise              = c

encodeCaeser :: Int -> String -> String
encodeCaeser n xs = [shift n x | x <- xs]

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
crack xs = encodeCaeser (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n<-[0..25]]
    table' = freqs xs

---------------------------------------------------------------------------------------------------------------------------------------------------------
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

---------------------------------------------------------------------------------------------------------------------------------------------------------

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
myAnd []     = True
myAnd (x:xs) = x && myAnd xs

myConcat :: [[a]] -> [a]
myConcat []     = []
myConcat (x:xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x: myReplicate (n-1) x

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
myMergeSort []  = []
myMergeSort [x] = [x]
myMergeSort x   = myMerge (myMergeSort first) (myMergeSort second) 
  where first   = take l x
        second  = drop l x
        l       = quot (length x) 2

---------------------------------------------------------------------------------------------------------------------------------------------------------
type Bit = Int
bin2int :: [Bit] -> Int
--bin2int bits = sum [w*b | (w,b) <- zip weights bits]
--  where weights = myIterate (*2) 1
--        myIterate = \f x -> x:myIterate f (f x)

--We have [dcba]_10 = a + 2b + 4c + 8d = a + 2(b + 2(c + 2(d + 2*0))) (**)
--which motivates the following simpler implementation:
bin2int = foldr (\x y -> x + 2*y) 0

--From (**) we also see how we would retrieve [a + 2b + 4c + 8d]_2 = dcba (***):
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2) --just stare at this and (***) to convince yourself that it is true. 

--We ensure that our binary numbers are eight bits by the following means:
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ myRepeat 0)
  where
    myRepeat = \x -> x: myRepeat x

--Encodes string by mapping its chars to a unicode number, then converting each unicode number to an 8bit binary number
encodeString :: String -> [Bit]
encodeString = concatMap (addParityBit . make8 . int2bin . ord)

--To decode the strict, we must chop it back into 8 bit binary numbers. To that end we define:
--chop :: Int -> [Bit] -> [[Bit]]
--chop n []   = []
--chop n bits = take n bits : chop n (drop 8 bits)

--And now we just undo what we did in encodeString:
decodeBits :: [Bit]->String 
decodeBits = map (chr . bin2int . processBits) . chop 9

--We model a perfect communication channel using the identity function, and simulate the transmission of a string as a list of bits: 
transmit :: String -> String
transmit = decodeBits . badChannel . encodeString 

channel :: [Bit] -> [Bit]
channel = id 

---------------------------------------------------------------------------------------------------------------------------------------------------------

--7.9 Exercises
  --1. Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using the higher-order functions map and filter.
       -- (map f) . (filter p)  
  --2. Without looking at the definitions from the standard prelude, define the following higher-order library functions on lists.
    -- a. Decide if all elements of a list satisfy a predicate:
    --    all :: (a -> Bool) -> [a] -> Bool
    -- b. Decide if any element of a list satisfies a predicate:
    --    any :: (a -> Bool) -> [a] -> Bool
    -- c. Select elements from a list while they satisfy a predicate:
    --    takeWhile :: (a -> Bool) -> [a] -> [a]
    -- d. Remove elements from a list while they satisfy a predicate:
    --    dropWhile :: (a -> Bool) -> [a] -> [a]
myAll :: (a -> Bool) -> [a] -> Bool 
myAll p xs = and (map p xs)

myAny :: (a -> Bool) -> [a] -> Bool 
myAny p xs= or (map p xs)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p (x:xs) | p x       = x:myTakeWhile p xs 
                     | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p (x:xs) | p x       = myDropWhile p xs 
                     | otherwise = x:xs

  --3. Redefine the functions map f and filter p using foldr
  
  --   The following is just to get an intuition.
  --   foldr f v []            = v
  --   foldr f v (x:xs)        = x `f` foldr f v xs = f x $ foldr f v xs 
  --   foldr f v [a_1,...,a_n] = a_1 `f` (a_2 `f` (...(a_n `f` v)...))
  
--myMap :: (a -> b) -> [a] -> [b]
--myMap f = foldr (\x y -> (f x): y) [] -- then myMap f [a_1, ..., a_n] = f a_1 : foldr (\x y -> (f x): y) [] [a_2, ..., a_n] = ...
                                      --                              = f a_1 : ... : f a_n : [] = [f a_1, ..., f a_n]
exerciseFilter :: (a -> Bool) -> [a] -> [a]
exerciseFilter p = foldr (\x y -> if p x then x:y else y) []

  --4. Using foldl, define a function dec2int :: [Int] -> Int that converts a decimal number into an integer. For example:
  --   dec2int [2,3,4,5] = 2345 = 2*10^3 + 3*10^2 + 4*10^1 + 5*10^0 = 5 + 10(4 + 10(3 + 10(2 + 10*0))) = (((0*10+2)*10 + 3)*10 + 4)*10 + 5,
  --   which with the following gives us our answer:
  --   foldl f v []            = v
  --   foldl f v (x:xs)        = foldl f (v `f` x) xs
  --   foldl f v [a_1,...,a_n] = (...((v `f` a_1) `f` a_2)...) `f` a_n
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x+y) 0

  --5. Without looking at the definitions from the standard prelude, define the higher-order library function ucrry that converts a function on
  --   pairs into a curried function, and, conversely, the function uncurry that converts a curried function with two arguments into a function 
  --   on pairs.
  
myCurry :: ((a, b) -> c) -> (a -> b -> c)
myCurry f = \x -> \y -> f (x, y)
myUncurry :: (a -> b -> c) -> ((a,b) -> c)
myUncurry f = \(x, y) -> f x y

  --6. A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be defined as follows:
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)
  --   That is, the function unfold p h t produces the empty list if the predicate p is true of the argument value, and otherwise produces
  --   a non-empty list by applying the function h to this value to give the head, and the function t to generate another argument that is
  --   recursively processed in the same way to produce the tail of the list. For example, the function int2bin can be rewritten more compactly
  --   using unfold as follows
  --   int2bin = unfold (== 0) (`mod` 2) (`div` 2)
  --   Redefine the functions chop8, map f and iterate f using unfold.
  
chop :: Int -> [Bit] -> [[Bit]]
chop n = unfold null (take n) (drop n)

exerciseMap :: (a -> b) -> [a] -> [b]
exerciseMap f = unfold null (f . head) tail

--iterate f x = [x, f x, f (f x), f (f (f x)), ...]
myIterate :: (a -> a) -> a -> [a] 
myIterate = unfold (const False) id

  --7. Modify the binary string transmitter example to detect simple tranmission errors using the concept of parity bits. That is, each eight-bit binary number produced during encoding is 
  --   extended with a parity bit, set to one if the number contains an odd number of ones, and to zero otherwise. In turn, each resulting nine-bit binary number consumed during decoding
  --   is checked to ensure that its parity bit is correct, with the parity bit being discarded if this is the case, and a parity error being reported otherwise.

parityBit :: [Bit] -> Bit
parityBit bs = mod (sum $ filter (== 1) bs) 2

addParityBit :: [Bit] -> [Bit]
addParityBit bs = parityBit bs : bs 

checkParityBit :: [Bit] -> Bool
checkParityBit bs = parityBit (tail bs) == head bs 

processBits :: [Bit] -> [Bit]
processBits bs | checkParityBit bs = tail bs 
               | otherwise         = error "Transmission error!"
  --8. Test your new string transmitter program from the previous exercise using a faulty communication channel that forgets the first bit, which can be modelled using the tail 
  --   function on lists of bits.
badChannel :: [Bit] -> [Bit]
badChannel = tail

  --9. Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately applies its two argument functions to successive elements in a list, in turn about order.
  --   eg: altMap (+10) (+100) [0,1,2,3,4] = [10,101,12,103,14]
  
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []     = []
altMap f g (x:xs) = f x : altMap g f xs

luhn xs = 10 `divides` sum (altMap luhnDouble id xs)

---------------------------------------------------------------------------------------------------------------------------------------------------------
--8. Declaring types and classes
type Assoc k v = [(k,v)]

findTable :: Eq k => k -> Assoc k v -> v
findTable k t = head [v | (k',v) <- t, k==k']

--8.6 Tautology checker (!!!!!!!!!!!!! lol)-----------------------------------------------------------------------------------------------------------------
data Prop = Const Bool 
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equivalent Prop Prop
-- Using this data type we can represent p = a => (a and b) as
-- p :: Prop
-- p = Imply (Var 'a') (And (Var 'a') (Var 'b'))
-- To evaluate a Prop we need to know the value of its variables.
-- The only already defined values we've got are boolean constants...
-- so we need a procedure to substitute in Const Bool for Var Char...
-- To that end we define the following table, which associates Char variable names to Bool logical values:
type Subst = Assoc Char Bool

evalProp :: Subst -> Prop -> Bool
evalProp _ (Const b) = b
evalProp s (Var x)   = findTable x s
evalProp s (Not p)   = not (evalProp s p)
evalProp s (And p q) = evalProp s p &&  evalProp s q
evalProp s (Or p q)  = evalProp s p || evalProp s q
evalProp s (Imply p q) = evalProp s p <= evalProp s q
evalProp s (Equivalent p q) = evalProp s p == evalProp s q

--takes a proposition and gives us its variables.
vars :: Prop -> [Char]
vars (Const _)        = []
vars (Var x)          = [x]
vars (Not p)          = vars p
vars (And p q)        = vars p ++ vars q
vars (Or p q)         = vars p ++ vars q
vars (Imply p q)      = vars p ++ vars q
vars (Equivalent p q) = vars p ++ vars q

--We note the following regularity:
----------------------
--False| False False 
--False| False True
--False| True False
--False| True True
---------------------
--True | False False
--True | False True
--True | True False
--True | True True
---------------------
--Which gives us the following recursive definition:
bools :: Int -> [[Bool]]
bools 0     = [[]]
bools n     = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

--We remove duplicates, and generate all possible values for a proposition.
substs :: Prop -> [Subst]
substs p = map (zip _vars) (bools (length _vars))
  where _vars = rmdups (vars p)

isTautology :: Prop -> Bool
isTautology p = and [evalProp s p | s <- substs p]

--8.7 Abstract machine--------------------------------------------------------------------------------------------------------------------------------

data Exprmachine = Valmachine Int | Addmachine Exprmachine Exprmachine | Multmachine Exprmachine Exprmachine

--value :: Expr -> Int
--value (Val n) = n
--value (Add x y) = value x + value y

--If we follow the evaluation of value we get an order of evaluation of subexpressions determined by Haskell. 
--To control order of evaluation we implement a control stack Cont of operations Op.
data Opmachine = EVAL_ADD Exprmachine | EVAL_MULT Exprmachine | ADD Int | MULT Int
type Ctrl = [Opmachine]

--Our evaluator takes an expression and a control stack.
evalExpr :: Exprmachine -> Ctrl -> Int 
evalExpr (Valmachine n) c   = exec c n                        --if the expression is an integer, it is already evaluated, and we begin executing the control stack.
evalExpr (Addmachine x y) c = evalExpr x (EVAL_ADD y : c)     --if it is an addition, we evaluate the first argument,
                                                              --placing EVAL_ADD y on top of the control stack to indicate
                                                              --that the second argument, y, should be evaluated once 
                                                              --evaluation of the first argument is completed.
evalExpr (Multmachine x y) c = evalExpr x (EVAL_MULT y : c)   --if it is a multiplication, same deal...
                                                 
--exec executes the control stack in the context of an integer argument:
exec :: Ctrl -> Int -> Int
exec [] n = n
exec (EVAL_ADD y: c) n = evalExpr y (ADD n : c)
exec (EVAL_MULT y: c) n = evalExpr y (MULT n : c)
exec (ADD m: c)  n = exec c (m+n)
exec (MULT m: c)  n = exec c (m*n)

valueMachine :: Exprmachine -> Int
valueMachine e = evalExpr e []

--As an example, (1+2)+3 |-> e = Add (Add (Val 1) (Val 2)) (Val 3)
-- value e = evalExpr (Add (Add (Val 1) (Val 2)) (Val 3)) []          **
--         = evalExpr (Add (Val 1) (Val 2)) (EVAL (Val 3) : [])
--         = evalExpr (Add (Val 1) (Val 2)) [EVAL (Val 3)]            **
--         = evalExpr (Val 1) (EVAL (Val 2):[EVAL (Val 3)])
--         = evalExpr (Val 1) [EVAL (Val 2), EVAL (Val 3)]            **
--         = exec [EVAL (Val 2), EVAL (Val 3)] 1                      **
--         = evalExpr (Val 2) ADD 1 : [EVAL (Val 3)]
--         = evalExpr (Val 2) [ADD 1, EVAL (Val 3)]                   **
--         = exec [ADD 1, EVAL (VAL 3)] 2                             **
--         = exec [EVAL (VAL 3)] 3                                    **
--         = evalExpr (VAL 3) ADD 3 : []
--         = evalExpr (VAL 3) [ADD 3]                                 **
--         = exec [ADD 3] 3                                           **
--         = exec [] 3+3
--         = exec [] 6                                                **
--         = 6
--A note, this looks a lot like the eval-apply loop from Structure and Interpretation of Computer Programs. Here, exec is playing the role of apply and evalExpr of eval...

--8.9 Exercises
  --1. In a similar manner to the function add, define a recursive multiplication function mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:
  --   Hint: make use of add in your definition.

--m*n= m + m + m + m ... + m (n times), so lets try recursion:
mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add (mult m n) n 

--This works because always the first position argument is being reduced:
--mult m n = add (mult (m-1) n) n
--         = add (add (mult (m-2) n) n) n
--         = ...
--         = add (add (...(add (mult Zero n) n) ...) n) n
--         = add (n (add (... (add Zero n) ...) n)) n
--         = add (n (add (... (add n n) ...) n) n) n
--         which translates to m additions of n, or m * n.

  --2. Although not included in appendix B, the standard prelude defines
  --     data Ordering = LT | EQ | GT
  --   together with a function
  --     compare :: Ord a => a -> a -> Ordering 
  --   that decides if one value in an ordered type is less than (LT), equal to (EQ), or greater than (GT) another value. Using this function, redefine the function
  --   occursSearchTree :: Ord a => a -> Tree a -> Bool for search trees. Why is this new definition more efficient than the original version?
occursSearchTree :: Ord a => a -> Tree a -> Bool
occursSearchTree x (Leaf y) = x == y
occursSearchTree x (Node l y r) = case compare x y of 
                                    EQ ->  True
                                    LT -> occursSearchTree x l
                                    GT -> occursSearchTree x r
  --3. Consider the following type of binary trees:
data Binarytree a = Binaryleaf a | Binarynode (Binarytree a) (Binarytree a)
  --   Let us say that such a tree is balanced if the number of leaves in the left and right subtree of every node differents by at most one, 
  --   with leaves themselves being trivially balanced. Define a function balanced :: Binarytree a -> Bool that decides if a binary tree is balanced or not.
  --   Hint: First define a function that returns the number of leaves in a tree.

numLeaves :: Binarytree a -> Int
numLeaves (Binaryleaf x) = 1
numLeaves (Binarynode t1 t2) = numLeaves t1 + numLeaves t2

balanced :: Binarytree a -> Bool
balanced (Binaryleaf x) = True
balanced (Binarynode t1 t2) | abs (numLeaves t1 - numLeaves t2) <= 1 = True
                            | otherwise = False
  --4. Define a function balance :: [a] -> Binarytree a that converts a non-empty list into a balanced tree. 
  --   Hint: first define a function that splits a list into two halves whose length differs by at most one.
balance :: [a] -> Binarytree a
--This is simple, given the hint. We just recursively split the list into halves that differ by at most one, and use each such half to make a tree.
--We already have a function that halves a list halve :: [a] -> ([a],[a]), defined above.
--By assumption, the argument to balance must be a non-empty list... so our base case is the singleton list.
balance [x] = Binaryleaf x
balance xs = Binarynode (balance (fst halves)) (balance (snd halves))
  where halves = halve xs
  --5. Given the type declaration
  --     data Expr = Val Int | Add Expr Expr
  --   define a higher-order-function
  --     folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a 
  --   such that folde f g replaces each Val constructor in an expression by the function f,
  --   and each Add constructor by the function g.

folde :: (Int -> a) -> (a -> a -> a) -> Exprmachine -> a 
folde f g (Valmachine x) = f x 
folde f g (Addmachine x y) = g (folde f g x) (folde f g y)

  --6. Using folde, define a function evalEx :: Expr -> Int that evaluates an expression to an integer value, and a function 
  --   sizeEx :: Expr -> Int that calculates the number of values in an expression.

evalEx :: Exprmachine -> Int
evalEx = folde id (+)

sizeEx :: Exprmachine -> Int
sizeEx = folde (const 1) (+)

  --7. Complete the following instance declarations:
  --     instance Eq a => Eq (Maybe a) where 
  --       ...
  --     instance Eq a => Eq [a] where
  --       ...
  --Commented them out so that the compiler doesn't complain.
  -- instance Eq a => Eq (Maybe a) where
  --    Just x == Nothing = False
  --    Nothing == Just x = False 
  --    Just x == Just y  = x == y
  --instance Eq a => Eq [a] where 
  --  (x:xs) == (y:ys) = x==y && xs == ys 

  --8. Extend the tautology checker to support the use of logical disjunction (Or) and equivalence (Equiv)
  --   Done.
  
  --9. Extend the abstract machine to support the use of multiplication.
  --   Done.
  --   For my own curiosity I have here the code again, and the evaluation of an expression involving mutliplication.
  --   evalExpr :: Expr -> Ctrl -> Int 
  --   evalExpr (Val n) c   = exec c n 
  --   evalExpr (Add x y) c = evalExpr x (EVAL_ADD y : c) 
  --   evalExpr (Mult x y) c = evalExpr x (EVAL_MULT y : c)
  --                                                    
  --   exec :: Ctrl -> Int -> Int
  --   exec [] n = n
  --   exec (EVAL_ADD y: c) n = evalExpr y (ADD n : c)
  --   exec (EVAL_MULT y: c) n = evalExpr y (MULT n : c)
  --   exec (ADD m: c)  n = exec c (m+n)
  --   exec (MULT m: c)  n = exec c (m*n)
  --   value :: Expr -> Int
  --   value e = evalExpr e []
  --
  --   value (Mult (Add (Val 3) (val 3)) (Val 6)) = evalExpr (Mult (Add (Val 3) (Val 3)) (Val 6)) [] 
  --                                              = evalExpr (Add (Val 3) (Val 3)) [EVAL_MULT Val 6]
  --                                              = evalExpr (Val 3) [EVAL_ADD Val 3, EVAL_MULT Val 6]
  --                                              = exec [EVAL_ADD Val 3, EVAL_MULT Val 6] 3
  --                                              = evalExpr Val 3 [ADD 3, EVAL_MULT Val 6]
  --                                              = exec [ADD 3, EVAL_MULT Val 6] 3
  --                                              = exec [EVAL_MULT Val 6] 6 
  --                                              = evalExpr Val 6 [MULT 6]
  --                                              = exec [MULT 6] 6 
  --                                              = exec [] 36
  --                                              = 36
  --   Another one
  --   value (Add (Mult (Val 3) (Val 10)) (Add (Val 6) (Val 5))) 
  --                                              = evalExpr (Mult (Val 3) (Val 10)) [EVAL_ADD (Add (Val 6) (Val 5))]
  --                                              = evalExpr (Val 3) [EVAL_MULT (Val 10), EVAL_ADD (Add (Val 6) (Val 5))]
  --                                              = exec [EVAL_MULT (Val 10), EVAL_ADD (Add (Val 6) (Val 5))] 3 
  --                                              = evalExpr (Val 10) [MULT 3, EVAL_ADD (Add (Val 6) (Val 5))]
  --                                              = exec [MULT 3, EVAL_ADD (Add (Val 6) (Val 5))] 10 
  --                                              = exec [EVAL_ADD (Add (Val 6) (Val 5))] 30 
  --                                              = evalExpr (Add (Val 6) (Val 5)) [ADD 30]
  --                                              = evalExpr (Val 6) [EVAL_ADD (Val 5), ADD 30]
  --                                              = exec [EVAL_ADD (Val 5), ADD 30] 6
  --                                              = evalExpr (Val 5) [ADD 6, ADD 30]
  --                                              = exec [ADD 6, ADD 30] 5
  --                                              = exec [ADD 30] 11
  --                                              = exec [] 41 
  --                                              = 41 
  --
  --
  --
  --

--9. The countdown problem--------------------------------------------------------------------------------------------------------------------------------
-- This is the problem:
--   Given a sequence of natural numbers and a target natural number, attempt to construct an expression whose value is the target, by combining one or more numbers from the sequence
--   using addition, subtraction, multplication, division, and parentheses.
data Opgame = Addgame | Subgame | Mulgame | Divgame

instance Show Opgame where 
  show Addgame = "+"
  show Subgame = "-"
  show Mulgame = "*"
  show Divgame = "/"

--valid decides if the operator applied to two natural numbers produces another natural number:
valid :: Opgame -> Int -> Int -> Bool
valid Addgame x y = x <= y --because x+y = y+x... this makes it so that we don't keep both expressions like x+y and y+x when x /= y
valid Subgame x y = x > y
valid Mulgame x y = x /= 1 && y /= 1 && x <= y --same reason as above, except now also we remove expressions of the form 1*y or x*1
valid Divgame x y = x /= 1 && x `mod` y == 0  

--apply performs the valid operation:
apply :: Opgame -> Int -> Int -> Int
apply Addgame x y = x + y
apply Subgame x y = x - y
apply Mulgame x y = x * y
apply Divgame x y = x `div` y

--We have a type for valid expressions:
data Exprgame = Valgame Int | Appgame Opgame Exprgame Exprgame

instance Show Exprgame where
  show (Valgame n) = show n 
  show (Appgame o l r) = brak l ++ show o ++ brak r 
    where 
      brak (Valgame n) = show n 
      brak e           = "(" ++ show e ++ ")"

valuesGame :: Exprgame -> [Int]
valuesGame (Valgame n) = [n]
valuesGame (Appgame _ l r) = valuesGame l ++ valuesGame r 

evalGame :: Exprgame -> [Int]
evalGame (Valgame n) = [n | n>0]
evalGame (Appgame o l r) = [apply o x y | x <- evalGame l, y <- evalGame r, valid o x y]

--returns all subsequencies of a list.
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = xss ++ map (x:) xss
  where xss = subs xs
--I'm having trouble running through the computation mentally, so I will work out an example:  
--subs [1,2,3] = subs [2,3] ++ map (1:) subs [2, 3]
--subs [2,3]   = subs [3]   ++ map (2:) subs [3]
--subs [3]     = subs []    ++ map (3:) subs []
--subs []      = [[]]
--subs [3]     = [[]]       ++ map (3:) [[]]
--             = [[]]       ++ [[3]]
--             = [[], [3]]
--subs [2,3]   = [[], [3]]  ++ map (2:) [[], [3]]
--             = [[],[3]]   ++ [[2], [2,3]]
--             = [[], [3], [2], [2,3]]
--subs [1,2,3] = [[], [3], [2], [2,3]] ++ [[1], [1,3], [1,2], [1,2,3]]
--             = [[], [3], [2], [2,3], [1], [1,3], [1,2], [1,2,3]]

--An easy way to see why this works is to consider any sequence 
--[a_0, a_1, ..., a_{n-1}]. We can cover all subsequences by grabbing all of the ones that start with a_0, and all of the ones that don't. 
--                          To get all of the ones that don't, assuming subs works as intended, we simply call subs [a_1, ..., a_{n-1}].
--                          To get all of the ones that do, we simply take all of the ones that don't and append a_0 to it, i.e. map (a_0:) subs [a_1, ..., a_{n-1}]
--                          That leaves us with subs [a_0, a_1, ..., a_{n-1}] = subs [a_1, ..., a_{n-1}] ++ maps (a_0:) subs [a_1, ..., a_{n-1}]
--PROOF OF CORRECTNESS:     Let s be a subsequence. Either s starts with a_0 or it doesn't. If it does, then the subsequence tail s is in subs [a_1, ..., a_{n-1}],
--                          by definition of subs, and so s is in map (a_0:) subs [a_1,..., a_{n-1}]. If it doesn't, then it's in subs [a_1, ... ,a_ {n-1}].

--returns all possible ways of inserting a new element into a list.
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys) -- either you insert it at the beginning or you don't... 

--returns all permutations of a list
perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]
--This is the definition suggested by the linter.
--The easier one is 
--perms []     = [[]]
--perms (x:xs) = concatMap (interleave x) (perms xs)
--Anyway, I will work out an example to get some intuition for what's going on: 
--perms [1,2,3] = (concatMap . interleave) 1 $ foldr (concatMap . interleave) [[]] [2,3]
--foldr (concatMap . interleave) [[]] [2,3] = (concatMap . interleave) 2 $ foldr (concatMap . interleave) [[]] [3]
--foldr (concatMap . interleave) [[]] [3] = (concatMap . interleave) 3 [[]]
--foldr (concatMap . interleave) [[]] [3] = concat $ (map . interleave) 3 [[]]
--                                        = concat (map (interleave 3) [[]])
--                                        = concat ([[[3]]])
--                                        = [[3]]
--foldr (concatMap . interleave) [[]] [2,3] = (concatMap . interleave) 2 [[3]]
--                                          = concat (map (interleave 2) [[3]])
--                                          = concat ([[[2,3], [3,2]]])
--                                          = [[2,3], [3,2]]
--perms [1,2,3] = (concatMap . interleave) 1 [[2,3], [3,2]]
--              = concat (map (interleave 1) [[2,3], [3,2]])
--              = concat ([ [[1,2,3], [2,1,3], [2,3,1]], [[1,3,2], [3,1,2], [3,2,1]]])
--              = [[1,2,3], [2,1,3], [2,3,1], [1,3,2], [3,1,2], [3,2,1]]

--returns all possible ways of selecting zero or more elements in any order from a list:
choices :: [a] -> [[a]]
choices = concatMap perms . subs 

solutionGame :: Exprgame -> [Int] -> Int -> Bool
solutionGame e ns n = elem (valuesGame e) (choices ns) && evalGame e == [n]
--where n is the solution that we're after
--what this says is that irrespective of the operations that we've used,
--if the numbers are one of the possible choices, and the expression evaluates to 
--n then we've got a solution.

--returns all possible ways of splitting a list into two non-empty lists that append to give the original list.
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs): [(x:ls,rs) | (ls,rs) <- split xs]
-- split [1,2,3] = ([1],[2,3]) : [(x:ls,rs) | (ls,rs) <- split [2,3]]
-- split [2,3] = ([2], [3]) : [(x:ls,rs) | (ls,rs) <- split [3]]
-- split [3] = [], by definition
-- split [2,3] = ([2], [3]) : [(x:ls,rs) | (ls,rs) <- []]
--             = [([2],[3])]
-- split [1,2,3] = ([1],[2, 3]) : [(1:ls, rs) | (ls,rs) <- [([2],[3])]]
--               = ([1],[2, 3]) : [([1, 2], [3])]
--               = [([1],[2,3]), ([1,2], [3])]          
--PROOF OF CORRECTNESS: First, x:xs = [x] ++ xs. Next, for any (x:ls, rs) we have x:xs = x:(ls ++ rs) = (x:ls) ++ rs.
--                      Let (ls', rs') be a split of (x:xs). Then ls' = x:ls, and so ... Yeah, split is correct, lul.

--returns all possible expressions whose list of values is precisely a given list:
exprsGame :: [Int] -> [Exprgame]
exprsGame [] = []
exprsGame [n] = [Valgame n]
exprsGame ns = [e | (ls,rs) <- split ns, l <- exprsGame ls, r <- exprsGame rs, e <- combineExprsgame l r] --powerful syntax, gaht dang.

combineExprsgame :: Exprgame -> Exprgame -> [Exprgame]
combineExprsgame l r = [Appgame o l r | o <- opsGame]

opsGame :: [Opgame]
opsGame = [Addgame, Subgame, Mulgame, Divgame]

--exprsGame [1,2] = [e | (ls,rs) <- [([1],[2])], l <- exprsGame ls, r <- exprsGame rs, e <- combineExprsgame l r]
--                = [e | l <- exprsGame [1], r <- exprsGame [2], e <- combineExprsgame l r]
--                = [e | e <- combineExprsgame [Val 1] [Val 2]]
--                = combineExprsgame [Val 1] [Val 2]
--                = [Appgame o l r | o <- [Addgame, Subgame, Mulgame, Divgame]] 
--                = [Appgame Addgame (Val 1) (Val 2), Appgame Subgame (Val 1) (Val 2), 
--
--exprsGame [1,2,3] = [e | (ls,rs) <- [([1],[2,3]), ([1,2], [3])], l <- exprsGame ls, r <- exprsGame rs, e <- combineExprsgame l r]
--                  = combineExprsgame exprsGame [1] exprsGame [2,3] ++
--                  = combineExprsgame exprsGame [1,2] exprsGame [3] ++

--returns all solutions to the game:
solutionsGame :: [Int] -> Int -> [Exprgame]
solutionsGame ns n = [e | ns' <- choices ns, e <- exprsGame ns', evalGame e == [n]]

--expression paired with its value.
type Result = (Exprgame, Int)

resultsGame :: [Int] -> [Result]
resultsGame [] = []
resultsGame [n] = [(Valgame n, n) | n>0]
resultsGame ns = [res | (ls, rs)  <- split ns,
                          lx  <- resultsGame ls,
                          ry  <- resultsGame rs,
                          res <- combineExprsgame' lx ry]
combineExprsgame' :: Result -> Result -> [Result]
combineExprsgame' (l,x) (r,y) = [(Appgame o l r, apply o x y) | o <- opsGame, valid o x y]

--As this solution filters out invalid operations, it runs faster.
solutionsGame' :: [Int] -> Int -> [Exprgame]
solutionsGame' ns n = [e | ns' <- choices ns, (e,m) <- resultsGame ns', m == n]

--9.11 Exercises
--  1. Redefine the combinatorial function choices using a list comprehension rather than using composition, concat and map.
myChoices :: [a] -> [[a]]
myChoices [x] = [[], [x]]
myChoices ns = [p | subseq <- subs ns, p <- perms subseq]

--  2. Define a recursive function isChoice :: Eq a => [a] -> [a] -> Bool that decides if one list is chosen from another, without using the combinatorial 
--     functions perms and subs. Hint: start by defining a function that removes the first occurence of a value from a list.
--isChoice :: Eq a => [a] -> [a] -> Bool 

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst x []                 = []
removeFirst x (y:ys) | x == y    = ys 
                     | otherwise = y:removeFirst x ys 

isChoice :: Eq a => [a] -> [[a]] -> Bool
isChoice x y = length (removeFirst x y) /= length y 

--10. Interactive programming--------------------------------------------------------------------------------------------------------------------------------

--do v1 <- a1 
--   v2 <- a2
--   .
--   .
--   .
--   vn <- an
--   return (f v1 v2 ... vn)

--first perform the action a1 store its result as v1, and so on, then return f v1 v2 ... vn as the result of the expression
--notes: the vi <- ai are called generators.
--       we can write _ <- ai if the result of the action is not needed.

--eg:

act :: IO (Char, Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y) -- return in the context behaves as  (Char, Char) -> IO (Char, Char), so it is a natural transformation...

myGetLine :: IO String
myGetLine = do x <- getChar
               if x == '\n' then
                  return []
               else 
                  do xs <- myGetLine
                     return (x:xs)

--myPutStr :: String -> IO ()
--myPutStr [] = return ()
--myPutStr (x:xs) = do putChar x
--                     putStr xs 

myPutStrLn :: String -> IO ()
myPutStrLn xs = do putStr xs 
                   putChar '\n'

myStrlen :: IO () 
myStrlen = do myPutStr "Enter a string: "
              xs <- myGetLine
              myPutStr "The string has "
              myPutStr (show (length xs))
              myPutStrLn " characters" 

hangman :: IO () 
hangman = do myPutStrLn "Think of a word: "
             word <- sgetLine 
             myPutStrLn "Try to guess it: "
             playHangman word 

--reads a string of characters from the keyboard and echoes each character as a dash symbol '-' 
sgetLine :: IO String 
sgetLine = do x <- getCh
              if x == '\n' then 
                 do putChar x 
                    return []
              else 
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

--reads a single character from the keyboard without echoing it to the screen.
--the primitive hSetEcho comes from the library System.IO and it turns input echoing 
--off prior to reading the characters, and back on again after.
getCh :: IO Char 
getCh = do hSetEcho stdin False 
           x <- getChar
           hSetEcho stdin True 
           return x 

--implements main game loop by repeatedly prompting the second player to guess
--until it equals the secret word:
playHangman :: String -> IO () 
playHangman word = do putStr "? "
                      guess <- myGetLine 
                      if guess == word then 
                         putStrLn "You got it!"
                      else 
                         do putStrLn (match word guess) 
                            playHangman word 
--displays which letters in the word occur anywhere in the guess 
match :: String -> String -> String 
match xs ys = [if x `elem` ys then x else '-' | x <- xs]

--Nim is played with a board as follows: 
--1: * * * * *
  --2: * * * *
    --3: * * *
      --4: * *
        --5: * 

--Then players take turns removing stars from the board. The winner is the one that removes the last star.

--gives next player 
next :: Int -> Int 
next 1 = 2 
next 2 = 1

type Board = [Int]
initial :: Board 
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0) 

--checks if a move is valid
validMove :: Board -> Int -> Int -> Bool 
validMove board row num = board !! (row -1) >= num 

--makes a move
move :: Board -> Int -> Int -> Board 
move board row num = [update r n | (r,n) <- zip [1..] board]
  where update r n = if r == row then n-num else n 

--displays a row to the screen
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

--putBoard :: Board -> IO ()
--putBoard [a,b,c,d,e] = do putRow 1 a 
--                          putRow 2 b
--                          putRow 3 c 
--                          putRow 4 d
--                          putRow 5 e
--
getDigit :: String -> IO Int 
getDigit prompt = do putStr prompt 
                     x <- getChar
                     newline
                     if isDigit x 
                        then return (digitToInt x)
                        else do putStrLn "ERROR: Invalid digit"
                                getDigit prompt
newline :: IO ()
newline = putChar '\n'

--board and player number as argument
playNim :: Board -> Int -> IO ()
playNim  board player =
  do newline
     putBoard board
     if finished board 
        then do newline
                putStr "Player "
                putStr (show (next player))
                putStrLn " wins!!"
        else do newline
                putStr "Player "
                print player
                row <- getDigit "Enter a row number: "
                num <- getDigit "Stars to remove : "
                if validMove board row num 
                   then playNim (move board row num) (next player)
                   else do newline
                           putStrLn "ERROR: Invalid move"
                           playNim board player
nim :: IO () 
nim = playNim initial 1 

--10.10 Exercises 
--  1. Redefine putStr :: String -> IO () using a list comprehension and the library function sequence_ :: [IO a] -> IO () 
myPutStr :: String -> IO ()
myPutStr xs = sequence_ [putChar x | x <- xs]
--  2. Using recursion, define a version of putBoard :: Board -> IO () that displays nim boards of any size, rather than being specific to boards with 
--     five rows of stars. Hint: first define an auxiliary function that takes the current row number as an additional argument 

makeBoard :: Int -> Board 
makeBoard n = reverse [1..n]

putBoard :: Board -> IO ()
--putBoard board = recursively 1 [] 
--  where helper n board                              = putRow n (board !! (n-1))
--        recursively i stack | (i-1) == length board = sequence_ stack
--                            | otherwise             = recursively (i+1) (stack ++ [helper i board])

--Textbook solution was:
--putBoard = putBoard' 1 
--putBoard' r [] = return ()
--putBoard' r (n:ns) = do putRow r n 
--                        putBoard' (r+1) ns 

--  3. In a similar manner to the first exercise, redefine the generalised version of putBoard using a list comprehension and sequence_ 
putBoard board = sequence_ [putRow k (board !! (k-1)) | k <- [1..(length board)]]

--  4. Define an action adder :: IO () that reads an given number of integers from the keyboard, one per line, and displays their sum.
--     Hint: start by defining an auxiliary function that takes the current total and how many numbers remain to be read as arguments. 
--           You will also likely need to use the library functions read and show.  

