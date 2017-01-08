-- Author: Georgios Kamaras
-- Date: 4/1/2017

--------------------------------------------------------------------------------
-- I need it for 'sort', in problem 3 - stringroot
import Data.List

--------------------------------------------------------------------------------
-- problem 1 - myinternet

-- n: megabytes available for current month
-- ls: current sub-list of each month's consumed megabytes
-- x: megabytes/month limit
-- xs: list of N natural numbers that indicate the consumed megabytes of the first N months
-- I return the megabytes that are available the (N+1)th month

myinternet_rec :: Int -> [Int] -> Int -> Int
myinternet_rec x ls n | (length ls) == 1 = n+x-(head ls)
                      | otherwise = myinternet_rec x (tail ls) n+x-(head ls)

myinternet :: Int -> [Int] -> Int
myinternet x xs = myinternet_rec x xs x

--------------------------------------------------------------------------------
-- problem 2 - pinkballoons

-- g: height of balloon-goal, the height where we expect a specific arrow to find the very next balloon
-- ls: currently examined sub-list of ballons' heights
-- xs: list of ballons' heights
-- I return the number of arrows that Jim will need to blow all the balloons

pinkb :: [Int] -> Int -> [Int]
pinkb ls g | (length ls) == 1 = if (g == (head ls) && g /= 0) then [0] else [(head ls)]
           | g == -1 = [(head ls)] ++ pinkb (tail ls) ((head ls)-1)
           | g == (head ls) = [0] ++ pinkb (tail ls) (g-1)
           | g == 0 = pinkb ls (-1)
           | otherwise = pinkb ([(head ls)] ++ pinkb (tail ls) (g-1)) (-1)

pinkballoons :: [Int] -> Int
pinkballoons xs = (length . filter (/= 0)) (pinkb xs (-1))
-- pinkb returns the initial list, having placed a 0 (zero) in the place of each blown balloon,
-- except for the places of the first ballons that each arrow blew
-- for example, at pinkballoons [2,1,5,4,3] --> 2
-- we have pinkb [2,1,5,4,3] (-1) --> [2,0,5,0,0], because the balloons positioned at this order,
-- at heights 2 and 5 are the first that each of the two arrows blow
-- or, at pinkballoons [4,5,2,1,4] --> 3
-- we have pinkb [4,5,2,1,4] (-1) --> [4,5,0,0,4], because the balloons positioned at this order,
-- at heights 4, 5 and 4 are the first that each of the three arrows blow
-- So, for pinkballoons, I just have to count those first-blown (non-zero) balloons

--------------------------------------------------------------------------------
-- problem 3 - stringroot

-- n: the length that we expect the root to have (starts as 1 (one) and it increments while we can't find a root of this length)
-- t: can be 0 (zero) or 1 (one), used to know when to increment "n"
-- xs: the examined string
-- I return the smallest (in length) root

-- decide if 2 strings (x and y) are anagrams of each other
anagrams :: String -> String -> Bool
anagrams x y = sort x == sort y

sroot :: String -> Int -> Int -> String
sroot xs n t | length xs < 2*n = ""
             | length xs == 2*n = if (anagrams (take n xs) (drop n xs)) then (take n xs) else ""
             | (anagrams (take n xs) (sroot (drop n xs) n 1)) = (take n xs)
             | t == 0 = sroot xs (n+1) t
             | otherwise = ""

stringroot :: String -> String
stringroot xs = sroot xs 1 0
-- In sroot, I iterate through the string recursively and each time I (take from it)
-- 2 parts of n-letters each and I compare them if they are anagrams. If they are I return
-- the first of them to the previous step of the recursion to be compared with another part of n length,
-- if they are not anagrams I return "" (empty string). My base-case occurs,
-- either when n becomes greater than half the length of the examined string
-- (no chance to find a root from that point and forward) or when n is equal to the length of
-- half the examined string, in which case I just compare the two halfs for anagrams and return
-- to the previous step of the recursion. Each time that I return to the initial string (t == 0)
-- and I have not found a root, I search again, with n = n+1
-- (I search for a root of length larger than the previous, by one character). This stops when
-- I reach one of my two base-cases.

--------------------------------------------------------------------------------
-- problem 4 - mymatrix

-- r: number of row
-- c: number of column
-- s: sum of row's elements so far
-- m: middle element
-- x: sum of rows' first elements
-- p: previous element
-- n: number of final matrix's rows and columns
-- I return the produced matrix

row :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int]
row r c s m n x p | c == n = [(n*m)-s] -- last element (last column), any row
                  -- last row, first element (first column) for even n
                  | (even n) && (r == n) && (c == 1) = [n*((n `div` 2)*2*n+1)-x] ++ row r (c+1) (s+n*((n `div` 2)*2*n+1)-x) m n x (n*((n `div` 2)*2*n+1)-x)
                  -- last row, first element (first column) for odd n
                  | (odd n) && (r == n) && (c == 1) = [n*((n `div` 2)*n+1)-x] ++ row r (c+1) (s+n*((n `div` 2)*n+1)-x) m n x (n*((n `div` 2)*n+1)-x)
                  -- any row, first element (first column) for even n
                  | (even n) && (c == 1) = [(r-1)*2*n+c] ++ row r (c+1) (s+(r-1)*2*n+c) m n ((r-1)*2*n+c) ((r-1)*2*n+c)
                  -- any row, first element (first column) for odd n
                  | (odd n) && (c == 1) = [(r-1)*n+c] ++ row r (c+1) (s+(r-1)*n+c) m n ((r-1)*n+c) ((r-1)*n+c)
                  -- any row, middle element
                  | (even n && (c == ((n `div` 2)+1))) || ((odd n) && c == (ceiling (fromIntegral n /fromIntegral 2))) = [p+1] ++ row r (c+1) (s+p+1) (p+1) n x (p+1)
                  -- any row, second element onwards for any n
                  | otherwise = [p+1] ++ row r (c+1) (s+p+1) m n x (p+1)

createrow :: Int -> Int -> Int -> [Int]
createrow r n x = row r 1 0 0 n x 0

creatematrix :: Int -> Int -> Int -> [[Int]]
creatematrix n r x | n == r = [createrow r n x]
                   | otherwise = do
                            let a = createrow r n x
                            let f = head a
                            return a ++ creatematrix n (r+1) (x+f)

mymatrix :: Int -> [[Int]]
mymatrix n = creatematrix n 1 0

--------------------------------------------------------------------------------
-- problem 5 - myseat

-- pm: previously found possible optimal seat
-- f: first seat of the currently examined sub-string (sub-list) of strings
-- l: currently examined sub-string (sub-list) of seats
-- ls: initial string (list) of seats
-- I return the number of the optimal seat (counting starts from 0 (zero))

findseat :: String -> Int -> Int -> Int
-- if occupied 'o' seat other than the first one and we already have found some (possibly) optimal seat then return
findseat l f pm | l == ['o'] && f > 0 && pm /= -1 = -1
                -- check if last seat is empty
                | last(l) == 'e' && f == 0 = (length l)-1
                -- check if first seat is empty
                | (head l) == 'e' && f == 0 = 0
                -- in case that only single seats are empty, like [o,e,o,e,o], we want to seat in the rightmost one
                | (head l) == 'e' = max f (findseat (tail l) (f+1) f)
                -- when we get the first [o,e,e] sequence, then the rightmost empty 'e' seat is the optimal
                | (take 3 l) == "oee" = f+2
                -- in case of single occupied seat, we continue
                | (head l) == 'o' = findseat (tail l) (f+1) pm
                -- if more than three seats are passed
                | otherwise = let
                            m1 = findseat [(head l)] f pm
                            m2 = findseat (tail l) (f+1) m1
                            in (max m1 m2)

-- start from the first seat and move backwards to find the optimal seat.
myseat :: String -> Int
myseat ls = findseat ls 0 (-1)