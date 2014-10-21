import Data.List
import Data.Char
{-
����� �������� � �������� ���� � �����������, �� �� ��������������. ���������� ���������� ������������
���������, ��������� ��� ���� �������� ������� ��� ����. ��������� ����������� ��������� ��� ����������
�������, �������� ���������� ������� �� ������ Data.List � ����� ������ �������. ����� ����������� �������
������� ������� �� ������ �� �������� ������� �������.
-}
{-
1. ���������� ������ �� ���������� ������� map � filter.
1.1 ������������� ������ ������ ����� ����� ��������� �������:
a) ��������� ��� ��� �������� � ��� ����;
b) ��������� ��� ��� �������� � ������� ���������� � ��� ����;
�) �������� ��� ��� �������� � ��������� ����������;
d) ������� �� ���� ��������, ������� ��������� ����� k;
e) ������������� ���, ������� � ������ ������ ������������� �����;
f) ������� �� ���� ��� ������������� ������ �����.
-}

f11a :: (Integral a) => [a] -> [a]
f11a xs = map (2*) xs

f11b :: (Integral a) => [a] -> [a]
f11b [] = []
f11b (x:xs) = map (\x -> if x `mod` 2 == 0 then x*2 else x ) (x:xs)

f11c :: (Integral a) => [a] -> [a]
f11c [] = []
f11c (x:xs) = map (\x -> if x `mod` 2 /= 0 then 0 else x) (x:xs)

f11d :: (Integral a, Ord a) => a -> [a] -> [a]
f11d k xs = filter (<= k) xs

f11e :: (Integral a, Ord a) => [a] -> [a]
f11e xs = filter (< 0) xs

f11f :: (Integral a, Ord a) => [a] -> [a]
f11f xs = filter (\x -> (x <= 0) && (x `mod` 2 /= 0)) xs


{-
1.2 ��� ������ ���������� ��������� ����� �� ��������� (��� ������������ �����).
������������� ��� ��������� �������:
a) ������������� ������ ���, ����� � �� �������� ����� �� �������� ������������ ��������;
b) ������������� ��������� ���������� � ��������.
-}

f12a :: (Num a, Ord a) => Int -> [(a, a)] -> [(a, a)]
f12a 1 xs = filter belongs_1 xs
    where
        belongs_1 (x, y) = (x > 0) && (y > 0)
f12a 2 xs = filter belongs_2 xs
    where
        belongs_2 (x, y) = (x < 0) && (y > 0)
f12a 3 xs = filter belongs_3 xs
    where
        belongs_3 (x, y) = (x < 0) && (y < 0)
f12a 4 xs = filter belongs_4 xs 
    where
        belongs_4 (x, y) = (x > 0) && (y < 0)

f12b :: (Floating a, Ord a) => [(a, a)] -> [(a, a)]
f12b xs = map convert xs
    where
        convert (x, y)
            | (x > 0) && (y >= 0)   = (r, atan (x / y))
            | (x > 0) && (y < 0)    = (r, atan (x / y) + 2 * pi)
            | (x == 0) && (y > 0)   = (r, pi / 2)
            | (x == 0) && (y < 0)   = (r, 3 * pi / 2)
            | otherwise             = (r, 0)
                where
                    r = sqrt ((x * x) + (y * y))

{-


1.3 ��� ������ ����.
a) ������������� ��� ����� � �������� ��������.
b) ������� �� ���� ��������� ���� �������� �����.
c) ������� �� ���� ��������� ����, ������������ � �������� �����.
-}
f13a :: [String] -> [String]
f13a xs = map (map toUpper) xs

f13b :: Int -> [[Char]] -> [[Char]]
f13b k xs = filter (\xs -> length xs == k) xs
{-
2. ������������ �������� ������������������� (iterate).
a) ������ ����������� �����, ������� � 0.
b) ������ ������ �����.
c) ������ ��������� ������������������: a_0=1, a_n=(1+a_{n-1})/2.
d) ������ �������� ����������� ��������.
e) ������ �����, �������������� n-������� �������� �����.
-}
nats :: [Integer]
nats = iterate (1+) 0

even_numbers :: [Integer]
even_numbers = iterate (2+) 0

my_sequence :: (Fractional a) => [a]
my_sequence = iterate regul 1
    where
        regul x = (1 + x)/2
		
alphabet :: [Char]
alphabet = iterate succ 'a'

binary_numbers :: Int -> [String]
binary_numbers n =  iterate inc_bin_num (zeros n)
    where
        zeros n
            | n > 1     = '0' : zeros (n-1)
            | n == 1    = ['0']
        inc_bin_num xs
            | (last xs) == '0'  = (init xs) ++ ['1']
            | (last xs) == '1'  = (inc_bin_num (init xs)) ++ ['0']		

{-
3. ����������� �������.
a) ��� ������ ��������. ������������� ������ ������ ������� �� ��������: ����� � �� ����� � ...
b) ��� ������ ��� ������������ ����� (��������� ����� �� ���������). ������������� ������ ������
���������� �����, ������� � ����� ������������ ��������.
c) ��� ������ � ��������� ����������� ����� n. ������� ������ �� ��������� ������ n ������.
��������� ��������� ����� ��������� ����� n ���������.
d) ��� ������ � ��������� ����������� ����� n � m. ������� ������ �� ��������������� ���������
������ n ��������� �� ������� ������������ ����������� ��������� �� m ���������.
e) ��� ������. ���������� ����� ������ �������� ���������, ����������� ������ ������ ���������� ��������.
-}

f3a :: [Char] -> [String]
f3a xs = groupBy num_or_not xs
	where
        num_or_not x y = isDigit x && isDigit y ||
         not (isDigit x) && not (isDigit y)
		 
f3b :: (Num a, Ord a) => [(a, a)] -> [[(a, a)]]
f3b xs = groupBy same_quadrant xs
	where
        same_quadrant (x, y) (a, b) = same_sign x a && same_sign y b
            where same_sign a b = (a * b > 0)

f3c :: [a] -> Int -> [[a]]
f3c xs n = map (take n) $ takeWhile (\ys -> length ys > 0) $ iterate (drop n) xs			

f3d :: [a] -> Int -> Int -> [[a]]
f3d xs n m = map (take n) $ takeWhile (\ys -> length ys > 0) $ iterate (drop m) xs
-- ������ ���� True
test_f3d = f3d [1..10] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]]

f3e :: (Eq a) => [a] -> Int
f3e xs = maximum $ map length $ groupBy (\x y -> x == y) xs

{-
4. ������ ������.
a) ��� ����� � ���� ������ ��������, ���������� ����� ������� �������� ������. ��������� ����������
���� ������������� � ������ �����.
b) ����� ����� ���� ����� ���������, ��������������� ��������� ���������, � ��������� ����������
(��������: ��� ������ �� 1 �� 106).
c) ���� ������ ������ � ����� n. ������������ ������, ���������� n ����� ����� ������������
� ������ ��������.
d) ��� ������ �����. ������������ ������ ��������� ���������� ��������� ������. ��������� ����������
���������� �������, ������� ����� �������.
e) ��� ������. �������������� ��� ��� ��������.
-}

f4a :: String -> Int
f4a xs = length $ filter (\xs -> isDigit $ head xs) $ f3a xs

f4b :: (Int -> Bool) -> Int -> Int -> Int
f4b f a b = sum $ filter f $ take (b - a) $ drop (a - 1) fib
	where
		fib = 0 : 1 : [a + b | (a,b) <- zip fib (tail fib)]

f4c :: String -> Int -> [Char]
f4c xs n = take n $reverse $ map head $ sortBy (\ys zs -> compare (length ys) (length zs)) $ groupBy (\x y -> x == y) $ sort xs

f4d :: (Ord a) => [a] -> [a]
f4d xs = map (head . drop 1) $ filter (\(x:y:z:xs) -> x < y && z < y ) $ takeWhile (\zs -> length zs > 2) $ f3d xs 3 1

f4e :: [a] -> [a]
f4e (x:xs) = x : (concat $ f3d (x:xs) 2 1) 
 