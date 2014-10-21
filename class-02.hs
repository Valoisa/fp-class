-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms a = (hour, min, sec)
    where
    hour = a `div` 3600
    min = (a - (hour * 3600)) `div` 60 
    sec = a - hour * 3600 - min * 60

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = (h * 3600) + (m * 60) + s
-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)
-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]
-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.
type Point = (Double, Double)
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1))
triangle :: Point -> Point -> Point -> (Double, Double)
triangle (x1, y1) (x2, y2) (x3, y3) = (p, s)
where
    p = ab + bc + ca
        where
            ab = distance (x1, y1) (x2, y2)
            bc = distance (x2, y2) (x3, y3)
            ca = distance (x3, y3) (x1, y1)
    s = (p/2*(p/2 - ab)*(p/2 - bc)*(p/2 - ca))
        where
            ab = distance (x1, y1) (x2, y2)
            bc = distance (x2, y2) (x3, y3)
            ca = distance (x3, y3) (x1, y1)
-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.
-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs)
    | (x `mod` 2) == 0  = 1 + nEven xs
    | otherwise         = nEven xs
-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
-- [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = x*2 : doubleElems xs
-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) = (if (x `mod` 2) /= 0 then [x] else []) ++ fltOdd xs
-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
delNeg :: (Num a, Ord a) => [a] -> [a]
delNeg [] = []
delNeg (x:xs)
    | x >= 0    = x : delNeg xs
    | otherwise = delNeg xs
    
-- б) увеличить элементы с чётными значениями в два раза;
doubleEven :: Integral a => [a] -> [a]
doubleEven [] = []
doubleEven (x:xs)
    | (x `mod` 2) == 0  = x*2 : doubleEven xs
    | otherwise         = x : doubleEven xs
    
-- в) переставить местами чётные и нечётные по порядку следования элементы
-- (для списков нечётной длины отбрасывать последний элемент).
swapOddEven :: Integral a => [a] -> [a]
swapOddEven [] = []
swapOddEven [x] = []
swapOddEven (x:y:xs)
        | (x `mod` 2) ==0 && (y `mod` 2) /= 0   = y : x : swapOddEven xs
        | (x `mod` 2) /=0 && (y `mod` 2) == 0   = y : x : swapOddEven xs
        | otherwise                             = x : y : swapOddEven xs
        
-- 2.5
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = x+y : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
couples :: Num a => [a] -> [a] -> [(a,a)]
couples [] ys = []
couples xs [] = []
couples (x:xs) (y:ys) = (x, y) : couples xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
natDecrease :: Integral n => n -> [n]
natDecrease 0 = []
natDecrease n = n : natDecrease (n - 1)

-- б) в порядке возрастания.
natIncrease :: Integral n => n -> [n]
natIncrease 0 = []
natIncrease n = natIncrease (n - 1) ++ [n]

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
fillA :: a -> [a] -> [a]
fillA a [] = []
fillA a [x] = [x]
fillA a (x:xs) = x : a: fillA a xs

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
separate :: (Eq a) => [a] -> ([a], [a])
separate [] = ([], [])
separate [x] = ([x], [])
separate (x:y:xs) = (alike (x:y:xs), others (x:y:xs))
    where
        alike [x] = [x]
        alike (x:y:xs)
            | x == y    = x : alike (y:xs)  
            | x /= y    = [x]
        others [x] = [] 
        others (x:y:xs)
            | x == y    = others (y:xs)
            | x /= y    = (y:xs)

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
sum_n_first_elems :: [a] -> Int -> a
sum_n_first_elems _ 0 = 0
sum_n_first_elems [] n = error "n is too large!" 
sum_n_first_elems (x:xs) n = x + sum_n_first_elems xs (n - 1)

-- б) Eq a => [a] -> a -> Bool
contains_a :: Eq a => [a] -> a -> Bool
contains_a [] a = False
contains_a (x:xs) a = if (x == a) then True else contains_a xs

-- в) [a] -> Int -> [a]
mult_by_n :: [a] -> Int -> [a]
mult_by_n [] n = []
mult_by_n (x:xs) n = (x * n) : mult_by_n xs

-- г) a -> Int -> [a]
iterate_a :: a -> Int -> [a]
iterate_a a 0 = []
iterate_a a n = a : iterate_a a (n - 1)

iterate_a' :: a -> Int -> [a]
iterate_a' a 0 = []
iterate_a' a n = iterate_a' a (n - 1) ++ [a]

-- д) [a] -> [a] -> [a]
sum_pairwise :: [a] -> [a] -> [a]
sum_pairwise xs [] = []
sum_pairwise ys [] = []
sum_pairwise (x:xs) (y:ys) = (x + y) : sum_pairwise xs ys


-- е) Eq a => [a] -> [[a]]
group :: (Eq a) => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (x:xs) = fst (separate (x:xs)) : group (snd (separate (x:xs)))

-- ж) [a] -> [(Int, a)]
number :: [a] -> [(Int, a)]
number xs = zip (natIncrease $ length xs) xs

-- з) Eq a => [a] -> [a]
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique xs = head (fst grpd) : unique (snd grpd) where
    grpd = separate xs