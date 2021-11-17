-- Лабораторна робота №3
-- студента групи КН-32
-- підгрупа 1
-- Бойчука Віталія
-- Варіант №4

-- Мета: Набути досвiду визначення та використання функцій вищого порядку.

-- Завдання: Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
-- застосуванням вбудованих функцiй вищого порядку. 

-- Завдання 1.4: Послідовність тотожних елементів списку замінити одним елементом,
-- наприклад: [1,1,1,5,5,3,1,1,222,222,222,222] => [1,5,3,1,222].

-- Завдання 2.4: Визначити, чи два числа взаємно прості.

-- Тести:
-- *Main> repIdentical1 [1,1,1,5,5,3,1,1,222,222,222,222]
-- [1,5,3,1,222]
-- *Main> repIdentical1 [2.5, 2.5, 10.0, 3.33, 3.33, 3.33, 1.0, 2.5]
-- [2.5,10.0,3.33,1.0,2.5]

-- *Main> repIdentical2 [1,1,1,5,5,3,1,1,222,222,222,222]
-- [1,5,3,1,222]

-- *Main> prime1 20 27
-- True
-- *Main> prime1 8 12
-- False

-- *Main> prime2 20 27
-- True
-- *Main> prime2 8 12
-- False


-- Завдання 1.4 a:
repIdentical1 :: (Eq a) => [a] -> [a]
repIdentical1 [] = [] 
repIdentical1 [x] = [x]
repIdentical1 (x:xs)
    | x == head xs = repIdentical1 xs
    | otherwise = x : repIdentical1 xs
 
-- Завдання 1.4 б:
repIdentical2 :: (Eq a) => [a] -> [a]
repIdentical2 [] = []
repIdentical2 (x:xs) = x : repIdentical2 (dropWhile (==x) xs) 

-- Завдання 2.4 a:
prime1 :: Integer  -> Integer -> Bool
prime1 a b 
    | div a b == 1 = True 
    | otherwise = False
    where
      div :: Integer -> Integer -> Integer  
      div a b 
         | a == b = a
         | a < b = div a (b-a)  
         | otherwise = div (a-b) b

-- Завдання 2.4 б:
prime2 :: Integer -> Integer -> Bool
prime2 a b = gcd a b == 1
         
-- Висновок: В результаті виконання лабораторної роботи, ми набули досвіду визначення
-- та використання функцій вищого порядку. 