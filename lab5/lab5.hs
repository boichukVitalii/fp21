-- Лабораторна робота №5
-- студента групи КН-32
-- підгрупа 1
-- Бойчука Віталія
-- Варіант №4

-- Мета: Ознайомитись з модульною органiзацiєю програм та засобами введення-
-- виведення. Набути досвiду компiляцiї Haskell-програм.

-- Завдання: Реалiзувати та скомпiлювати одну з програм, розроблених у лабораторнiй роботi 
-- № 3 для Вашого варiанта з введенням даних: а) з клавiатури, б) з файлу 
-- та виведенням результатiв: в) на екран, г) у файл.


-- Тести:
-- З консолі на екран:
-- 1 1 1 5 5 3 1 1 222 222 222 222
-- [1,5,3,1,222]
-- З файлу в файл:
-- 1 1 1 5 5 3 1 1 222 222 222 222
-- 1 5 3 1 222


repIdentical1 :: (Eq a) => [a] -> [a]
repIdentical1 [] = [] 
repIdentical1 [x] = [x]
repIdentical1 (x:xs)
    | x == head xs = repIdentical1 xs
    | otherwise = x : repIdentical1 xs
 
func :: [Int] -> [String]
func = map show

main :: IO ()
main = do 

    --З консолі
    con_input <- getLine 
    let console_list = map read (words con_input) :: [Int]

    --В консоль
    print(repIdentical1 console_list)

    --З файлу
    contents <- readFile "inFile.txt"
    let file_list = map read (words contents) :: [Int] 
    
    --В файл
    let output = repIdentical1 file_list                    
    writeFile "outFile.txt" (concat (func output))
    

-- Висновок: Під час виконання лабораторної роботи, ми ознайомилися
-- з модульною організацією програм та засобами введення-виведення.
-- Набули досвіду компіляції Haskell-програм.