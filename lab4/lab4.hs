-- Лабораторна робота №4
-- студента групи КН-32
-- підгрупа 1
-- Бойчука Віталія
-- Варіант №4

-- Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.

-- Завдання: Фiгури на площинi. Використовуються такi фiгури, як коло (центр та радiус),
-- прямокутник (координати лiвої верхньої та правої нижньої точок), трикутник (координати вершин)
-- та мiтка — label (координати лiвої нижньої точки, шрифт та рядок). 
-- Доступнi шрифти — Consolas, Lucida Console та Source Code Pro. 
-- Визначте функцiї для: отримання прямокутника, який мiстить усi фiгури iз заданого списку.

-- Тести:
-- *Main> getRectangle [Circle 0 0 5, Rectangle 1 1 7 7, Triangle 0 0 7 7 7 0, TextBox 0 0 Consolas "hello"]
--  Rectangle (-5.0) (-8.0) 40.0 7.0


data Font = Consolas | SourceCode | Lucida deriving (Eq, Show)

data Figure = Circle Float Float Float | Rectangle Float Float Float Float | Triangle Float Float Float Float Float Float
              | TextBox  Float Float Font String deriving (Eq, Show)

getLetterSize :: Font -> Float
getLetterSize Consolas  = 6
getLetterSize SourceCode = 8
getLetterSize Lucida   = 10

getRectangles :: [Figure] -> [Figure]
getRectangles [] = []
getRectangles ((Rectangle x1 y1 x2 y2):fs) = Rectangle x1 y1 x2 y2 : getRectangles fs
getRectangles ((Circle {}):fs) = getRectangles fs
getRectangles ((Triangle {}):fs) = getRectangles fs
getRectangles ((TextBox {}):fs) = getRectangles fs

getBound :: Figure -> Figure
getBound (Rectangle x1 y1 x2 y2)     = Rectangle x1 y1 x2 y2
getBound (Circle x y r)              = Rectangle (x-r) (y-r) (x+r) (y+r)
getBound (TextBox x y f s)           = Rectangle x (y-getLetterSize f) (x+fromIntegral (length s) * getLetterSize f) y
getBound (Triangle x1 y1 x2 y2 x3 y3)= Rectangle (min x1 (min x2 x3)) (max y1 (max y2 y3)) (max x1 (max x2 x3)) (min y1 (min y2 y3))

getBounds :: [Figure] -> [Figure]
getBounds = map getBound

getMinX1 :: [Figure] -> Float
getMinX1 [] = 0
getMinX1 [Rectangle x1 y1 x2 y2] = x1
getMinX1 ((Rectangle x1 y1 x2 y2):xs) = min x1 (getMinX1 xs)

getMaxX2 :: [Figure] -> Float
getMaxX2 [] = 0
getMaxX2 [Rectangle x1 y1 x2 y2] = x2
getMaxX2 ((Rectangle x1 y1 x2 y2):xs) = max x2 (getMaxX2 xs)

getMinY1 :: [Figure] -> Float
getMinY1 [] = 0
getMinY1 [Rectangle x1 y1 x2 y2] = y1
getMinY1 ((Rectangle x1 y1 x2 y2):xs) = min y1 (getMinY1 xs)

getMaxY2 :: [Figure] -> Float
getMaxY2 [] = 0
getMaxY2 [Rectangle x1 y1 x2 y2] = y2
getMaxY2 ((Rectangle x1 y1 x2 y2):xs) = max y2 (getMaxY2 xs)

getRectangle :: [Figure] -> Figure
getRectangle x = Rectangle (getMinX1 (getBounds x))  (getMinY1 (getBounds x)) (getMaxX2 (getBounds x)) (getMaxY2 (getBounds x))


-- Висновок: Під час виконання лабораторної роботи, ми ознайомилися та 
-- імплементували класи типів мови Haskell. Також ознайомилися з системою
-- типів та класів типів, визначили власні функції для нового типу. 