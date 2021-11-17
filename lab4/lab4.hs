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
-- *Main> getRectangles [(Circle (1, 1) 5),(Rectangle (1, 2) (4, 4)),(Triangle (0, 0) (2, 2) (3,3))]
-- [Circle (1,1) 5,Rectangle (1,2) (4,4),Triangle (0,0) (2,2) (3,3)]

-- *Main> getRectangles [(Circle (1, 4) 10),(Rectangle (2, 2) (3, 3)),(Triangle (1, 2) (3, 4) (5,7)), (Label (3, 3) Consolas "labslabs")]
-- [Circle (1,4) 10,Rectangle (2,2) (3,3),Triangle (1,2) (3,4) (5,7),Label (3,3) Consolas "labslabs"]

data Font = Consolas 
    | LucidaConsole 
    | SourceCodePro 
    deriving (Eq, Show)

data Figure 
    = Circle (Integer, Integer) Integer
  | Rectangle (Integer, Integer) (Integer, Integer)
  | Triangle (Integer, Integer) (Integer, Integer) (Integer, Integer)
  | Label (Integer, Integer) Font String
  deriving (Eq, Show)

getRectangle :: [Figure] -> [Figure]
getRectangle [] = []
getRectangle ((Rectangle (x1, y1) (x2, y2)) : fs) = Rectangle (x1, y1) (x2, y2) : getRectangle fs
getRectangle ((Circle (x1, y1) n) : fs) = Circle (x1, y1) n : getRectangle fs
getRectangle ((Triangle (x1, y1) (x2, y2) (x3, y3)) : fs) = Triangle (x1, y1) (x2, y2) (x3, y3) : getRectangle fs
getRectangle ((Label (x1, y1) fnt str) : fs) = Label (x1, y1) fnt str : getRectangle fs


-- Висновок: Під час виконання лабораторної роботи, ми ознайомилися та 
-- імплементували класи типів мови Haskell. Також ознайомилися з системою
-- типів та класів типів, визначили власні функції для нового типу. 