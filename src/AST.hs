{- |
  Description : Абстрактное Синтаксическое Дерево
  Maintainer  : vlastachu@gmail.com
  
  Модуль объявляет структуры для Абстрактного Синтаксического Дерева
  Такие структуры не зависят от исходного языка (т.е. не должны зависить от модуля "Parser")
  и также не зависят от целевого языка (т.е. не должны зависить от  модуля "Compiler").
  Объявление структур похоже на БНФ нотацию. При этом сами структуры также во многом повторяют 
  описание яызка в БНФ нотации. Зачастую порядок аргументов у конструктора записан исходя из 
  порядка последовательностей токенов в языковых конструкциях. Но не всегда, например у конструктора 
  @Binary@ было удобнее записать сначала оператор, затем операнды (в самом языке эта 
  конструкция записывается в инфиксной форме: операнд оператор операнд).
  Почти все @deriving@ написаны для использования в отладке (@Show@) или на всякий случай (@Eq@).
  В двух словах - @deriving@ это магия языка Хаскелл. Например Show обеспечивает нам функцию 
  @show@, которая принимает наш тип данных и возвращает строку содержащую  имя конструктора и @show@ от каждого его параметра.
  Если нас не устраивает такой вывод мы можем определить @instance Show@ сами (см ниже).
-}
module AST where

-- * Структуры данных
-- \ Начальный нетерминант или корень синтаксического дерева. Программа является списком операторов (утверждений?).
newtype Program = Program [Statement] deriving (Show)

-- \ Операторами являются
data Statement 
  -- \ оператор вывода
  =  Write Expr 
  -- \ оператор присваивания (строка представляет собой имя идентификаторы, которому присваивается 
  | Assign String Expr -- В других компиляторах не принято представлять идентификатор переменной как строку
  -- \ оператор @while@ - конструкция цикла с предусловием (выполняет список операторов, при успешной проверке Cond)
  | While Cond [Statement] 
  -- \ оператор @if@ - конструкция ветвления с возможной веткой else 
  | If Cond [Statement] (Maybe [Statement])
  deriving (Eq, Show)

-- \ Условие состоит в сравнении двух выражений
data Cond = Cond Expr Relation Expr deriving (Eq, Show)

-- \ операторы сравнения
data Relation
  = Eq -- ^ EQual - равенство
  | Ne -- ^ Not Equal - неравенство
  | Lt -- ^ Less Than - строгое меньше
  | Gt -- ^ Great Than - строгое больше
  | Le -- ^ Less or Equal - нестрогое меньше 
  | Ge -- ^ Great or Equal - нестрогое больше 
  deriving (Eq, Enum)

{-
  Здесь есть небольшой трюк. Мы используем реализацию перечислений Enum, вовсе не для того чтобы можно было перечислять
  операторы Eq..Gt (что не имеет особого смысла), а для того чтобы воспользоваться функцией fromEnum которая принимает 
  перечисление и возвращает целое число. Таким образом мы можем удобно соотнести операторы сравнения с их представлением в
  виртуацльной машине: Eq = 0, Ne = 1... Однако при отладке мы не увидим Eq, что не удобно. И порядокобъявления операторов 
  выше влияет на работоспособность компилятора. Вообщем спорное решение.
-}

-- \ Определяем вывод (отображение в строку) оператора сравнения
instance Show Relation where   
    -- \ выводом оператора является вывод его порядкового номера при объявлении выше 
    show r = show $ fromEnum r

-- Выражением являются
data Expr 
  -- \ Идентификатор переменной. (т.е. строка - имя переменной) 
  = Identifier String
  -- \ Просто константа (В милане единственный тип - целое число)
  | Constant Int
  -- \ Оператор чтение со стандартного ввода
  | Read
  -- \ Комбинация из двух выражений связанная бинарным оператором.
  | Binary BinOperator Expr Expr
  -- \ унарный оператор от выражения
  | Unary UnOperator Expr
  -- \ выражение в скобках. Лишено смысла - от скобок нужно будет избавиться на этапе синтаксического разбора.
  | Paren Expr
  deriving (Eq, Show)

-- \ всего один унарный оператор - отрицание
data UnOperator = Neg deriving (Eq, Show)

-- \ бинарные операторы
data BinOperator
  = Add -- ^ сложение
  | Sub -- ^ вычитание
  | Div -- ^ деление
  | Mult -- ^ умножение
  deriving (Eq, Show) 
-- __не путать__ @'Add' и ADD (команда виртуальной машины). 
