-- data keyword helps to define a type. 
--define types
--data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show) 

--function
--surface :: Shape -> Float  
--surface (Circle _ _ r) = pi * r ^ 2  
--surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)  

surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  

nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))


baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)  

--data Person = Person String String Int Float String String deriving (Show)

--firstName :: Person -> String  
--firstName (Person firstname _ _ _ _ _) = firstname  
  
--lastName :: Person -> String  
--lastName (Person _ lastname _ _ _ _) = lastname  
  
--age :: Person -> Int  
--age (Person _ _ age _ _ _) = age  
  
--height :: Person -> Float  
--height (Person _ _ _ height _ _) = height  
  
--phoneNumber :: Person -> String  
--phoneNumber (Person _ _ _ _ number _) = number  
  
--flavor :: Person -> String  
--flavor (Person _ _ _ _ _ flavor) = flavor   

--replaced with this data, for better efficiency
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show) 
					 
--data Car = Car String String Int deriving (Show)
--data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  

data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show)

--tellCar :: Car -> String  
--tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  		

tellCar :: (Show a) => Car String String a -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  


data Maybe a = Nothing | Just a  


data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  

data Persona = Persona { firstNamea :: String  
                     , lastNamea :: String  
                     , agea :: Int  
                     } deriving (Eq, Show, Read)  
					 
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum) 
		   
data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord) 

--infixr 5 :-:  
--data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)  

infixr 5  .++  
(.++) :: List a -> List a -> List a   
Empty .++ ys = ys  
--(x :-: xs) .++ ys = x :-: (xs .++ ys) 

--Tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)  

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right  


data TrafficLight = Red | Yellow | Green  

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  
	
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"

class YesNo a where  
    yesno :: a -> Bool 
	
instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True  
	
instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True  

instance YesNo Bool where  
    yesno = id 

instance YesNo (Prelude.Maybe a) where  
    yesno (Prelude.Just _) = True  
    yesno Prelude.Nothing = False  

instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True 

instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True 


yesnoIf :: (YesNo y) => y -> a -> a -> a  
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult  

data Frank a b  = Frank {frankField :: b a} deriving (Show)  
--various other similar instances tested and answered for...

