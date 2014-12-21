--Sean Staz
--11/02/2014
--CSci_450
-----------------------------------------------------------------------

{- CSci 450/503, Fall 2014
   Homework #4: Sandwich DSL
   H. Conrad Cunningham
   27 Ocotber 2014

1234567890123456789012345678901234567890123456789012345678901234567890

This is the SandwichDSL base code from the case study. It can be
expanded to build the module for Assignment #4.

-}

module SandwichDSL
where

-- Used functions from these modules in my implementation
import Data.Maybe
import Data.List

{- Haskell data type definitions from "Building the DSL" -}

data Platter   = Platter [Sandwich] 
                 deriving Show

data Sandwich  = Sandwich [Layer]
                 deriving Show

data Layer     = Bread Bread         | Meat Meat           |
                 Cheese Cheese       | Vegetable Vegetable | 
                 Condiment Condiment
                 deriving (Eq,Show)

data Bread     = White | Wheat | Rye
                 deriving (Eq,Show)

data Meat      = Turkey | Chicken | Ham | RoastBeef | Tofu
                 deriving (Eq,Show)

data Cheese    = American | Swiss | Jack | Cheddar
                 deriving (Eq,Show)

data Vegetable = Tomato | Onion | Lettuce | BellPepper
                 deriving (Eq,Show)

data Condiment = Mayo | Mustard | Ketchup | Relish | Tabasco
                 deriving (Eq,Show)

-- Function type signatures given in section
-- newSandwich :: Bread -> Sandwich
-- addLayer ::    Sandwich -> Layer -> Sandwich
-- newPlatter ::  Platter
-- addSandwich :: Platter -> Sandwich -> Platter


{- Haskell data type definitions from 
   "Compiling the Program for the SueChef Controller"
-}

data SandwichOp = StartSandwich    | FinishSandwich |
                  AddBread Bread   | AddMeat Meat   |
                  AddCheese Cheese | AddVegetable Vegetable | 
                  AddCondiment Condiment |
                  StartPlatter | MoveToPlatter | FinishPlatter 
                  deriving (Eq, Show) 

data Program = Program [SandwichOp]
               deriving Show
               

prices = [(Bread White,20),(Bread Wheat,30),(Bread Rye,30), 
          (Meat Turkey,100),(Meat Chicken,80),(Meat Ham,120),
          (Meat RoastBeef,140),(Meat Tofu,50),
          (Cheese American,50),(Cheese Swiss,60),
          (Cheese Jack,60),(Cheese Cheddar,60),
          (Vegetable Tomato,25),(Vegetable Onion,20),
          (Vegetable Lettuce,20),(Vegetable BellPepper,25),
          (Condiment Mayo,5),(Condiment Mustard,4),
          (Condiment Ketchup,4),(Condiment Relish,10),
          (Condiment Tabasco,5) 
         ]

----whiH :: Sandwich
----whiH = Sandwich[Bread White, Meat Ham, Bread White]

----wheTJ :: Sandwich
----wheTJ = Sandwich[Bread Wheat, Meat Tofu, Cheese Jack, Bread Wheat]

----rTSO :: Sandwich
----rTSO = Sandwich[Bread Rye, Meat Turkey, Cheese Swiss, Vegetable Onion]

----whiCALMa :: Sandwich
----whiCALMa = Sandwich[Bread White, Meat Chicken, Cheese American, Vegetable Lettuce, Condiment Mayo]
    
------------ End of SandwichDSL_base --------------------------------------------------------------------

newSandwich :: Bread -> Sandwich
newSandwich bread = Sandwich[Bread bread]

--newSandwich2 :: Bread -> Cheese -> Sandwich
--newSandwich2 bread chz = Sandwich[Bread bread, Cheese chz]

outputSandwich :: Sandwich -> Sandwich
outputSandwich sandwich = sandwich

addLayer ::    Sandwich -> Layer -> Sandwich
addLayer (Sandwich head) layer = Sandwich(layer : head)

newPlatter ::  Platter
newPlatter = Platter []

addSandwich :: Platter -> Sandwich -> Platter
addSandwich (Platter platter) sandwich = Platter(sandwich : platter)

isBread :: Layer -> Bool
isBread (Bread b)          = True
isBread notBread           = False

isMeat :: Layer -> Bool
isMeat (Meat b)            = True
isMeat notMeat             = False

isCheese :: Layer -> Bool
isCheese (Cheese b)        = True
isCheese notCheese         = False

isVegetable :: Layer -> Bool
isVegetable (Vegetable b)  = True
isVegetable notVegetable   = False

isCondiment :: Layer -> Bool
isCondiment (Condiment b)  = True
isCondiment notCondiment   = False

noMeat :: Sandwich -> Bool
noMeat (Sandwich meat) = searchMeat == []
                        where searchMeat = filter isMeat meat


eqSandwich :: Sandwich -> Sandwich -> Bool
eqSandwich (Sandwich sandwich1) (Sandwich sandwich2) = sandwich1 `intersect` sandwich2 == sandwich1

priceSandwich :: Sandwich -> Int
priceSandwich (Sandwich layers) = total (layers) 0
        where total :: [Layer] -> Int -> Int
              total [] cents   = cents
              total (x:xs) sum = total xs (search + sum)
                               where search = fromJust(lookup x prices)
---------------------------------------------------------------------------------------------------------


test_priceSandwich =
        do
        putStrLn "Testing -> priceSandwich"
        putStrLn ""
        ---------------------------
        putStr "Testing: priceSandwich (Sandwich [Meat Ham])"
        putStrLn ""
        putStr "    "
        putStrLn (show (priceSandwich (Sandwich [Meat Ham])))
        putStrLn ""
        ---------------------------
        putStr "Testing: priceSandwich (Sandwich [Bread Rye])"
        putStrLn ""
        putStr "    "
        putStrLn (show (priceSandwich (Sandwich [Bread Rye])))
        putStrLn ""
        ---------------------------  
        putStr "Testing: priceSandwich (Sandwich [Bread Rye, Cheese Jack])"
        putStrLn ""
        putStr "    "
        putStrLn (show (priceSandwich (Sandwich [Bread Rye, Cheese Jack])))
        putStrLn ""
        ---------------------------  
        putStr "Testing: priceSandwich (Sandwich [Bread Rye, Cheese Jack, Meat Ham])"
        putStrLn ""
        putStr "    "
        putStrLn (show (priceSandwich (Sandwich [Bread Rye, Cheese Jack, Meat Ham])))
        putStrLn ""
        ---------------------------  

test_eqSandwich =
        do
        putStrLn "Testing -> eqSandwich"
        putStrLn ""
        ---------------------------
        putStr "Testing: eqSandwich (Sandwich [Bread Rye, Meat Ham, Bread Rye]) (Sandwich [Bread Rye, Meat Ham, Bread Rye])"
        putStrLn ""
        putStr "    "
        putStrLn (show (eqSandwich (Sandwich [Bread Rye, Meat Ham, Bread Rye]) (Sandwich [Bread Rye, Meat Ham, Bread Rye])))
        putStrLn ""
        ---------------------------
        putStr "Testing: eqSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])"
        putStrLn ""
        putStr "    "
        putStrLn (show (eqSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])))
        putStrLn ""
        ---------------------------  
        putStr "Testing: eqSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Vegetable Lettuce, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])"
        putStrLn ""
        putStr "    "
        putStrLn (show (eqSandwich (Sandwich[Bread White, Meat Ham, Cheese Jack, Vegetable Lettuce, Bread White]) (Sandwich[Bread White, Cheese Jack, Meat Ham, Bread White])))
        putStrLn ""
        ---------------------------  
        
test_noMeat5 =
        do
        putStrLn "Testing -> noMeat"
        putStrLn ""
        ---------------------------
        putStr "Testing: noMeat (Sandwich [Meat Ham])"
        putStrLn ""
        putStr "    "
        putStrLn (show (noMeat (Sandwich [Meat Ham])))
        putStrLn ""
        ---------------------------
        putStr "Testing: noMeat (Sandwich [Bread Rye])"
        putStrLn ""
        putStr "    "
        putStrLn (show (noMeat (Sandwich [Bread Rye])))
        putStrLn ""
        ---------------------------  
        putStr "Testing: noMeat (Sandwich [Bread Rye, Cheese Jack])"
        putStrLn ""
        putStr "    "
        putStrLn (show (noMeat (Sandwich [Bread Rye, Cheese Jack])))
        putStrLn ""
        ---------------------------  
        putStr "Testing: noMeat (Sandwich [Bread Rye, Cheese Jack, Meat Ham])"
        putStrLn ""
        putStr "    "
        putStrLn (show (noMeat (Sandwich [Bread Rye, Cheese Jack, Meat Ham])))
        putStrLn ""
        ---------------------------  

test_isBread =
        do
        putStrLn "Testing -> isBread"
        putStrLn ""
        ---------------------------
        putStr "Testing: isBread (Bread White)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isBread (Bread White)))
        putStrLn ""
        ---------------------------
        putStr "Testing: isBread (Cheese Jack)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isBread (Cheese Jack)))
        putStrLn ""
        ---------------------------        
        putStr "Testing: isBread (Meat Ham)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isBread (Meat Ham)))
        putStrLn ""
        ---------------------------
        putStr "Testing: isBread (Vegetable Onion)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isBread (Vegetable Onion)))
        putStrLn ""
        ---------------------------  
        putStr "Testing: isBread (Condiment Mayo)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isBread (Condiment Mayo)))
        putStrLn ""
        ---------------------------     

test_isMeat =
        do
        putStrLn "Testing -> isMeat"
        putStrLn ""
        ---------------------------
        putStr "Testing: isMeat (Bread White)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isMeat (Bread White)))
        putStrLn ""
        ---------------------------
        putStr "Testing: isMeat (Cheese Jack)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isMeat (Cheese Jack)))
        putStrLn ""
        ---------------------------        
        putStr "Testing: isMeat (Meat Ham)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isMeat (Meat Ham)))
        putStrLn ""
        ---------------------------
        putStr "Testing: isMeat (Vegetable Onion)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isMeat (Vegetable Onion)))
        putStrLn ""
        ---------------------------  
        putStr "Testing: isMeat (Condiment Mayo)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isMeat (Condiment Mayo)))
        putStrLn ""
        ---------------------------           

test_isCheese =
        do
        putStrLn "Testing -> isCheese"
        putStrLn ""
        ---------------------------
        putStr "Testing: isCheese (Bread White)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isCheese (Bread White)))
        putStrLn ""
        ---------------------------
        putStr "Testing: isCheese (Cheese Jack)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isCheese (Cheese Jack)))
        putStrLn ""
        ---------------------------        
        putStr "Testing: isCheese (Meat Ham)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isCheese (Meat Ham)))
        putStrLn ""
        ---------------------------
        putStr "Testing: isCheese (Vegetable Onion)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isCheese (Vegetable Onion)))
        putStrLn ""
        ---------------------------  
        putStr "Testing: isCheese (Condiment Mayo)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isCheese (Condiment Mayo)))
        putStrLn ""
        ---------------------------    
        
test_isVegetable =
        do
        putStrLn "Testing -> isVegetable"
        putStrLn ""
        ---------------------------
        putStr "Testing: isVegetable (Bread White)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isVegetable (Bread White)))
        putStrLn ""
        ---------------------------
        putStr "Testing: isVegetable (Cheese Jack)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isVegetable (Cheese Jack)))
        putStrLn ""
        ---------------------------        
        putStr "Testing: isVegetable (Meat Ham)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isVegetable (Meat Ham)))
        putStrLn ""
        ---------------------------
        putStr "Testing: isVegetable (Vegetable Onion)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isVegetable (Vegetable Onion)))
        putStrLn ""
        ---------------------------  
        putStr "Testing: isVegetable (Condiment Mayo)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isVegetable (Condiment Mayo)))
        putStrLn ""
        ---------------------------  
        
test_isCondiment =
        do
        putStrLn "Testing -> isCondiment"
        putStrLn ""
        ---------------------------
        putStr "Testing: isCondiment (Bread White)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isCondiment (Bread White)))
        putStrLn ""
        ---------------------------
        putStr "Testing: isCondiment (Cheese Jack)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isCondiment (Cheese Jack)))
        putStrLn ""
        ---------------------------        
        putStr "Testing: isCondiment (Meat Ham)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isCondiment (Meat Ham)))
        putStrLn ""
        ---------------------------
        putStr "Testing: isCondiment (Vegetable Onion)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isCondiment (Vegetable Onion)))
        putStrLn ""
        ---------------------------  
        putStr "Testing: isCondiment (Condiment Mayo)"
        putStrLn ""
        putStr "    "
        putStrLn (show (isCondiment (Condiment Mayo)))
        putStrLn ""
        --------------------------- 
test_newSandwich =
        do
        putStrLn "Testing -> newSandwich"
        putStrLn ""
        ---------------------------
        --putStr "Testing: newSandwich _"
        --putStrLn (show (newSandwich      ))
        ---------------------------
        putStr "Testing: newSandwich White"
        putStrLn ""
        putStr "    "
        putStrLn (show (newSandwich White))
        putStrLn ""
        ---------------------------
        putStr "Testing: newSandwich Wheat"
        putStrLn ""
        putStr "    "
        putStrLn (show (newSandwich Wheat))
        putStrLn ""
        ---------------------------
        putStr "Testing: newSandwich Rye"
        putStrLn ""
        putStr "    "
        putStrLn (show (newSandwich Rye))
        putStrLn ""
        ---------------------------
        
test_newPlatter =
        do
        putStrLn "Testing -> newPlatter"
        putStrLn ""
        ---------------------------
        putStr "Testing: newPlatter"
        putStrLn ""
        putStr "    "
        putStrLn (show (newPlatter))
        putStrLn ""
        ---------------------------
        
test_addLayer =
        do
        putStrLn "Testing -> addLayer"
        putStrLn ""
        ---------------------------
        putStr "Testing: addLayer (newSandwich White)(Meat Turkey)"
        putStrLn ""
        putStr "    "
        putStrLn (show (addLayer (newSandwich White)(Meat Turkey)))
        putStrLn ""        
        ---------------------------
        putStr "Testing: addLayer (newSandwich White)(Cheese Jack)"
        putStrLn ""
        putStr "    "
        putStrLn (show (addLayer (newSandwich White)(Cheese Jack)))
        putStrLn "" 
        ---------------------------
        putStr "Testing: addLayer (Sandwich [Meat Turkey,Bread White])(Vegetable Onion)"
        putStrLn ""
        putStr "    "
        putStrLn (show (addLayer (Sandwich [Meat Turkey,Bread White])(Vegetable Onion)))
        putStrLn ""        
        ---------------------------
        
test_addSandwich =
        do
        putStrLn "Testing -> addSandwich"
        putStrLn ""
        ---------------------------
        putStr "Testing: addSandwich (newPlatter)(newSandwich White)"
        putStrLn ""
        putStr "    "
        putStrLn (show (addSandwich (newPlatter)(newSandwich White)))
        putStrLn ""        
        ---------------------------
        putStr "Testing: addSandwich (newPlatter)(Sandwich [Vegetable Onion,Meat Turkey,Bread White])"
        putStrLn ""
        putStr "    "
        putStrLn (show (addSandwich (newPlatter)(Sandwich [Vegetable Onion,Meat Turkey,Bread White])))
        putStrLn "" 
        ---------------------------
        putStr "Testing: addSandwich (newPlatter) (addLayer (Sandwich [Meat Turkey,Bread White])(Vegetable Onion))"
        putStrLn ""
        putStr "    "
        putStrLn (show (addSandwich (newPlatter) (addLayer (Sandwich [Meat Turkey,Bread White])(Vegetable Onion))))
        putStrLn ""        
        ---------------------------
