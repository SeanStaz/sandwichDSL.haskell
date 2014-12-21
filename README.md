sandwichDSL.haskell
===================

Assignment #4 for CSci 450

CSci 450-01: Organization of Programming Languages
CSci 503-01: Fundamental Concepts in Languages
Fall 2014

Assignment #4: Sandwich DSL
Deadline: Friday, 7 November, 11:59 p.m.

    This assignment requires you to complete several exercises from the Sandwich DSL Case Study. You may download the Haskell file SandwichDSL_base.hs to begin your work.

    CSci 503 students: Implement all programming exercises in Exercise Sets 1 and 2 except that you may choose to omit one of the functions inOSO, intoOSO, or eqSandwich.

    CSci 450 students: Do all the exercises above except you may choose to omit two of the functions inOSO, intoOSO, or eqSandwich.

    Please format and document your program source code appropriately.

    Test your programs appropriately and thoroughly. Show your tests in an IO program as I did in my solution to Assignment #1.

    When this assignment is complete, submit your program source code file to Blackboard. Be sure that you identify yourself and the assignments in comments in the source file.

    Also submit a paper copy of the above files at the following class meeting.

UP to CSci 450-01 assignments document?

Copyright © 2014, H. Conrad Cunningham
Last modified: Wed Oct 29 19:54:10 CDT 2014

______________________________________________________________________________________________________________________

 Sandwich DSL Case Study and Exercises
Building the DSL

Suppose Emerald de Gassy, the owner of the Oxford-based catering business Deli-Gate, hires us to design a domain-specific language (DSL) for describing sandwich platters. The DSL scripts will direct Deli-Gate's robotic kitchen appliance SueChef (Sandwich and Utility Electronic Chef) to assemble platters of sandwiches.

In discussing the problem with Emerald and the Deli-Gate staff, we discover the following:

    A sandwich platter consists of zero or more sandwiches. (Zero? Why not! Although a platter with no sandwiches may not be a useful, or profitable, case, there does not seem to be any harm in allowing this degenerate case. It may simplify some of the coding and representation.)
    Each sandwich consists of layers of ingredients.
    The categories of ingredients are breads, meats, cheeses, vegetables, and condiments.
    Available breads are white, wheat, and rye.
    Available meats are turkey, chicken, ham, roast beef, and tofu. (Okay, tofu is not a meat, but it is a good protein source for those who do not wish to eat meat. This is a college town after all. Oh, there is also a special meat served for football games Thanksgiving week called "bulldog", but it is really just chicken, so we can ignore that choice for our purposes here.)
    Available cheeses are American, Swiss, jack, and cheddar.
    Available vegetables are tomato, lettuce, onion, and bell pepper.
    Available condiments are mayo, mustard, relish, and Tabasco. (Of course, this being the South, the mayo is Blue Plate Mayonnaise and the mustard is a Creole mustard.)

Let's define this as an internal DSL--in particular, by using a relatively deep embedding.

What is a sandwich? ... Basically, it is a stack of ingredients.

Should we require the sandwich to have a bread on the bottom? ... Probably. ... On the top? Maybe not, to allow "open-faced" sandwiches. ... What can the SueChef build? ... We don't know at this point, but let's assume it can stack up any ingredients without restriction.

For simplicity and flexibility, let's define a Haskell data type Sandwich to model sandwiches. It wraps a possibly empty list of ingredient layers. We assume the head of the list to be the layer at the top of the sandwich. We derive Show so we can display sandwiches.

    data Sandwich = Sandwich [Layer]
                    deriving Show

Note: In this case study, we use the same name for both algebraic data types and constructors within them. Above the Sandwich after data defines a type and the one after the "=" defines the single constructor for that type.

Note: Sandwich is the specification for a sandwich. When "executed" by the SueChef, it results in the assembly of a sandwich that satisfies the specification.

As defined, the Sandwich data type does not require there to be a bread in the stack of ingredients. However, we add function newSandwich that starts a sandwich with a bread at the bottom and a function addLayer that adds a new ingredient to the top of the sandwich. We leave the implementation of these functions as exercises.

    newSandwich :: Bread -> Sandwich
    addLayer    :: Sandwich -> Layer -> Sandwich

Ingredients are in one of five categories: breads, meats, cheeses, vegetables, and condiments. Because both the categories and the specific type of ingredient is important, we choose to represent both in the type structures and define the following types. A value of type Layer represents a single ingredient. Note that we use names such as Bread both as a constructor of the Layer type and the type of the ingredients within that category.

    data Layer     = Bread Bread         | Meat Meat           |
                     Cheese Cheese       | Vegetable Vegetable | 
                     Condiment Condiment
                     deriving (Eq, Show)

    data Bread     = White | Wheat | Rye
                     deriving (Eq, Show)
    
    data Meat      = Turkey | Chicken | Ham | RoastBeef | Tofu
                     deriving (Eq, Show)

    data Cheese    = American | Swiss | Jack | Cheddar
                     deriving (Eq, Show)

    data Vegetable = Tomato | Onion | Lettuce | BellPepper
                     deriving (Eq, Show)

    data Condiment = Mayo | Mustard | Ketchup | Relish | Tabasco
                     deriving (Eq, Show)

We need to be able to compare ingredients for equality. Because the default definitions are appropriate, we derive both classes Show and Eq for these ingredient types. We did not derive Eq for Sandwich because the default element-by-element equality of lists does not seem to be the appropriate equality comparison for sandwiches.

To complete the model, we define type Platter to wrap a list of sandwiches.

    data Platter = Platter [Sandwich] 
                   deriving Show

We also define functions newPlatter to create a new Platter and addSandwich to add a sandwich to the Platter. We leave the implementation of these functions as exercises.

    newPlatter  :: Platter 
    addSandwich :: Platter -> Sandwich -> Platter 

Exercise Set 1

Please put these functions in a Haskell module SandwichDSL. You may use functions defined earlier in the exercises to implement those later in the exercises.

    Define the Haskell functions newSandwich, addLayer, newPlatter, and addSandwich described above.

    Define the Haskell query functions below that take an ingredient (i.e., Layer) and return True if and only if the ingredient is in the specified category.

    isBread     :: Layer -> Bool
    isMeat      :: Layer -> Bool
    isCheese    :: Layer -> Bool
    isVegetable :: Layer -> Bool
    isCondiment :: Layer -> Bool

    Define a Haskell function noMeat that takes a sandwich and returns True if and only if the sandwich contains no meats.

    noMeat :: Sandwich -> Bool

    According to a proposed City of Oxford ordinance, in the future it may be necessary to assemble all sandwiches in Oxford Standard Order (OSO): a slice of bread on the bottom, then zero or more meats layered above that, then zero or more cheeses, then zero or more vegetables, then zero or more condiments, and then a slice of bread on top. The top and bottom slices of bread must be of the same type. Define a Haskell function inOSO that takes a sandwich and determines whether it is in OSO and another function intoOSO that takes a sandwich and a default bread and returns the sandwich with the same ingredients ordered in OSO.

    inOSO   :: Sandwich -> Bool
    intoOSO :: Sandwich -> Bread -> Sandwich

    Hint: Remember prelude functions like dropWhile.
    Note: It is impossible to rearrange the layers into OSO if the sandwich does not include exactly two breads of the same type. If the sandwich does not include any breads, then the default bread type (second argument) should be specified for both. If there is at least one bread, then the bread type nearest the bottom can be chosen for both top and bottom.

    Assuming that the price for a sandwich is the sum of the prices of the individual ingredients, define a Haskell function priceSandwich that takes a sandwich and returns its price. You may use the price database prices given below.

    priceSandwich :: Sandwich -> Int 

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

    Hint: Consider use of the lookup function in the prelude. The library Data.Maybe may also include helpful functions.

    Define a Haskell function eqSandwich that compares two sandwichs for equality. (What does equality mean for sandwiches?) Then give the declaration needed to make Sandwich an instance of class Eq.

    eqSandwich :: Sandwich -> Sandwich -> Bool 

    Hint: The "sets" operations in library Data.List might be helpful.

Compiling the Program for the SueChef Controller

In this section, we look at compiling the Platter and Sandwich descriptions to issue a sequence of commands for the SueChef's controller.

The SueChef supports the special instructions that can be issued in sequence to its controller. The data type SandwichOp below represents the instructions.

    data SandwichOp = StartSandwich    | FinishSandwich |
                      AddBread Bread   | AddMeat Meat   |
                      AddCheese Cheese | AddVegetable Vegetable | 
                      AddCondiment Condiment |
                      StartPlatter | MoveToPlatter | FinishPlatter 
                      deriving (Eq, Show) 

We also define the type Program to represent the sequence of commands resulting from compilation of a Sandwich or Platter specification.

    data Program = Program [SandwichOp]
                   deriving Show

The flow of a program is given by the following pseudocode:

    StartPlatter
    for each sandwich needed
        StartSandwich
        for each ingredient needed
            Add ingredient on top
        FinishSandwich
        MoveToPlatter
    FinishPlatter

Consider a sandwich defined as follows:

    Sandwich [Bread Rye,Condiment Mayo,Cheese Swiss,
              Meat Ham,Bread Rye]

The corresponding sequence of SueChef commands would be the following.

    [StartSandwich,AddBread Rye,AddMeat Ham,AddCheeseSwiss,
     AddCondiment Mayo,AddBread Rye,FinishSandwich,MoveToPlatter]

Correction: The original handout distributed in class on 27 October 2014 had the order of the ingredients reversed in the sequence of instructions.
Exercise Set 2

    Define a Haskell function compileSandwich to convert a sandwich specification into the sequence of SueChef commands to assemble the sandwich.

    compileSandwich :: Sandwich -> [SandwichOp]

    Define a Haskell function compile to convert a platter specification into the sequence of SueChef commands to assemble the sandwiches on the platter.

    compile :: Platter -> Program
