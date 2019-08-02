context("Printing")


# Set testing objects
set.seed(112)
fruits <- sample(c("apple", "banana", "cherry", "orange", "pineapple", "really very long fruit name"), 100, replace = T)

# For testing maxWidth
maxWidth50 <- "really ver~ | cherry      | cherry     
banana      | really ver~ | pineapple  
orange      | banana      | really ver~
pineapple   | apple       | apple      
really ver~ | cherry      | pineapple  
apple       | orange      | banana     
really ver~ | orange      | really ver~
orange      | banana      | cherry     
pineapple   | really ver~ | orange     
cherry      | pineapple   | pineapple  
apple       | orange      | really ver~
really ver~ | orange      | orange     
orange      | orange      | banana     
apple       | banana      | banana     
really ver~ | cherry      | cherry     
banana      | orange      | really ver~
apple       | apple       | banana     
pineapple   | cherry      | cherry     
pineapple   | apple       | apple      
cherry      | pineapple   | cherry     
cherry      | cherry      | orange     
apple       | banana      | really ver~
apple       | really ver~ | really ver~
cherry      | apple       | really ver~
cherry      | pineapple   | cherry     
cherry      | really ver~ | pineapple  
apple       | banana      | pineapple  
banana      | orange      | cherry     
banana      | really ver~ | really ver~
really ver~ | cherry      | cherry     
orange      | banana      | orange     
really ver~ | really ver~ | banana     
banana      | orange      |            
really ver~ | orange      |            "
maxWidth100 <- "really ver~ | banana      | orange      | orange      | banana      | cherry      | really ver~
banana      | apple       | really ver~ | orange      | orange      | orange      | really ver~
orange      | pineapple   | banana      | banana      | really ver~ | pineapple   | cherry     
pineapple   | pineapple   | really ver~ | cherry      | cherry      | really ver~ | pineapple  
really ver~ | cherry      | cherry      | orange      | banana      | orange      | pineapple  
apple       | cherry      | really ver~ | apple       | really ver~ | banana      | cherry     
really ver~ | apple       | banana      | cherry      | orange      | banana      | really ver~
orange      | apple       | apple       | apple       | orange      | cherry      | cherry     
pineapple   | cherry      | cherry      | pineapple   | cherry      | really ver~ | orange     
cherry      | cherry      | orange      | cherry      | pineapple   | banana      | banana     
apple       | cherry      | orange      | banana      | really ver~ | cherry      |            
really ver~ | apple       | banana      | really ver~ | apple       | apple       |            
orange      | banana      | really ver~ | apple       | pineapple   | cherry      |            
apple       | banana      | pineapple   | pineapple   | banana      | orange      |            
really ver~ | really ver~ | orange      | really ver~ | really ver~ | really ver~ |            "
maxWidth150 <- "really ver~ | apple       | cherry      | orange      | orange      | apple       | banana      | really ver~ | banana      | really ver~
banana      | really ver~ | apple       | really ver~ | banana      | cherry      | orange      | apple       | banana      | really ver~
orange      | orange      | apple       | banana      | really ver~ | apple       | really ver~ | pineapple   | cherry      | cherry     
pineapple   | apple       | cherry      | really ver~ | pineapple   | pineapple   | cherry      | banana      | really ver~ | pineapple  
really ver~ | really ver~ | cherry      | cherry      | orange      | cherry      | banana      | really ver~ | banana      | pineapple  
apple       | banana      | cherry      | really ver~ | orange      | banana      | really ver~ | cherry      | cherry      | cherry     
really ver~ | apple       | apple       | banana      | orange      | really ver~ | orange      | orange      | apple       | really ver~
orange      | pineapple   | banana      | apple       | banana      | apple       | orange      | pineapple   | cherry      | cherry     
pineapple   | pineapple   | banana      | cherry      | cherry      | pineapple   | cherry      | really ver~ | orange      | orange     
cherry      | cherry      | really ver~ | orange      | orange      | really ver~ | pineapple   | orange      | really ver~ | banana     "

# For testing maxLen
maxLen5 <- "reall~ | apple  | cherr~ | orang~ | orang~ | apple  | banan~ | reall~ | banan~ | reall~
banan~ | reall~ | apple  | reall~ | banan~ | cherr~ | orang~ | apple  | banan~ | reall~
orang~ | orang~ | apple  | banan~ | reall~ | apple  | reall~ | pinea~ | cherr~ | cherr~
pinea~ | apple  | cherr~ | reall~ | pinea~ | pinea~ | cherr~ | banan~ | reall~ | pinea~
reall~ | reall~ | cherr~ | cherr~ | orang~ | cherr~ | banan~ | reall~ | banan~ | pinea~
apple  | banan~ | cherr~ | reall~ | orang~ | banan~ | reall~ | cherr~ | cherr~ | cherr~
reall~ | apple  | apple  | banan~ | orang~ | reall~ | orang~ | orang~ | apple  | reall~
orang~ | pinea~ | banan~ | apple  | banan~ | apple  | orang~ | pinea~ | cherr~ | cherr~
pinea~ | pinea~ | banan~ | cherr~ | cherr~ | pinea~ | cherr~ | reall~ | orang~ | orang~
cherr~ | cherr~ | reall~ | orang~ | orang~ | reall~ | pinea~ | orang~ | reall~ | banan~"
maxLen20 <- "really very long fru~ | cherry                | apple                 | cherry               
banana                | apple                 | cherry                | orange               
orange                | banana                | apple                 | pineapple            
pineapple             | banana                | pineapple             | really very long fru~
really very long fru~ | really very long fru~ | cherry                | orange               
apple                 | orange                | banana                | banana               
really very long fru~ | really very long fru~ | really very long fru~ | banana               
orange                | banana                | apple                 | cherry               
pineapple             | really very long fru~ | pineapple             | really very long fru~
cherry                | cherry                | really very long fru~ | banana               
apple                 | really very long fru~ | banana                | cherry               
really very long fru~ | banana                | orange                | apple                
orange                | apple                 | really very long fru~ | cherry               
apple                 | cherry                | cherry                | orange               
really very long fru~ | orange                | banana                | really very long fru~
banana                | orange                | really very long fru~ | really very long fru~
apple                 | banana                | orange                | really very long fru~
pineapple             | really very long fru~ | orange                | cherry               
pineapple             | pineapple             | cherry                | pineapple            
cherry                | orange                | pineapple             | pineapple            
cherry                | orange                | really very long fru~ | cherry               
apple                 | orange                | apple                 | really very long fru~
apple                 | banana                | pineapple             | cherry               
cherry                | cherry                | banana                | orange               
cherry                | orange                | really very long fru~ | banana               "

# For testing order
orderSort <- "apple | apple  | banana | cherry | cherry | orange    | pineapple   | really ver~ | really ver~
apple | banana | banana | cherry | orange | orange    | pineapple   | really ver~ | really ver~
apple | banana | banana | cherry | orange | orange    | pineapple   | really ver~ | really ver~
apple | banana | banana | cherry | orange | orange    | pineapple   | really ver~ | really ver~
apple | banana | banana | cherry | orange | orange    | pineapple   | really ver~ |            
apple | banana | cherry | cherry | orange | orange    | pineapple   | really ver~ |            
apple | banana | cherry | cherry | orange | pineapple | really ver~ | really ver~ |            
apple | banana | cherry | cherry | orange | pineapple | really ver~ | really ver~ |            
apple | banana | cherry | cherry | orange | pineapple | really ver~ | really ver~ |            
apple | banana | cherry | cherry | orange | pineapple | really ver~ | really ver~ |            
apple | banana | cherry | cherry | orange | pineapple | really ver~ | really ver~ |            
apple | banana | cherry | cherry | orange | pineapple | really ver~ | really ver~ |            "
orderShort <- "apple | apple  | banana | orange | orange | orange    | pineapple   | really ver~ | really ver~
apple | banana | banana | banana | cherry | cherry    | pineapple   | really ver~ | really ver~
apple | orange | orange | cherry | banana | cherry    | pineapple   | really ver~ | really ver~
apple | orange | banana | orange | cherry | cherry    | pineapple   | really ver~ | really ver~
apple | cherry | cherry | cherry | orange | orange    | pineapple   | really ver~ |            
apple | orange | banana | cherry | orange | banana    | pineapple   | really ver~ |            
apple | banana | cherry | banana | banana | pineapple | really ver~ | really ver~ |            
apple | cherry | orange | banana | banana | pineapple | really ver~ | really ver~ |            
apple | cherry | orange | orange | cherry | pineapple | really ver~ | really ver~ |            
apple | cherry | banana | cherry | banana | pineapple | really ver~ | really ver~ |            
apple | cherry | orange | banana | cherry | pineapple | really ver~ | really ver~ |            
apple | cherry | orange | orange | cherry | pineapple | really ver~ | really ver~ |            "


# For testing printOut
outputRows <- c(
  "really ver~ | banana      | orange      | orange      | banana      | cherry      | really ver~",
  "banana      | apple       | really ver~ | orange      | orange      | orange      | really ver~",
  "orange      | pineapple   | banana      | banana      | really ver~ | pineapple   | cherry     ",
  "pineapple   | pineapple   | really ver~ | cherry      | cherry      | really ver~ | pineapple  ",
  "really ver~ | cherry      | cherry      | orange      | banana      | orange      | pineapple  ",
  "apple       | cherry      | really ver~ | apple       | really ver~ | banana      | cherry     ",
  "really ver~ | apple       | banana      | cherry      | orange      | banana      | really ver~",
  "orange      | apple       | apple       | apple       | orange      | cherry      | cherry     ",
  "pineapple   | cherry      | cherry      | pineapple   | cherry      | really ver~ | orange     ",
  "cherry      | cherry      | orange      | cherry      | pineapple   | banana      | banana     ",
  "apple       | cherry      | orange      | banana      | really ver~ | cherry      |            ",
  "really ver~ | apple       | banana      | really ver~ | apple       | apple       |            ",
  "orange      | banana      | really ver~ | apple       | pineapple   | cherry      |            ",
  "apple       | banana      | pineapple   | pineapple   | banana      | orange      |            ",
  "really ver~ | really ver~ | orange      | really ver~ | really ver~ | really ver~ |            "
)

# Testing single line output
singleLine <- "the | of | and | a | to | in | is | you | that | it | he | was | for | on | are | as | with | his | they | I | at | be | this | have | from"


# Test vecPrint
test_that("vecPrint", {
  
  # Test maxWidth
  expect_output(vec_print(fruits, maxWidth = 50), maxWidth50)
  expect_output(vec_print(fruits, maxWidth = 100), maxWidth100)
  expect_output(vec_print(fruits, maxWidth = 150), maxWidth150)
  
  # Test maxLen
  expect_output(vec_print(fruits, maxWidth = 100, maxLen = 5), maxLen5)
  expect_output(vec_print(fruits, maxWidth = 100, maxLen = 20), maxLen20)
  
  # Test order
  expect_output(vec_print(fruits, maxWidth = 100, order = "sort"), orderSort)
  expect_output(vec_print(fruits, maxWidth = 100, order = "short"), orderShort)
  
  # Test order equality
  expect_equal(vec_print(fruits, maxWidth = 100, printOut = FALSE),
               vec_print(fruits, maxWidth = 100, order = "none", printOut = FALSE))
  expect_equal(vec_print(sort(fruits), maxWidth = 100, printOut = FALSE),
               vec_print(fruits, maxWidth = 100, order = "sort", printOut = FALSE))
  expect_equal(vec_print(fruits, maxWidth = 100, order = "short", printOut = FALSE),
               vec_print(fruits, maxWidth = 100, order = "shortest", printOut = FALSE))
  
  # Test printOut
  expect_equal((vec_print(fruits, maxWidth = 100, printOut = FALSE)), outputRows)
  
  # Test max.print limit
  mp <- options(max.print = 10)
  expect_message(capture_output(vec_print(fruits, maxWidth = 50)),
                 " [ reached getOption('max.print') -- omitted 24 entries ]")
  expect_silent(vec_print(fruits, maxWidth = 50, printOut = FALSE))
  options(mp)
  
  # Test single line output
  expect_output(vec_print(lexicon::sw_fry_25, maxWidth = 200), singleLine)
  
})
