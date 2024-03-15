
# INTRO -------------------------------------------------------------------

# coding exercise: not_quite_lisp
# source:          https://adventofcode.com/2015/day/1


## PROBLEM -------------------------------------------------

#' Santa is trying to deliver presents in a large apartment building, but he can't find the right floor - 
#' the directions he got are a little confusing. 
#' 
#' He starts on the ground floor (floor 0) and then follows the instructions one character at a time.
#' 
#' An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ), means he should go down one floor.
#' 
#' The apartment building is very tall, and the basement is very deep; he will never find the top or bottom floors.
#' 
#' For example:
#' 
#' (()) and ()() both result in floor 0.
#' ((( and (()(()( both result in floor 3.
#' ))((((( also results in floor 3.
#' ()) and ))( both result in floor -1 (the first basement level).
#' ))) and )())()) both result in floor -3.
#' To what floor do the instructions take Santa?

puzzle_input <- "main/00_data/not_quite_lisp.txt"



## YOUR SOLUTION -------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# start with puzzle_input
puzzle_input %>% 
  
  # read as data.table
  data.table::fread(header = FALSE) %>% 
  
  # extract as vector
  .[, V1] %>% 
  
  # count occurrences of each type of parenthesis
  str_count(pattern = c("\\(", "\\)")) %>% 
  
  # set open parenthesis as +1 and close parenthesis as -1
  {. * c(+1, -1)} %>% 
  
  # take sum
  sum()

# the answer is 232



# PROBLEM -----------------------------------------------------------------

#' Now, given the same instructions, find the position of the first character that causes him to enter the basement 
#' (floor -1). The first character in the instructions has position 1, the second character has position 2, and so on.
#' 
#' For example:
#'   
#' ) causes him to enter the basement at character position 1.
#' ()()) causes him to enter the basement at character position 5.
#' 
#' What is the position of the character that causes Santa to first enter the basement?


# start with puzzle_input
puzzle_input %>% 
  
  # read as data.table
  data.table::fread(header = FALSE) %>% 
  
  # extract as vector
  .[, V1] %>% 
  
  # separate into individual digits
  str_split(pattern = "") %>% 
  
  # convert to vector
  unlist() %>% 
  
  # set open parenthesis as +1 and close parenthesis as -1
  {ifelse(. == "(", 1, -1)} %>% 
  
  # get cumulative sum
  cumsum() %>% 
  
  # get all occurrences where the first basement was reached
  {. == -1} %>% 
  
  # convert to vector location
  which() %>% 
  
  # take first instance
  head(1)

# the answer is 1783
