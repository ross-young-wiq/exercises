
# INTRO -------------------------------------------------------------------

# coding exercise: inverse_captcha
# source:          https://adventofcode.com/2017/day/1



## PROBLEM -------------------------------------------------

# You're standing in a room with "digitization quarantine" written in LEDs along one wall. 
# The only door is locked, but it includes a small interface. "Restricted Area - Strictly No Digitized Users Allowed."
# 
# It goes on to explain that you may only leave by solving a captcha to prove you're not a human. 
# Apparently, you only get one millisecond to solve the captcha: too fast for a normal human, 
# but it feels like hours to you.
# 
# The captcha requires you to review a sequence of digits (your puzzle input) and find the 
# sum of all digits that match the next digit in the list. 
# The list is circular, so the digit after the last digit is the first digit in the list.
# 
# For example:
#   
# 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
# 1111 produces 4 because each digit (all 1) matches the next.
# 1234 produces 0 because no digit matches the next.
# 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.
# 
# What is the solution to your captcha?

puzzle_input <- "main/00_data/inverse_captcha.txt"



## YOUR SOLUTION -------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# start with puzzle input
puzzle_input %>% 
  
  # read data
  fread(header = FALSE) %>% 
  
  # extract value as vector
  .[, V1] %>% 
  
  # split into separate digits
  str_split(pattern = "") %>% 
  
  # convert from list into numeric
  unlist() %>%
  as.numeric() %>% 
  
  # save vector as variable
  force() -> v_puzzle_input


# create function to test comparison logic
compare_vals <- function(x, n = 1) {
  
  # offset vector
  y <- c(tail(x, -n), head(x, n))
  
  # return sum of comparison
  return(sum(x[x == y]))
  
}

# run function where n = 1
compare_vals(x = v_puzzle_input)

# the answer is 1089



# PROBLEM -----------------------------------------------------------------

# Now, instead of considering the next digit, it wants you to consider the digit halfway around the circular list. 
# That is, if your list contains 10 items, only include a digit in your sum if the digit 10/2 = 5 steps forward 
# matches it. Fortunately, your list has an even number of elements.
# 
# For example:
#   
# 1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
# 1221 produces 0, because every comparison is between a 1 and a 2.
# 123425 produces 4, because both 2s match each other, but no other digit has a match.
# 123123 produces 12.
# 12131415 produces 4.
# 
# What is the solution to your new captcha?



# YOUR SOLUTION -----------------------------------------------------------

# run function where n = length of the vector then halved
compare_vals(x = v_puzzle_input, n = length(v_puzzle_input) / 2)

# the answer is 1156
