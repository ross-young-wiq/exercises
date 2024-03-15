
# INTRO -------------------------------------------------------------------

# coding exercise: rocket_fuel
# source:          https://adventofcode.com/2019/day/1



## PROBLEM -------------------------------------------------

# You are packing fuel for a trip into space.
#
# Fuel required to launch a given module is based on its mass. 
# Specifically, to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2.
# 
# For example:
#   
# For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
# For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
# For a mass of 1969, the fuel required is 654.
# For a mass of 100756, the fuel required is 33583.
# 
# To find the fuel requirement for your trip, individually calculate the fuel needed for the mass of each module 
# (your puzzle input), then add together all the fuel values.
# 
# 1) What is the sum of the fuel requirements for all of the modules on your spacecraft?

puzzle_input <- "main/00_data/rocket_fuel.txt"



## YOUR SOLUTION -------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# function for fuel requirements
fuel <- function(x, ...) {floor(x / 3) - 2}

# clean data
input <- read_lines(puzzle_input) %>% as.numeric()

# start with puzzle_input
input %>% 
  
  # run loop and calculate fuel
  map_dbl(.f = fuel) %>% 
  
  # take the sum
  sum()
  
# the answer is 3188480  



# PROBLEM -----------------------------------------------------------------

# You realise that fuel requires fuel - take its mass, divide by three, round down, and then subtract 2
# But that fuel ALSO requires fuel, and so on.
# However, if your fuel ever requires a negative value, then assume zero further fuel is required from there.
#
# For example:
# A module of mass 14 requires 2 fuel. This fuel requires no further fuel (2 divided by 3 and rounded down is 0, 
# which would call for a negative fuel), so the total fuel required is still just 2.
# 
# At first, a module of mass 1969 requires 654 fuel. 
# Then, this fuel requires 216 more fuel (654 / 3 - 2). 216 then requires 70 more fuel, 
# which requires 21 fuel, which requires 5 fuel, which requires no further fuel. 
# So, the total fuel required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966.
#
# The fuel required by a module of mass 100756 and its fuel is: 
# 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346.
#
# 2) What is the sum of the fuel requirements for all of the modules on your spacecraft when also taking into 
# account the mass of the added fuel? 
#
# (Calculate the fuel requirements for each module separately, then add them all up at the end.)



# SOLUTION ----------------------------------------------------------------

# start with puzzle_input
input %>% 
  
  # run loop for each individual value
  map_dbl(
    .f = function(x) {
      
      # run loop, passing the result into itself until done condition
      purrr::accumulate(
        .x = 1:1e2,
        .f = function(y, ...) {
          if (fuel(y) <= 0) {return(done())} else {fuel(y)}
        },
        .init = x
      ) %>% 
        
        # take the sum of fuel values, and remove the initial input value
        sum(.) - x
      
    }
  ) %>% 
  
  # take the sum
  sum()

# the answer is 4779847  
