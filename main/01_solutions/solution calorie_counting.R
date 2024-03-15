
# INTRO -------------------------------------------------------------------

# coding exercise: calorie_counting
# source:          https://adventofcode.com/2022/day/1



## PROBLEM -------------------------------------------------

# You are packing for a trip and you need the most important supply - FOOD
# Everyone in your group takes turns writing down the number of Calories contained by the various meals, 
# snacks, rations, etc. that they've brought with them, one item per line. 

# Each person separates their own inventory from the previous person's inventory (if any) by a blank line.
# 
# For example, suppose the finished list looks like:
#
#
#
# 1000
# 2000
# 3000
# 
# 4000
# 
# 5000
# 6000
# 
# 7000
# 8000
# 9000
# 
# 10000
#
#
#
# This list represents the Calories of the food carried by five people:
# 
# The first is carrying food with 1000, 2000, and 3000 Calories, a total of 6000 Calories.
# The second is carrying one food item with 4000 Calories.
# The third is carrying food with 5000 and 6000 Calories, a total of 11000 Calories.
# The fourth is carrying food with 7000, 8000, and 9000 Calories, a total of 24000 Calories.
# The fifth is carrying one food item with 10000 Calories.
# 
# In case your group gets hungry and need extra snacks, they need to know which person to ask: 
# they'd like to know how many Calories are being carried by the person carrying the most Calories. 
# 
# In the example above, this is 24000 (carried by the fourth person).
# 
# Find the person carrying the most Calories using the PUZZLE INPUT
# 
# 1) HOW MANY TOTAL CALORIES ARE THEY CARRYING?
# 
# 2) HOW MANY TOTAL CALORIES ARE BEING CARRIED BY THE TOP THREE TEAM MEMBERS?

puzzle_input <- "main/00_data/calorie_counting.txt"


## YOUR SOLUTION -------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# start with puzzle_input
puzzle_input %>% 
  
  # read as data.table
  data.table::fread(header = FALSE) %>% 
  
  # create a group column, separated by blank (NA) lines
  .[, V2 := 1L + cumsum(is.na(V1))] %>% 
  
  # remove blank lines
  .[!is.na(V1)] %>%
  
  # add up calories by person
  .[, .(V3 = sum(V1, na.rm = TRUE)), by = .(V2)] %>%
  
  # sort by calorie weight
  .[order(-V3)] %>% 
  
  # take the first person and their calorie weight
  .[1, V3]

# the answer is 65,912



puzzle_input %>% 
  
  # read as data.table
  data.table::fread(header = FALSE) %>% 
  
  # create a group column, separated by blank (NA) lines
  .[, V2 := 1L + cumsum(is.na(V1))] %>% 
  
  # remove blank lines
  .[!is.na(V1)] %>%
  
  # add up calories by person
  .[, .(V3 = sum(V1, na.rm = TRUE)), by = .(V2)] %>%
  
  # sort by calorie weight
  .[order(-V3)] %>% 
  
  # take the first three people and their combined calorie weight
  .[1:3, sum(V3)]
  
# the answer is 195,625 
