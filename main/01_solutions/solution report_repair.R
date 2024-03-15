
# INTRO -------------------------------------------------------------------

# coding exercise: report_repair
# source:          https://adventofcode.com/2020/day/1



## PROBLEM -------------------------------------------------

# Finance needs you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.
#
# Specifically, you need to find the two entries that sum to 2020 and then multiply those two numbers together.
# 
# For example, suppose your expense report contained the following:
# 
# 1721
# 979
# 366
# 299
# 675
# 1456
# 
# In this list, the two entries that sum to 2020 are 1721 and 299. 
# Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.
# 
# Of course, your expense report is much larger. Find the two entries that sum to 2020; 
# what do you get if you multiply them together?

puzzle_input <- "main/00_data/report_repair.txt"



## YOUR SOLUTION -------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# start with puzzle input
puzzle_input %>% 
  
  # read data
  read_lines() %>%
  
  # convert to integer
  as.integer() %>% 
  
  # create a cross-join for every combination
  crossing(X1 = ., X2 = .) %>% 
  
  # remove duplicates
  filter(X1 >= X2) %>% 
  distinct() %>% 
  
  # filter to where sum is equal to 2020
  filter(X1 + X2 == 2020L) %>% 
  
  # multiply together
  mutate(result = X1 * X2) %>% 
  
  # extract value
  pull(result)
  
# The answer is 1006875. 



# PROBLEM -----------------------------------------------------------------

# Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. 
# Multiplying them together produces the answer, 241861950.
# 
# In your expense report, what is the product of the three entries that sum to 2020?



# YOUR SOLUTION -----------------------------------------------------------

# start with puzzle input
puzzle_input %>% 
  
  # read data
  read_lines() %>%
  
  # convert to integer
  as.integer() %>% 
  
  # create a cross-join for every combination
  crossing(X1 = ., X2 = ., X3 = .) %>% 
  
  # remove duplicates
  filter(X1 >= X2, X2 >= X3) %>% 
  distinct() %>% 
  
  # filter to where sum is equal to 2020
  filter(X1 + X2 + X3 == 2020L) %>% 
  
  # multiply together
  mutate(result = X1 * X2 * X3) %>% 
  
  # extract value
  pull(result)

# The answer is 165026160.
