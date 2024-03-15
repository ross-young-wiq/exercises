
# INTRO -------------------------------------------------------------------

# coding exercise: binary_diagnostic
# source:          https://adventofcode.com/2021/day/3



## PROBLEM -------------------------------------------------

#' The diagnostic report (your puzzle input) consists of a list of binary numbers for your car.
#' The first parameter to check is the power consumption.
#' 
#' You need to use the binary numbers in the diagnostic report to generate two new binary numbers 
#' (called the gamma rate and the epsilon rate). The power consumption can then be found by multiplying the 
#' gamma rate by the epsilon rate.
#' 
#' Each bit in the gamma rate can be determined by finding the most common bit in the corresponding position 
#' of all numbers in the diagnostic report. For example, given the following diagnostic report:
#'   
#' 00100
#' 11110
#' 10110
#' 10111
#' 10101
#' 01111
#' 00111
#' 11100
#' 10000
#' 11001
#' 00010
#' 01010
#' 
#' 
#' Considering only the first bit of each number, there are five 0 bits and seven 1 bits. 
#' Since the most common bit is 1, the first bit of the gamma rate is 1.
#' 
#' The most common second bit of the numbers in the diagnostic report is 0, 
#' so the second bit of the gamma rate is 0.
#' 
#' The most common value of the third, fourth, and fifth bits are 1, 1, and 0, respectively, 
#' and so the final three bits of the gamma rate are 110.
#' 
#' So, the gamma rate is the binary number 10110, or 22 in decimal.
#' 
#' The epsilon rate is calculated in a similar way; rather than use the most common bit, 
#' the least common bit from each position is used. So, the epsilon rate is 01001, or 9 in decimal. 
#' Multiplying the gamma rate (22) by the epsilon rate (9) produces the power consumption, 198.
#' 
#' Use the binary numbers in your diagnostic report to calculate the gamma rate and epsilon rate, 
#' then multiply them together. 
#' 
#' What is the power consumption? 
#' (Be sure to represent your answer in decimal, not binary.)


puzzle_input <- "main/00_data/binary_diagnostic.txt"



## YOUR SOLUTION -------------------------------------------

# load packages
library(tidyverse)
library(data.table)


# create function to convert binary to numeric
bin_2_nbr <- function(x) {
  
  # get string length
  x_len <- str_length(x)
  
  # start with string
  x %>% 
    
    # split into individual digits
    str_split(pattern = "") %>% 
    
    # convert from list to vector
    unlist() %>% 
    
    # convert to logical vector
    {. == "1"} %>% 
    
    # multiply each digit by power of 2
    {. * 2^((x_len-1):0)} %>% 
    
    # take sum
    sum() %>% 
    
    # return value
    return()
  
}


# start with puzzle_input
puzzle_input %>% 
  
  # read as data.table
  data.table::fread(header = FALSE, colClasses = "character") %>% 
  
  # add row_id
  .[, V2 := .I] %>% 
  
  # split string into individual digits
  .[, V3 := str_split(string = V1, pattern = "")] %>% 
  
  # unnest new column
  .[, .(V3 = unlist(V3)), by = .(V1, V2)] %>% 
  
  # add row_id within each group
  .[, V4 := rowid(V2)] %>% 
  
  # count 0s and 1s per group row_id
  .[, .(N = .N), keyby = .(V4, V3)] %>% 
  
  # get max_N for each group
  .[, max_N := max(N), by = .(V4)] %>% 
  
  # logic test for if 0 is the mode or 1 is the mode
  .[, is_mode := N == max_N] %>% 
  
  # summarise into two binary values
  .[, .(V5 = paste0(V3, collapse = "")), by = .(is_mode)] %>% 
  
  # convert binary to numeric
  .[, V6 := map_dbl(.x = V5, .f = bin_2_nbr)] %>% 
  
  # extract result as vector
  .[, V6] %>% 
  
  # multiply
  prod()

# the answer is 3429254
