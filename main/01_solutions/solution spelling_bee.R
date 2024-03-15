
# INTRO -------------------------------------------------------------------

# coding exercise: spelling bee
# source:          https://fivethirtyeight.com/features/can-you-solve-the-vexing-vexillology/



## PROBLEM -------------------------------------------------

# You have a honeycomb of letters: R (center), surrounded by A, E, G, I, N, T
# 
# You want to identify as many English-language words that meet the following criteria:
# 
# - The word must be at least four letters long.
# - The word must include the central letter of your honeycomb.
# - The word cannot include any letter beyond the seven given letters of the honeycomb.
# 
# Note that letters can be repeated. For example, the words AGAR and TRITE are both acceptable words. 
# Four-letter words are worth 1 point each, while five-letter words are worth 5 points, six-letter words are 
# worth 6 points, seven-letter words are worth 7 points, etc. 
# 
# Words that use all of the seven letters in the honeycomb are known as “pangrams” and earn 7 bonus points 
# (in addition to the points for the length of the word). So in the above example, GRANITE is worth 14 points.
# 
# If you got all possible words for this honeycomb, What would be your overall game score?
# 
# For consistency, please use this list of valid words as an input:
# https://norvig.com/ngrams/enable1.txt



## YOUR SOLUTION -------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# parameters
dt_words  <- data.table(word = read_lines("https://norvig.com/ngrams/enable1.txt"))
v_letters <- c("R", "A", "E", "G", "I", "N", "T")
v_centre  <- "R"

# start with data.table of all words
dt_words %>%
  
  # add column for word length
  .[, word_length := str_length(word)] %>% 
  
  # REQUIREMENT: word must be at least 4 letters long
  .[word_length >= 4] %>% 
  
  # convert words to upper case (given parameters are capitalised)
  .[, word := str_to_upper(word)] %>% 
  
  # REQUIREMENT: word must include the central letter
  .[str_detect(string = word, pattern = v_centre)] %>% 
  
  # split word into individual letters
  .[, word_split := str_split(string = word, pattern = "")] %>% 
  
  # count number of letters from the input set
  .[, n_input_letters_in_word := map_dbl(.x = word_split, .f = ~ sum(.x %chin% v_letters))] %>% 
  
  # REQUIREMENT: word must include only input letters
  .[word_length == n_input_letters_in_word] %>% 
  
  # check number of unique honeycomb letters used
  .[, n_input_letters_used := map_dbl(.x = word, .f = ~ sum(str_detect(string = .x, pattern = v_letters)))] %>% 
  
  # check if word is a pangram
  .[, is_pangram := n_input_letters_used == 7] %>% 
  
  # calculate score
  .[, score := fifelse(word_length == 4, 1, word_length) + (is_pangram * 7)] %>%
  
  # sum score
  .[, sum(score)]


# the answer is 3,898
