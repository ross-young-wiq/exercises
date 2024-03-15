
# INTRO -------------------------------------------------------------------

# coding exercise: low roll
# source:          https://fivethirtyeight.com/features/how-low-can-you-roll/



## PROBLEM -------------------------------------------------

# You are given a fair, unweighted 10-sided die with sides labeled 0 to 9 and a sheet of paper to record your score. 
# 
# To start the game, you roll the die. Your current “score” is the number shown, divided by 10. 
# For example, if you were to roll a 7, then your score would be 0.7. Then, you keep rolling the die over and over 
# again. Each time you roll, if the digit shown by the die is less than or equal to the last digit of your score, 
# then that roll becomes the new last digit of your score. Otherwise you just go ahead and roll again. 
# The game ends when you roll a zero.
# 
# For example, suppose you roll the following: 6, 2, 5, 1, 8, 1, 0. 
# After your first roll, your score would be 0.6, After the second, it’s 0.62. You ignore the third roll, 
# since 5 is greater than the current last digit, 2. After the fourth roll, your score is 0.621. 
# You ignore the fifth roll, since 8 is greater than the current last digit, 1. 
# After the sixth roll, your score is 0.6211. 
# And after the seventh roll, the game is over — 0.6211 is your final score.
# 
# What will be your average final score in this game?
# 
# Simulate up to 10,000 times.
# Round your answer to 2 decimal places.



## YOUR SOLUTION -------------------------------------------

# set seed for reproducibility
set.seed(12345)

# fix number of simulations
n_sim <- 10000

# create data.table with dice rolls
data.table(rolls = sample(x = 0:9, size = 1e6, replace = TRUE)) %>% 
  
  # add column to separate rounds 
  # lag column so that 0-value 'ends' the round
  .[, round := 1L + shift(cumsum(rolls == 0), n = 1, fill = 0, type = "lag")] %>% 
  
  # filter to a fixed number of simulations
  .[round <= n_sim] %>%
  
  # take cumulative min within each round
  .[, min_roll := cummin(rolls), by = .(round)] %>% 
  
  # filter out rolls that exceed the cumulative minimum
  .[rolls == min_roll] %>% 
  
  # get rowid within each round
  .[, row_id := rowid(round)] %>% 
  
  # get score for each roll
  .[, score := rolls / (10^row_id)] %>%
  
  # sum to get score per round
  .[, .(round_score = sum(score)), by = .(round)] %>%

  # extract a vector of round scores
  .[, round_score] %>% 
  
  # take average
  mean() %>% 
  
  # round to 2 decimal places
  round(digits = 2)


# the answer is 0.47
