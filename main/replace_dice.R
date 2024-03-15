
# INTRO -------------------------------------------------------------------

# coding exercise: replace dice
# source:          https://fivethirtyeight.com/features/can-you-get-the-gloves-out-of-the-box/



## PROBLEM -------------------------------------------------

# You start with a fair 6-sided die and roll it six times, recording the results of each roll. 
# You then write these numbers on the six faces of another, unlabeled fair die. 
# For example, if your six rolls were 3, 5, 3, 6, 1 and 2, then your second die wouldn’t have a 4 on it; 
# instead, it would have two 3s.
# 
# Next, you roll this second die six times. You take those six numbers and write them on the faces of yet another 
# fair die, and you continue this process of generating a new die from the previous one.
# 
# Eventually, you’ll have a die with the same number on all six faces. 
# If one ROUND is equal to six dice rolls, what is the average number of ROUNDS it will take to reach this end state?
# 
# Simulate up to 10,000 times.
# Round your answer to 1 decimal place.



## YOUR SOLUTION -------------------------------------------

# set seed for reproducibility
set.seed(12345)

# fix number of simulations
n_sim <- 10000






