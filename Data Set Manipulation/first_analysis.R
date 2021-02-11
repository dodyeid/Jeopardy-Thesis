## 10/5/19
## Purpose is to run initial analyses on Jeopardy Data for Senior Thesis
## WARNING: One line of code is non-dynamic and needs to be updated if original data set is updated.

setwd("C:\\Users\\dodye\\Desktop\\Senior Thesis R")
library(tidyverse)
library(ggplot2)

master_raw <- read_rds("master_raw")

## Let's look at all lock games!
lock_games <- master_raw %>% filter(lock == 1)

## Recall that lock-tie games are games when p1 has double p2. Before Nov 2014, lock-ties were locks.
lock_tie_games <- master_raw %>% filter(row_number() < 4515) %>% filter(lock_tie == 1)
##31 games were lock-ties prior to the rule change.
## What was the leading wager in that situation?
lock_tie_games_sum <-  lock_tie_games %>% mutate(tie_bet = ifelse(djfirst_place_wager==0,1,0))

## Let's look at all lock games. I include those that are in a lock-tie pre nov 2014
lock_games <- master_raw %>% filter(lock == 1 | (row_number() < 4515 & lock_tie == 1))

## Do all people in lock games make a "smart" wager?
lock_games_rational <- lock_games %>% mutate(rational = ifelse(djfirst_place_wager <= djfirst_place_dj_score - 2*djsecond_place_dj_score,1,0))
lock_games_rational_count <- lock_games_rational %>% 
                                      group_by(rational) %>% 
                                      summarise(rational_sum = n())
##51 people are being dumb! I'll filter them out.
lock_games_2 <- lock_games_rational %>% filter(rational == 1)

##################### SUMMARY STATISTICS #######################

dj_score_distribution <- lock_games_2 %>% ggplot(aes(x = djfirst_place_dj_score)) +
                                          geom_histogram()
dj_score_distribution ## Need to make this pretty

## Now, interested in how much money first place has to wager
dj_differential_distribution <- lock_games_2 %>% ggplot(aes(x = (djfirst_place_dj_score-djsecond_place_dj_score))) +
  geom_histogram()
dj_differential_distribution ## Need to make this pretty

### Summary Statistic Table ###


## Let's now build a probability model. 
## Since the title "Player 1" is randomly distributed, we can run a regression with p1_got it as dv.
log_reg <- glm(P1_gotit ~ P1_coryat + P1_dj_score + P1_correct + P1_incorrect + P1_place_dj
                                + P2_coryat + P2_dj_score + P2_correct + P2_incorrect + P2_place_dj
                                + P3_coryat + P3_dj_score + P3_correct + P3_incorrect + P3_place_dj,
                                data = lock_games_2,
                                family = binomial)
summary(log_reg)
