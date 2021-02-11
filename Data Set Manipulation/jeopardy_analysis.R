library(tidyverse)
library(readxl)
library(Rfast)
# Load in raw data file
jeopardy_1 <- read_csv("Jeopardy Games.csv")

# Let's examine games with NA for correct/incorrect
jeopardy_2 <- jeopardy_1 %>% 
              filter(
                    !( #comment this line out when needed
                      # Check weird games
                     (is.na(P1_correct) |
                     is.na(P2_correct) |
                     is.na(P3_correct) |
                     is.na(P1_incorrect) |
                     is.na(P2_incorrect) |
                     is.na(P3_incorrect) |
                     is.na(P1_coryat) |
                     is.na(P2_coryat) |
                     is.na(P3_coryat) |
                     is.na(P1_dj_score) |
                     is.na(P2_dj_score) |
                     is.na(P3_dj_score) |
                     is.na(P1_final_score) |
                     is.na(P2_final_score) |
                     is.na(P3_final_score) ) |
                      
                      # We'll want to get rid of odd jeopardy tournaments
                     (grepl("All-Star",game_info,fixed = TRUE)) |
                     (grepl("Super Jeopardy!",game_info,fixed = TRUE))
                    ) #comment this line out when needed
                     
                     
                     # We'll probably want to get rid of all types of tournaments
                     #and stick to regular jeopardy games.
                     ) #ctrl enter here to run block

#Let's pull show number as a separate numeric variable.

jeopardy_3 <- jeopardy_2 %>% mutate(show_num = 
                                      as.numeric(substr(name,
                                             unlist(gregexpr(pattern='#',name))+1,
                                             unlist(gregexpr(pattern='-',name))-2))) %>% 
                             arrange(show_num)

#Now we'll want to convert all dollars into numbers.
#Create a simple conversion function:
dolnum <- function(x) 
{
x <- gsub("$", "", x, fixed=TRUE) #Get rid of dollar sign
x <- gsub(",", "", x, fixed=TRUE) #Get rid of commas
x <- gsub(" ", "", x, fixed = TRUE) #Get rid of blanks
 
# 
# #Worry about decimals for other figures
# if (grepl(".",x,fixed=TRUE)) {
#   y <- unlist(strsplit(x,".",fixed=TRUE))
# 
#   if (grepl("-",y[1],fixed=TRUE))
#     {
#     x <- as.numeric(y[1]) - (as.numeric(y[2])/(10^(nchar(y[2]))))
#     } else {
#     x <- as.numeric(y[1]) + (as.numeric(y[2])/10^(nchar(y[2])))
#     }
#   }
x <- as.numeric(x) #Make a number
return(x)
}

#Convert dollars to numeric
jeopardy_4 <- jeopardy_3 %>% mutate(P1_coryat = dolnum(P1_coryat),
                                    P2_coryat = dolnum(P2_coryat),
                                    P3_coryat = dolnum(P3_coryat),
                                    P1_dj_score = dolnum(P1_dj_score),
                                    P2_dj_score = dolnum(P2_dj_score),
                                    P3_dj_score = dolnum(P3_dj_score),
                                    P1_final_score = dolnum(P1_final_score),
                                    P2_final_score = dolnum(P2_final_score),
                                    P3_final_score = dolnum(P3_final_score)
                                    )

#Let's now add people's wager
#Did the player get the question right?
jeopardy_5 <- jeopardy_4 %>% mutate(P1_gotit = 
                                      ifelse(P1_final_score - P1_dj_score > 0,
                                             1,
                                             0),
                                    P2_gotit =
                                      ifelse(P2_final_score - P2_dj_score > 0,
                                             1,
                                             0),
                                    P3_gotit =
                                      ifelse(P3_final_score - P3_dj_score > 0,
                                             1,
                                             0)
                                    )
#Add wagers
jeopardy_6 <- jeopardy_5 %>% mutate(P1_wager = 
                                      ifelse(P1_gotit == 1,
                                             P1_final_score-P1_dj_score,
                                             P1_dj_score-P1_final_score),
                                    P2_wager = 
                                      ifelse(P2_gotit == 1,
                                             P2_final_score-P2_dj_score,
                                             P2_dj_score-P2_final_score),
                                    P3_wager = 
                                      ifelse(P3_gotit == 1,
                                             P3_final_score-P3_dj_score,
                                             P3_dj_score-P3_final_score)
                                    )

# Repeat the same but now order by first, second, and third.
# Note: First, second, and third are reflective of places BEFORE final jeop.

#Placing the players BEFORE final jeopardy
jeopardy_7 <- jeopardy_6 %>% mutate(
                                P1_place_dj =
                                ifelse(P1_dj_score >= P2_dj_score &
                                   P1_dj_score >= P3_dj_score, 1,
                                ifelse(P1_dj_score < P2_dj_score &
                                   P1_dj_score < P3_dj_score,3,2)),
                                
                                P2_place_dj =
                                  ifelse(P2_dj_score >= P3_dj_score &
                                           P2_dj_score >= P1_dj_score, 1,
                                         ifelse(P2_dj_score < P3_dj_score &
                                                  P2_dj_score < P1_dj_score,3,2)),
                                
                                P3_place_dj =
                                  ifelse(P3_dj_score >= P1_dj_score &
                                           P3_dj_score >= P2_dj_score, 1,
                                         ifelse(P3_dj_score < P2_dj_score &
                                                  P3_dj_score < P1_dj_score,3,2))
                                )

#Placing the players AFTER final jeopardy

jeopardy_8 <- jeopardy_7 %>% mutate(
  P1_place_final =
    ifelse(P1_final_score >= P2_final_score &
             P1_final_score >= P3_final_score, 1,
           ifelse(P1_final_score < P2_final_score &
                    P1_final_score < P3_final_score,3,2)),
  
  P2_place_final =
    ifelse(P2_final_score >= P3_final_score &
             P2_final_score >= P1_final_score, 1,
           ifelse(P2_final_score < P3_final_score &
                    P2_final_score < P1_final_score,3,2)),
  
  P3_place_final =
    ifelse(P3_final_score >= P1_final_score &
             P3_final_score >= P2_final_score, 1,
           ifelse(P3_final_score < P2_final_score &
                    P3_final_score < P1_final_score,3,2))
)

#Now with player's placed, let's do a probability analysis.
#Im going to impose a probability p of getting final j correct:
#Define that p as: number correct/(number incorrect +number correct)
#Note that number incorrect for a given player includes triple stumpers in game.

jeopardy_9 <- jeopardy_8 %>% mutate(P1_p = P1_correct/(P1_correct+P1_incorrect),
                                    P2_p = P2_correct/(P2_correct+P2_incorrect),
                                    P3_p = P3_correct/(P3_correct+P3_incorrect))
#Now we keep only the variables we want for probability analysis

jeopardy_10 <- jeopardy_9 %>% select(P1_p,
                                     P2_p,
                                     P3_p,
                                     P1_gotit,
                                     P2_gotit,
                                     P3_gotit)
#6244 rows

jeopardy_ps <- jeopardy_9 %>% 
  select(P1_p,
         P2_p,
         P3_p) %>% unlist() %>% tibble::enframe(name=NULL) %>% rename(p = value)

jeopardy_suc <- jeopardy_9 %>% 
  select(P1_gotit,
         P2_gotit,
         P3_gotit) %>% unlist() %>% tibble::enframe(name=NULL) %>% rename(gotit = value)

p_set <- cbind(jeopardy_ps,jeopardy_suc)                                

dot_prob_plot <- plot(p_set$p,p_set$gotit)

lin_reg_prob <- lm(p_set$gotit ~ p_set$p, data = p_set)

p_set_2 <- p_set %>% group_by(p) %>% summarise(avg_suc = (sum(gotit)/n()),
                                               n = n())
plot(p_set_2$p,p_set_2$avg_suc)

linreg <- lm(p_set_2$avg_suc ~ p_set_2$p, data = p_set_2)
summary(linreg)
summary(lin_reg_prob)

#Is my estimation of probability slightly higher than the true probability?
# Let us now filter out non-lock games. Keep only lock games.

lock_1 <- jeopardy_9 %>% mutate(first = max(P1_dj_score,P2_dj_score,P3_dj_score))                           
