library(tidyverse)
library(readxl)

setwd("C:\\Users\\dodye\\Desktop\\Senior Thesis R\\senior_thesis")

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
                     is.na(P3_final_score) ) 
                      
                      # We'll want to get rid of odd jeopardy tournaments
                     # (grepl("All-Star",game_info,fixed = TRUE)) |
                     # (grepl("Super Jeopardy!",game_info,fixed = TRUE)) 
                    ) #comment this line out when needed
                     
                     
                     # We'll probably want to get rid of all types of tournaments
                     #and stick to regular jeopardy games.
                     )
saveRDS(jeopardy_2,"raw_nona")
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

# Get rid of tournament games.
jeopardy_9 <- jeopardy_8 %>% 
  filter((is.na(game_info)) | 
           (!(str_detect(game_info, "ournament")) & 
           !(str_detect(game_info, "Kids")) &
           !(str_detect(game_info, "kids")) &
           !(str_detect(game_info, "College")) &
           !(str_detect(game_info, "college")) &
            !(str_detect(game_info, "elebrity")) &
             !(str_detect(game_info, "All-Star")) &
             !(str_detect(game_info, "Super Jeopardy"))))
saveRDS(jeopardy_9,"raw_nona_notour")

#Add a variable for the winner_ID and winner_name
jeopardy_10 <- jeopardy_9 %>% mutate(winner_ID = ifelse(P1_place_final == 1, "P1_name",
                                                        ifelse(P2_place_final == 1, "P2_name",
                                                               "P3_name")))
jeopardy_11 <- jeopardy_10 %>% mutate(winner_name = ifelse(winner_ID == "P1_name", P1_name,
                                                           ifelse(winner_ID == "P2_name", P2_name,
                                                                  P3_name)))

#Add continuation variable
jeopardy_12 <- jeopardy_11 %>% mutate(last_game_winner = lag(winner_name))
jeopardy_13 <- jeopardy_12 %>% mutate(continuation = ifelse(
                                          P1_name == last_game_winner |
                                          P2_name == last_game_winner |
                                          P3_name == last_game_winner, 1, 0))

# Add a streak variable which equals 1 for the first game someone wins, 2 for their second game, etc.
jeopardy_14 <- jeopardy_13 %>% 
                        mutate(streak = sequence(rle(as.character(winner_name))$lengths))

# Add variables for dj score by place. djfirst indicates "the person who was in first place going into final j."
jeopardy_15 <- jeopardy_14 %>% 
              mutate(first_place_dj_score = ifelse(P1_place_final == 1, P1_dj_score,
                                                   ifelse(P2_place_final==1, P2_dj_score, P3_dj_score))) %>% 
              mutate(second_place_dj_score = ifelse(P1_place_final == 2, P1_dj_score,
                                       ifelse(P2_place_final==2, P2_dj_score, P3_dj_score))) %>% 
              mutate(third_place_dj_score = ifelse(P1_place_final == 3, P1_dj_score,
                                        ifelse(P2_place_final==3, P2_dj_score, P3_dj_score))) %>%
  
              mutate(djfirst_place_dj_score = ifelse(P1_place_dj == 1, P1_dj_score,
                                       ifelse(P2_place_dj==1, P2_dj_score, P3_dj_score))) %>% 
              mutate(djsecond_place_dj_score = ifelse(P1_place_dj == 2, P1_dj_score,
                                        ifelse(P2_place_dj==2, P2_dj_score, P3_dj_score))) %>% 
              mutate(djthird_place_dj_score = ifelse(P1_place_dj == 3, P1_dj_score,
                                       ifelse(P2_place_dj==3, P2_dj_score, P3_dj_score)))
              


#Add variable called lock for when first place has more than double second place
jeopardy_16 <- jeopardy_15 %>% mutate(lock = ifelse(djfirst_place_dj_score > 2*djsecond_place_dj_score,1,0)) 

#Add variable called lock_tie for when first place has exactly double second place
jeopardy_17 <- jeopardy_16 %>% mutate(lock_tie = ifelse(djfirst_place_dj_score == 2*djsecond_place_dj_score,1,0))

jeopardy_18 <- jeopardy_17 %>% 
  mutate(first_place_wager = ifelse(P1_place_final == 1, P1_wager,
                                    ifelse(P2_place_final==1, P2_wager, P3_wager))) %>% 
  mutate(second_place_wager = ifelse(P1_place_final == 2, P1_wager,
                                     ifelse(P2_place_final==2, P2_wager, P3_wager))) %>% 
  mutate(third_place_wager = ifelse(P1_place_final == 3, P1_wager,
                                    ifelse(P2_place_final==3, P2_wager, P3_wager))) %>%
  mutate(djfirst_place_wager = ifelse(P1_place_dj == 1, P1_wager,
                                      ifelse(P2_place_dj==1, P2_wager, P3_wager))) %>% 
  mutate(djsecond_place_wager = ifelse(P1_place_dj == 2, P1_wager,
                                       ifelse(P2_place_dj==2, P2_wager, P3_wager))) %>% 
  mutate(djthird_place_wager = ifelse(P1_place_dj == 3, P1_wager,
                                      ifelse(P2_place_dj==3, P2_wager, P3_wager))) %>% 
  
  mutate(first_place_final_score = ifelse(P1_place_final == 1, P1_final_score,
                                    ifelse(P2_place_final==1, P2_final_score, P3_final_score))) %>% 
  mutate(second_place_final_score = ifelse(P1_place_final == 2, P1_final_score,
                                     ifelse(P2_place_final==2, P2_final_score, P3_final_score))) %>% 
  mutate(third_place_final_score = ifelse(P1_place_final == 3, P1_final_score,
                                    ifelse(P2_place_final==3, P2_final_score, P3_final_score))) %>%
  mutate(djfirst_place_final_score = ifelse(P1_place_dj == 1, P1_final_score,
                                      ifelse(P2_place_dj==1, P2_final_score, P3_final_score))) %>% 
  mutate(djsecond_place_final_score = ifelse(P1_place_dj == 2, P1_final_score,
                                       ifelse(P2_place_dj==2, P2_final_score, P3_final_score))) %>% 
  mutate(djthird_place_final_score = ifelse(P1_place_dj == 3, P1_final_score,
                                      ifelse(P2_place_dj==3, P2_final_score, P3_final_score))) %>% 
  
  mutate(djfirst_gotit = 
           ifelse(djfirst_place_final_score - djfirst_place_dj_score > 0,
                  1,
                  0),
         djsecond_gotit =
           ifelse(djsecond_place_final_score - djsecond_place_dj_score > 0,
                  1,
                  0),
         djthird_gotit =
           ifelse(djthird_place_final_score - djthird_place_dj_score > 0,
                  1,
                  0)
  )


  
  

#And now I will call this jeopardy data set the master raw data set
master_raw <- jeopardy_18

saveRDS(master_raw, file = "master_raw")
  









