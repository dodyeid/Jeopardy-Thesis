# install.packages("rvest", dependencies = TRUE)

# install.packages("gsubfn")

# install.packages("stringr")

# library(rvest)

# library(gsubfn)

# library(stringr)

rm(list=ls())



tbl_colnames <- c("name","game_j_number", "game_info",
                  
                  "P1_full_desc",
                  
                  "P2_full_desc",
                  
                  "P3_full_desc",
                  
                  "P1_name",
                  
                  "P2_name",
                  
                  "P3_name",
                  
                  "P1_coryat",
                  
                  "P2_coryat",
                  
                  "P3_coryat",
                  
                  "P1_dj_score",
                  
                  "P2_dj_score",
                  
                  "P3_dj_score",
                  
                  "P1_final_score",
                  
                  "P2_final_score",
                  
                  "P3_final_score",
                  
                  "P1_correct",
                  
                  "P2_correct",
                  
                  "P3_correct",
                  
                  "P1_incorrect",
                  
                  "P2_incorrect",
                  
                  "P3_incorrect")



dataset <- as_tibble(data.frame(matrix(nrow=0,ncol=length(tbl_colnames))))

colnames(dataset) <- tbl_colnames





# Need to make a test data set. For tests.

# dataset_test <- as_tibble(data.frame(matrix(nrow=0,ncol=length(tbl_colnames))))

# colnames(dataset_test) <- tbl_colnames





for (i in 1:6389) {
  
  
  
  tryCatch({
    
    # i <- 197
    
    str <- as.character(i)
    
    
    
    html <- paste0("http://www.j-archive.com/showgame.php?game_id=",str)
    
    
    
    game <- read_html(html)
    
    
    
    info <- game %>%
      
      html_nodes("div#game_comments") %>%
      
      html_text()
    
    
    
    x <- game %>%
      
      html_nodes("div h1") %>%
      
      html_text()
    
    
    
    contestants <- game %>%
      
      html_nodes("td p") %>%
      
      html_text()
    
    
    
    final_score_names <- game %>%
      
      html_nodes("div#double_jeopardy_round table td.score_player_nickname") %>%
      
      html_text()
    
    
    
    final_score_vector <- game %>%
      
      html_nodes("div#double_jeopardy_round table tr") %>%
      
      html_text()
    
    
    
    unclean_final_scores <- gsub(" ", "", unlist(strsplit(ifelse(grepl("$",final_score_vector[length(final_score_vector) - 1],fixed=TRUE),
                                                                 
                                                                 final_score_vector[length(final_score_vector) - 1],
                                                                 
                                                                 final_score_vector[length(final_score_vector)]), "\n")))
    
    final_scores <- unclean_final_scores[-4]
    
    
    
    
    
    end_scores_vector <- game %>%
      
      html_nodes("div#final_jeopardy_round table tr") %>%
      
      html_text()
    
    
    
    unclean_end_scores <- gsub(" ", "", unlist(strsplit(end_scores_vector[length(end_scores_vector) - 4], "\n")))
    
    end_scores <- unclean_end_scores[-4]
    
    
    
    
    
    end_coryat_vector <- game %>%
      
      html_nodes("div#final_jeopardy_round table tr") %>%
      
      html_text()
    
    
    
    unclean_end_coryat <- gsub(" ", "", unlist(strsplit(end_coryat_vector[length(end_coryat_vector) - 1], "\n")))
    
    end_coryat <- unclean_end_coryat[-4]
    
    
    
    
    
    
    
    ultimate_answer <- as.character(game)
    
    # answers <- html_attr(html_nodes(game, "div"), "onmouseover")
    
    #
    
    # ultimate_answer <- paste(answers,collapse = "")
    
    
    
    # right <- regmatches(ultimate_answer, gregexpr("right\\\">([a-zA-Z'| ]+)<", ultimate_answer))
    
    # wrong <- regmatches(ultimate_answer, gregexpr("wrong\\\">([a-zA-Z'| ]+)<", ultimate_answer))
    
    # right
    
    # wrong
    
    # library(stringr)
    
    # library(gsubfn)
    
    # right <- str_match(ultimate_answer, "right\\\">([a-zA-Z'| ]+)<")
    
    right <- strapply(ultimate_answer, "right\\\">([a-zA-Z'| |.|-]+)<|right&quot;&gt;([a-zA-Z'| |.|-]+)&", FUN=c, simplify=rbind, backref=NULL)
    
    wrong <- strapply(ultimate_answer, "wrong\\\">([a-zA-Z'| |.|-]+)<|wrong&quot;&gt;([a-zA-Z'| |.|-]+)&", FUN=c, simplify=rbind, backref=NULL)
    
    
    
    right <- as.vector(right)
    
    wrong <- as.vector(wrong)
    
    lst <- as.list(wrong)
    
    
    
    wrong <- unlist(lapply(lst, function(x) if(x == "Triple Stumper") c(final_score_names) else x))
    
    
    
    # library(tidyverse)
    
    
    
    right <- data.frame(right)
    
    right_tbl <- bind_cols(right)
    
    wrong <- data.frame(wrong)
    
    wrong_tbl <- bind_cols(wrong)
    
    
    
    right_tbl <- right_tbl %>% count(right, name = "correct") %>% rename("Name" = right)
    
    right_tbl
    
    
    
    wrong_tbl <- wrong_tbl %>% count(wrong, name = "incorrect") %>% rename("Name" = wrong)
    
    wrong_tbl
    
    
    
    correct_incorrect <- left_join(right_tbl, wrong_tbl, by = "Name")
    
    correct_incorrect
    
    
    
    pct_correct <- correct_incorrect %>% mutate(pct = (correct/(correct + incorrect)))
    
    
    
    # Build one row of this game
    
    
    
    P1_name <- final_score_names[1]
    
    P2_name <- final_score_names[2]
    
    P3_name <- final_score_names[3]
    
    
    
    P1_coryat <- end_coryat[1]
    
    P2_coryat <- end_coryat[2]
    
    P3_coryat <- end_coryat[3]
    
    
    
    P1_dj_score <- final_scores[1]
    
    P2_dj_score <- final_scores[2]
    
    P3_dj_score <- final_scores[3]
    
    
    
    P1_final_score <- end_scores[1]
    
    P2_final_score <- end_scores[2]
    
    P3_final_score <- end_scores[3]
    
    
    
    correct_incorrect_P1 <- correct_incorrect %>% filter(Name == P1_name) %>% slice(1) %>% unlist(., use.names=FALSE)
    
    P1_correct <- as.numeric(correct_incorrect_P1[2])
    
    
    
    correct_incorrect_P2 <- correct_incorrect %>% filter(Name == P2_name) %>% slice(1) %>% unlist(., use.names=FALSE)
    
    P2_correct <- as.numeric(correct_incorrect_P2[2])
    
    
    
    correct_incorrect_P3 <- correct_incorrect %>% filter(Name == P3_name) %>% slice(1) %>% unlist(., use.names=FALSE)
    
    P3_correct <- as.numeric(correct_incorrect_P3[2])
    
    
    
    P1_incorrect <- as.numeric(correct_incorrect_P1[3])
    
    P2_incorrect <- as.numeric(correct_incorrect_P2[3])
    
    P3_incorrect <- as.numeric(correct_incorrect_P3[3])
    
    # Can still do occupations/descriptions
    
    
    
    # assign(paste0("game_",str),
    
    #        c(name,
    
    #          game_j_number,
    
    #          P1_name,
    
    #          P2_name,
    
    #          P3_name,
    
    #         
    
    #          P1_coryat,
    
    #          P2_coryat,
    
    #          P3_coryat,
    
    #         
    
    #          P1_dj_score,
    
    #          P2_dj_score,
    
    #          P3_dj_score,
    
    #         
    
    #          P1_final_score,
    
    #          P2_final_score,
    
    #          P3_final_score,
    
    #         
    
    #          P1_correct,
    
    #          P2_correct,
    
    #          P3_correct,
    
    #         
    
    #          P1_incorrect,
    
    #          P2_incorrect,
    
    #          P3_incorrect))
    
    
    
    # dataset <- rbind(dataset, get(paste0("game_",str)))
    
    dataset <- add_row(dataset,
                       
                       name = x,
                       
                       game_j_number = i,
                       
                       game_info = info,
                       
                       
                       
                       P1_full_desc = contestants[1],
                       
                       P2_full_desc = contestants[2],
                       
                       P3_full_desc = contestants[3],
                       
                       
                       
                       P1_name = final_score_names[1],
                       
                       P2_name = final_score_names[2],
                       
                       P3_name = final_score_names[3],
                       
                       
                       
                       P1_coryat = end_coryat[1],
                       
                       P2_coryat = end_coryat[2],
                       
                       P3_coryat = end_coryat[3],
                       
                       
                       
                       P1_dj_score = final_scores[1],
                       
                       P2_dj_score = final_scores[2],
                       
                       P3_dj_score = final_scores[3],
                       
                       
                       
                       P1_final_score = end_scores[1],
                       
                       P2_final_score = end_scores[2],
                       
                       P3_final_score = end_scores[3],
                       
                       
                       
                       P1_correct = as.numeric(correct_incorrect_P1[2]),
                       
                       P2_correct = as.numeric(correct_incorrect_P2[2]),
                       
                       P3_correct = as.numeric(correct_incorrect_P3[2]),
                       
                       
                       
                       P1_incorrect = as.numeric(correct_incorrect_P1[3]),
                       
                       P2_incorrect = as.numeric(correct_incorrect_P2[3]),
                       
                       P3_incorrect = as.numeric(correct_incorrect_P3[3]))
    
    
    
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

