#### Top level ####
setwd("C:/Users/Michael/Google Drive/R stuff/3 - Fall 2020/POLISCI919/Project")

library(haven)
library(randomForest)
library(caret)
library(ROCR) 
library(pROC)
library(parallel)
library(doParallel)
library(MLmetrics)
library(ordinalForest)
library(gbm)

cls = makeCluster(2)
registerDoParallel(cls) 

#### Controls ####
tc_class <-trainControl(method="cv", number=10,
                        summaryFunction=twoClassSummary,
                        classProb=T,
                        savePredictions = T,
                        allowParallel = T)

tc_cont <-trainControl(method="cv", number=10,
                       savePredictions = T,
                       allowParallel = T)

#### Loop ####

out_score_list <- list()
out_group_list <- list()
out_group2_list <- list()

for(i in 1:4){
  sco <- c("AAS6Score", "ARAS7Score",
           "ARAS_S5Score", "ARAS_H_Ave")
  
  cats <- c("AAS", "ARAS_S",
            "ARAS_P", "ARAS_H")
  grou <- paste(cats, "_Group", sep="")
  
  student_data <- get(paste("dat_no_", cats[i],
                            sep=""))[["Student"]] %>% drop_na()
  faculty_data <- get(paste("dat_no_", cats[i],
                            sep=""))[["Faculty"]] %>% drop_na()
  
  stud_x <- student_data %>%
    select(-grou, -sco)
  stud_y <- student_data %>%
    pull(sco[i])
  
  fac_x <- faculty_data %>%
    select(-grou, -sco)
  fac_y <- faculty_data %>%
    pull(sco[i])
  
  print("a")
  
  labs <- paste(c("S_", "F_"), sco[i], sep="")
  
  out_score_list[[labs[1]]] <- train(x=stud_x,
                                     y=stud_y,
                                     method="gbm",
                                     trControl=tc_cont,
                                     verbose=F)
  print("a")
  out_score_list[[labs[2]]] <- train(x=fac_x,
                                     y=fac_y,
                                     method="gbm",
                                     trControl=tc_cont,
                                     verbose=F)
  stud_df <- student_data %>%
    select(-grou[-i], -sco) %>% 
    filter(!!as.symbol(grou[i]) != "mid") %>%
    mutate(across(grou[i], ~as.logical(as.integer(.x)-1))) %>%
    as.data.frame()
  
  stud_df_x <- stud_df %>%
    select(-grou[i])
  
  stud_df_y <- stud_df %>%
    pull(grou[i])
  
  fac_df <- faculty_data %>%
    select(-grou[-i], -sco) %>%
    filter(!!as.symbol(grou[i]) != "mid") %>%
    mutate(across(grou[i], ~as.logical(as.integer(.x)-1))) %>%
    as.data.frame()
  
  fac_df_x <- fac_df %>%
    select(-grou[i])
  
  fac_df_y <- fac_df %>%
    pull(grou[i])
  
  labs_g <- paste(c("S_", "F_"), grou[i], sep="")
  
  form <- as.formula(paste(grou[i], "~.", sep=""))
  
  print("b")
  out_group2_list[[labs_g[1]]] <- gbm(form,
                                      data = stud_df,
                                      n.cores = 2,
                                      interaction.depth = 5,
                                      cv.folds = 5,
                                      train.fraction = 0.75)
  
  print("b")
  out_group2_list[[labs_g[2]]] <- gbm(form,
                                      data = fac_df,
                                      n.cores = 2,
                                      interaction.depth = 5,
                                      cv.folds = 5,
                                      train.fraction = 0.75)
  
  stud_df <- student_data %>%
    select(-grou[-i], -sco) %>%
    as.data.frame()
  
  fac_df <- faculty_data %>%
    select(-grou[-i], -sco) %>%
    as.data.frame()
  
  labs_g <- paste(c("S_", "F_"), grou[i], sep="")
  
  out_group_list[[labs_g[1]]] <- ordfor(grou[i],
                                        data=stud_df)
  
  out_group_list[[labs_g[2]]] <- ordfor(grou[i],
                                        data=fac_df)
  
}

save(list=c("out_group_list", 
            "out_group2_list", 
            "out_score_list"), 
     file="../Data/out_lists2.RData")

save(list=c("out_group_list",
            "out_group2_list", 
            "out_score_list"),
     file="../Data/out_lists.RData")



