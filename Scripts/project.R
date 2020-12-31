#### Top level ####
setwd("C:/Users/Michael/Google Drive/R stuff/3 - Fall 2020/POLISCI919/Project")

library(tidyverse)
library(broom)
library(labelled)
library(grid)
library(gtable)
library(ggh4x)
library(ggpmisc)
library(gridExtra)

load("./Data/proj_vec.RData")
load("./Data/dat_file.RData")
source("./Scripts/functions.R")
#load("./Data/out_lists.RData")
#load("./Data/out_lists2.RData")
load("./Data/var_imp.RData")
load("./Data/vecs.RData")
load("./Data/description.RData")

#### ___ ####

#### Question vectors ####
# Scale aggregate scores
scores <- c("AAS6Score", "ARAS7Score", "ARAS_S5Score")

# Questions of interest
interest <- c(paste("q", c(1, 2, 4, 12:15, 20), sep=""),
              "q21a", "q22b", scores)

# Animal Attitude Scale (AAS)
AAS <- c(paste("q3", letters[1:6], sep=""))

# Animal Research Attitude Scale - Purpose (ARAS-P)
ARAS_P <- c("ARAS_anim_dis", "ARAS_agr", "ARAS_basic_res",
            "ARAS_hum_dis", "ARAS_med_hum", "ARAS_chemicals",
            "ARAS_cosmetics")

# Animal Research Attitude Scale - Species (ARAS-S)
ARAS_S <- c(paste("q6", letters[1:5], sep=""))

# Animal Research Attitude Scale - Species (ARAS-PD)
ARAS_PD <- c(paste("q7", letters[c(1, 3:7)], sep=""),
             paste("q8", letters[c(1, 3:7)], sep=""),
             paste("q9", letters[1:7], sep=""),
             paste("q10", letters[1:7], sep=""),
             paste("q11", letters[1:7], sep=""))

# Other types of questions
importance <- "q1"
knowledge <- c("q2", "q12", "q20")
transl <- "q4"
rules <- c("q12", "q13", "q14", "q15", "q16")
sources <- c(paste("q17", letters[1:7], sep=""),
             paste("q18", letters[1:7], sep=""),
             paste("q19", letters[1:7], sep=""))
personal <- c("q21a", "q22a")
personal_full <- c(paste("q21", letters[1:2], sep=""),
                   paste("q22", letters[1:6], sep=""),
                   paste("q", 23:26, sep=""),
                   paste("q27", letters[1:6], sep=""))

# "Don't know" questions
dont_know <- c(paste("q", 12:16, sep=""), "q20")

# Yes/no questions
yn <- c(paste("q21", letters[1:2], sep=""), 
        paste("q22", letters[1:6], sep=""))

# All questions
all <- c(AAS, ARAS_P, ARAS_S, ARAS_PD, importance,
         knowledge, transl, rules, rules, sources,
         personal_full, scores, yn, scores)

#### Categories ####
categories <- tibble(Question=all,
                     category=case_when(
                       Question == "q1" ~ "General",
                       Question == "q2" ~ "General",
                       Question == "q20" ~ "General",
                       Question %in% sources ~ "Sources",
                       Question %in% rules ~ "Rules",
                       Question %in% ARAS_P ~ "ARAS-P",
                       Question %in% ARAS_PD ~ "ARAS-PD",
                       Question %in% ARAS_S ~ "ARAS-S",
                       Question %in% AAS ~ "AAS",
                       Question %in% personal_full ~ "Personal",
                       Question %in% transl ~ "General",
                       Question %in% scores ~"Score"
                     )
  ) %>%
  distinct() %>%
  rename(variable=Question) %>%
  add_row(tibble(variable=c("division", "gender_m_d", "racenew"),
                 category=rep("Demographic", 3)))

# Save RData object
save(AAS, all, ARAS_P, ARAS_PD, ARAS_S, dont_know,
     importance, interest, knowledge, personal,
     personal_full, rules, scores,
     sources, transl, yn, categories,
     file = "./Data/proj_vec.RData")

#### Question Description ####

descr <- c("Score: Animal Attitude",
           "Purpose: Agriculture",
           "Purpose: Animal Disease",
           "Purpose: Basic Research",
           "Purpose: Chemical Safety",
           "Purpose: Cosmetic Safety",
           "Score: Pain/Distress",
           "Purpose: Human Disease",
           "Purpose: Human Medication",
           "Score: Species",
           "Score: Purpose",
           "Topic Importance",
           "Topic Knowledge",
           "Attitude: Sport Hunting",
           "Attitude: Medical Research",
           "Attitude: Cow/Pig Consumption",
           "Attitude: Purebreed Dogs",
           "Attitude: Upset at Zoos",
           "Attitude: Anthropocentrism",
           "Translatability",
           "Species: Monkeys",
           "Species: Dogs/Cats",
           "Species: Pigs/Sheep",
           "Species: Rats/Mice",
           "Species: Small Fish",
           "Harm (Monkeys): Animal Diseases",
           "Harm (Monkeys): Basic Research",
           "Harm (Monkeys): Human Diseases",
           "Harm (Monkeys): Human Medication",
           "Harm (Monkeys): Chemical Safety",
           "Harm (Monkeys): Cosmetic Safety",
           
           "Harm (Dogs/Cats): Animal Diseases",
           "Harm (Dogs/Cats): Basic Research",
           "Harm (Dogs/Cats): Human Diseases",
           "Harm (Dogs/Cats): Human Medication",
           "Harm (Dogs/Cats): Chemical Safety",
           "Harm (Dogs/Cats): Cosmetic Safety",
           
           "Harm (Pigs/Sheep): Animal Diseases",
           "Harm (Pigs/Sheep): Agriculture",
           "Harm (Pigs/Sheep): Basic Research",
           "Harm (Pigs/Sheep): Human Diseases",
           "Harm (Pigs/Sheep): Human Medication",
           "Harm (Pigs/Sheep): Chemical Safety",
           "Harm (Pigs/Sheep): Cosmetic Safety",
           
           "Harm (Rats/Mice): Animal Diseases",
           "Harm (Rats/Mice): Agriculture",
           "Harm (Rats/Mice): Basic Research",
           "Harm (Rats/Mice): Human Diseases",
           "Harm (Rats/Mice): Human Medication",
           "Harm (Rats/Mice): Chemical Safety",
           "Harm (Rats/Mice): Cosmetic Safety",
           
           "Harm (Small Fish): Animal Diseases",
           "Harm (Small Fish): Agriculture",
           "Harm (Small Fish): Basic Research",
           "Harm (Small Fish): Human Diseases",
           "Harm (Small Fish): Human Medication",
           "Harm (Small Fish): Chemical Safety",
           "Harm (Small Fish): Cosmetic Safety",
           
           "Regulations: Knowledge",
           "Regulations: Sufficciency",
           "Regulations: Enforcement",
           "Regulations: Proposal Review",
           "Regulations: Minimize Suffering",
           
           "Sources (Trust): News Media",
           "Sources (Trust): Advertisements",
           "Sources (Trust): Social Media",
           "Sources (Trust): Social Circle/Family",
           "Sources (Trust): UW Courses",
           "Sources (Trust): UW Spokespeople",
           "Sources (Trust): Activist Groups",
           
           "Sources (Relevance): News Media",
           "Sources (Relevance): Advertisements",
           "Sources (Relevance): Social Media",
           "Sources (Relevance): Social Circle/Family",
           "Sources (Relevance): UW Courses",
           "Sources (Relevance): UW Spokespeople",
           "Sources (Relevance): Activist Groups",
           
           "Sources (Amount): News Media",
           "Sources (Amount): Advertisements",
           "Sources (Amount): Social Media",
           "Sources (Amount): Social Circle/Family",
           "Sources (Amount): UW Courses",
           "Sources (Amount): UW Spokespeople",
           "Sources (Amount): Activist Groups",
           
           "Self-Assessed Knowledge",
           
           "Personal: Did Animal Research",
           "Personal: Used Animal Research",
           
           "Personal: Vegan/Vegetarian",
           "Personal: Animal Activism",
           "Personal: Supporting Petition",
           "Personal: Opposing Petition",
           "Personal: Fishing",
           "Personal: Hunting",
           
           "Demographic: Major",
           "Demographic: Birth Year",
           "Demographic: Gender",
           "Demographic: Hispanic/Latino",
           "Demographic (Race): White",
           "Demographic (Race): Black",
           "Demographic (Race): Indigenous",
           "Demographic (Race): Asian",
           "Demographic (Race): Pacific Islander",
           "Demographic (Race): Other")

desc_tab <- tibble(variable=unique(
  str_sort(c("ARAS_H_Ave", all),
           numeric = T)),
  descr=descr)

save(list=c("descr", "desc_tab"),
     file="./Data/description.RData")

#### ___ ####

#### All data - read and clean ####
# Vectors of variable names to remove
a <- c("acad_level_label", "acad_level_Fres",
       "acad_level_Soph","acad_level_Jun", "acad_level_Sen",
       "dvsn_BioRef", "dvsn_HumanRef","dvsn_PhyRef",
       "dvsn_SocRef", "dvsn_GenRef",
       "s_freshman", "s_sophomore", "s_junior", "s_senior",
       'divisionHumanitiesCat1', 'divisionBiologicalSciCat2' ,
       'divisionPhysicalSciCat3' , 'divisionSocialSciCat4' ,
       'divisionTotal' , 'division1Fraction_P' ,
       'division2Fraction_P' , 'division3Fraction_P' ,
       'division4Fraction_P' , 'division1Fraction_S' ,
       'division2Fraction_S' , 'division3Fraction_S' ,
       'division4Fraction_S' , 'division1_S_F' , 
       'division2_S_F' , 'division3_S_F' , 'division4_S_F')
ar <- c("ARAS_anim_dis", "ARAS_agr", "ARAS_basic_res",
        "ARAS_hum_dis", "ARAS_med_hum", "ARAS_chemicals",
        "ARAS_cosmetics")
non_ord <- c("q18", paste("q", 21:27, sep=""))

# Data itself 
dat <- read_dta("./Data/survdata.dta")

dat2 <- dat %>%
  dplyr::select(# Experiment info - useless to me
    -comments, -experiment_group, -`_merge`,
    -page, -submissions, -loaded, -time,
    -survey_type, -cmpl_date,
    -FvsS_D, -SvsF_D,
    -SCREEN_WIDTH, -SCREEN_HEIGHT, -SCREEN_STYLE,
    -contains("String"),
    -starts_with("weight"),
    -starts_with("rake"),
    # Duplicates
    -JobTitle, -Dept_Unit, -exp_group,
    -starts_with("major"),
    # Omnibus
    -one_of(a)) %>%
  mutate(
    
    # Create groups for research question
    ARAS_H_Group = factor(case_when(
      between(ARAS_H_Ave, 1, 2.5)~"low",
      between(ARAS_H_Ave, 2.5, 3.5)~"mid",
      between(ARAS_H_Ave, 3.5, 5)~"high"
    )),
    ARAS_P_Group = factor(case_when(
      between(ARAS7Score, 1, 2.5)~"low",
      between(ARAS7Score, 2.5, 3.5)~"mid",
      between(ARAS7Score, 3.5, 5)~"high"
    )),
    ARAS_S_Group = factor(case_when(
      between(ARAS_S5Score, 1, 2.5)~"low",
      between(ARAS_S5Score, 2.5, 3.5)~"mid",
      between(ARAS_S5Score, 3.5, 5)~"high"
    )),
    AAS_Group = factor(case_when(
      between(AAS6Score, 1, 2.5)~"low",
      between(AAS6Score, 2.5, 3.5)~"mid",
      between(AAS6Score, 3.5, 5)~"high"
    )),
    #ARAS_H_Group = fct_relevel(ARAS_H_Group, "mid"),
    
    # Remove &s from certain questions
    #   TODO: Check that this doesn't have unintended
    #   consequences
    across(w(is.character), 
           function(x) str_replace(x, pattern="&",
                                   replacement = "")),
    
    # All of the questions - not the summary statistics/
    #    randomization order/etc
    across(starts_with("q")&!contains("Total")&
             !contains("random")&
             !contains("Score")&!contains("Sum")&
             !contains("Exp")&!starts_with(!!non_ord)&
             !ends_with("_d")&!contains("q13")&
             !contains("q18"),
           # The _d ones are coded binary.
           #   Maybe worth looking at?
           as.ordered),
    
    # Reordering ordinal factor levels
    #   TODO: Clean up the formatting
    #   TODO: Check that the orders are consisten for each
    q1=factor(q1, levels=1:5,
              labels=c("Not at all important",
                   "A little important", "Somewhat important",
                   "Very important", "Extremely important")),
    across(c(q2, q12), function(x){
      factor(x, levels=1:5,
             labels=c("Nothing", "A little", "Some",
                  "Quite a bit", "A great deal"))}),
    across(paste("q3", letters[1:6], sep=""), 
           function(x){
             factor(x, levels=1:5,
                      labels=c("Strongly disagree",
                         "Disagree",
                         "Neither agree nor disagree",
                         "Agree", "Strongly agree"))}),
    q3a = fct_rev(q3a),
    q3d = fct_rev(q3d),
    q3e = fct_rev(q3e),
    q4=factor(q4, levels=1:5,
              labels=c("Not at all", "A little", "Somewhat",
                   "Quite a bit", "A great deal")),
    across(starts_with("q6")&!ends_with("random"), 
           function(x){
             factor(x, levels=1:5,
                    labels=c("Never", "Rarely",
                         "Sometimes", "Usually", "Always"))}),
    across(starts_with(paste("q", 7:11, sep=""))&
             !ends_with("random")&!ends_with("_d")&
             !ends_with("Sum"),
           function(x){
             factor(x, levels=1:5,
                    labels=c("None", "A little", "Some",
                         "Quite a bit", "A great deal"))}),
    q13=to_factor(q13, ordered=T),
    #q13 = fct_rev(q13),
    q14=factor(q14, levels=c(-1, 1:5),
                    labels=c("Don't know",
                    "Not at all well", "A little well",
                    "Somewhat well", "Very well", 
                    "Extremely well")),
    q15=factor(q15, levels=c(-1, 1:5),
                    labels=c("Don't know", 
                    "Not at all", "A little",
                    "Somewhat", "Very", 
                    "Extremely")),
    q16 = factor(q16, levels=c(-1, 1:5), 
                      labels=c("Don't know", "Never",
                      "Rarely", "Sometimes",
                      "Usually", "Always")),
    across(starts_with("q17")&
             !ends_with("random")&
             !ends_with("Total"), 
           function(x){
             factor(x, levels=1:5, 
                    labels=c("Not at all", 
                         "A little", "Some",
                         "Quite a bit", "A great deal"))}),
    across(paste("q19", letters[1:7], sep=""),
           function(x){
             factor(x, levels=1:3,
                         labels=c("Too little", "Just enough",
                         "Too much"))}),
    q20=factor(q20, levels=c(-1, 1:5),
               labels=c("Don't know", 
                    "Not at all", "A little", "Somewhat",
                    "Quite a bit", "A great deal")),
    q12DontKnow_d = factor(ifelse(q12=="Nothing",
                                  "Don't know",
                                  "Substantive Knowledge")),
    # Transform non-ordinal questions to factor,
    #    including randomization order
    across(starts_with("q")&!contains("Total")&
             !contains("Score")&!contains("Sum")&
             !contains("Exp")&starts_with(!!non_ord)&
             !is.ordered&!contains("q18")&
             !contains("q21")&!contains(paste("q", 22:27,
                                              sep="")), 
           as.factor),
    # Transform ARAS questions (q5_)
    across(matches(!!ar), as.ordered),
    across(matches(!!ar), ~factor(.x, levels=1:5,
                                  labels=c("Never", "Rarely",
                                       "Sometimes", "Usually",
                                       "Always"))),
    ARAS_H_Group = factor(ARAS_H_Group),
    across(w(is.character), as.factor),
    caseid_original = as.character(caseid_original),
    #q22b = fct_rev(q22b),
    AAS6Score = 6-AAS6Score,
    across(is.labelled, to_factor),
    across(contains("_Group"),
           function(x){as.ordered(fct_relevel(x, "low",
                                              "mid", "high"))})
  ) %>%
  rename(S_or_F = data)

dat_all_important <- dat2 %>%
                      dplyr::select(all,
                                    contains("score"),
                                    ARAS_H_Ave,
                                    contains("group"),
                                    S_or_F,
                                    division,
                                    gender_m_d,
                                    racenew,
                                    -contains("composite"),
                                    -Group_d,
                                    -q23,
                                    -contains("q27"))

dat_no_AAS <- dat_all_important %>%
  select(-all_of(AAS)) %>%
  split(., .$S_or_F)

dat_no_AAS[[1]] <- dat_no_AAS[[1]] %>%
                    select(-S_or_F)
dat_no_AAS[[2]] <- dat_no_AAS[[2]] %>%
                    select(-S_or_F)

dat_no_ARAS_P <- dat_all_important %>%
  select(-all_of(ARAS_P)) %>%
  split(., .$S_or_F)

dat_no_ARAS_P[[1]] <- dat_no_ARAS_P[[1]] %>%
  select(-S_or_F)
dat_no_ARAS_P[[2]] <- dat_no_ARAS_P[[2]] %>%
  select(-S_or_F)

dat_no_ARAS_S <- dat_all_important %>%
  select(-all_of(ARAS_S)) %>%
  split(., .$S_or_F)

dat_no_ARAS_S[[1]] <- dat_no_ARAS_S[[1]] %>%
  select(-S_or_F)
dat_no_ARAS_S[[2]] <- dat_no_ARAS_S[[2]] %>%
  select(-S_or_F)

dat_no_ARAS_H <- dat_all_important %>%
  select(-all_of(ARAS_PD)) %>%
  split(., .$S_or_F)

dat_no_ARAS_H[[1]] <- dat_no_ARAS_H[[1]] %>%
  select(-S_or_F)
dat_no_ARAS_H[[2]] <- dat_no_ARAS_H[[2]] %>%
  select(-S_or_F)

save(list=c("dat2", "dat_all_important",
            "dat_no_AAS", "dat_no_ARAS_P",
            "dat_no_ARAS_S", "dat_no_ARAS_H"),
     file="./Data/dat_file.RData")

#### ___ ####

#### Variable Importance ####

score_imp <- list()
group_imp <- list()
group2_imp <- list()

labs_s <- c(apply(expand.grid(c("S_", "F_"),
                              c("AAS6Score", "ARAS7Score",
                                "ARAS_S5Score", "ARAS_H_Ave")),
                1, paste, collapse="", sep=""))

labs_g <- c(apply(expand.grid(c("S_", "F_"),
                              c("AAS_Group", "ARAS_S_Group",
                                "ARAS_P_Group", "ARAS_H_Group")),
                  1, paste, collapse="", sep=""))

for(i in 1:length(out_group2_list)){
  
  l_s <- labs_s[i]
  mod_s <- out_score_list[[l_s]]$finalModel
  score_imp[[l_s]] <- tidy.gbm(mod_s)
  
  l_g <- labs_g[i]
  mod_g <- out_group_list[[l_g]]$forestfinal
  group_imp[[l_g]] <- tidy.ordfor(mod_g)
  
  mod_g2 <- out_group2_list[[l_g]]
  group2_imp[[l_g]] <- tidy.gbm(mod_g2)
  
}

sco <- c("AAS6Score", "ARAS7Score",
         "ARAS_S5Score", "ARAS_H_Ave")
cats <- c("AAS", "ARAS_S",
          "ARAS_P", "ARAS_H")
grou <- paste(cats, "_Group", sep="")

save(list=c("sco", "cats", "grou"),
     file="./Data/vecs.RData")

group_imp_long <- list()
group2_imp_long <- list()
score_imp_long <- list()

for(i in c(1, 3, 5, 7)){
group_imp_long[[grou[(i%/%2)+1]]] <- group_imp[c(i, i+1)] %>%
    bind_rows(., .id="S_or_F") %>%
    mutate(S_or_F = str_sub(S_or_F, 1, 1),
           S_or_F = case_when(
             S_or_F=="S" ~ "Student",
             TRUE ~ "Faculty"
           ),
           subq=ifelse(str_detect(variable, "^q[:digit:].*[a-z]$"),
                       str_sub(variable, -1, -1),
                       NA),
           subq=ifelse(str_detect(variable, "ARAS"),
                       str_sub(variable, 6, -1),
                       subq),
           question=ifelse(str_detect(variable, "^q[:digit:].*[a-z]$"),
                           str_sub(variable, 1, -2),
                           NA),
           question=ifelse(str_detect(variable, "ARAS"),
                           "ARAS",
                           question),
           question=ifelse(is.na(question),
                           variable,
                           question),
           question=factor(question),
           subq=factor(subq),
           subq=fct_relevel(subq, letters[1:7]))
  
  
group2_imp_long[[grou[(i%/%2)+1]]] <- group2_imp[c(i, i+1)] %>%
  bind_rows(., .id="S_or_F") %>%
  mutate(S_or_F = str_sub(S_or_F, 1, 1),
         S_or_F = case_when(
           S_or_F=="S" ~ "Student",
           TRUE ~ "Faculty"
         ),
         subq=ifelse(str_detect(variable, "^q[:digit:].*[a-z]$"),
                     str_sub(variable, -1, -1),
                     NA),
         subq=ifelse(str_detect(variable, "ARAS"),
                     str_sub(variable, 6, -1),
                     subq),
         question=ifelse(str_detect(variable, "^q[:digit:].*[a-z]$"),
                         str_sub(variable, 1, -2),
                         NA),
         question=ifelse(str_detect(variable, "ARAS"),
                         "ARAS",
                         question),
         question=ifelse(is.na(question),
                         variable,
                         question),
         question=factor(question),
         subq=factor(subq),
         subq=fct_relevel(subq, letters[1:7])) %>%
         left_join(categories, by="variable")

score_imp_long[[sco[(i%/%2)+1]]] <- score_imp[c(i, i+1)] %>%
  bind_rows(., .id="S_or_F") %>%
  mutate(S_or_F = str_sub(S_or_F, 1, 1),
         S_or_F = case_when(
           S_or_F=="S" ~ "Student",
           TRUE ~ "Faculty"
         ),
         subq=ifelse(str_detect(variable, "^q[:digit:].*[a-z]$"),
                     str_sub(variable, -1, -1),
                     NA),
         subq=ifelse(str_detect(variable, "ARAS"),
                     str_sub(variable, 6, -1),
                     subq),
         question=ifelse(str_detect(variable, "^q[:digit:].*[a-z]$"),
                         str_sub(variable, 1, -2),
                         NA),
         question=ifelse(str_detect(variable, "ARAS"),
                         "ARAS",
                         question),
         question=ifelse(is.na(question),
                         variable,
                         question),
         question=factor(question),
         subq=factor(subq),
         subq=fct_relevel(subq, letters[1:7])) %>%
  left_join(categories, by="variable")

}


#### Plot Data ####

dat_plot_group <- group_imp_long %>%
  bind_rows(., .id="Group") %>%
  mutate(Group=str_sub(Group, 1,-7),
         subq = fct_rev(subq),
         S_or_F = fct_rev(S_or_F)) %>%
  group_by(Group, category, variable) %>%
  mutate(dif=diff(rel_importance),
         m=mean(rel_importance)) %>%
  ungroup() %>% 
  full_join(desc_tab, by="variable") %>% 
  mutate(descr=case_when(
    variable=="racenew"~"Demographic: Race",
    variable=="division"~"Demographic: Academic Division",
    variable=="gender_m_d"~"Demographic: Gender",
    T~descr
  ),
  subq=ifelse(is.na(subq),
              as.character(question),
              as.character(subq)),
  alt_descr = str_replace(descr,
                          "^(.*)\\s(\\(.*\\))(:.*)",
                          "\\1\\3 \\2"),
  short_descr = str_replace(alt_descr,
                            "^(.*):\\s(.*\\(.*\\)|.*)$",
                            "\\2"),
  category=case_when(
    category=="ARAS-P"~"Purpose",
    category=="ARAS-S"~"Species",
    category=="ARAS-PD"~"Pain/Distress",
    category=="AAS"~"Attitude",
    variable=="q24"~"Demographic",
    T~category
  ),
  category=factor(category,
                  levels=c("Attitude",
                           "Pain/Distress",
                           "Purpose",
                           "Species",
                           " ",
                           "Demographic",
                           "Personal",
                           "Sources",
                           "Rules",
                           "  ",
                           "General")))

dat_plot_group2 <- group2_imp_long %>%
  bind_rows(., .id="Group") %>%
  mutate(Group=str_sub(Group, 1,-7),
         subq = fct_rev(subq),
         S_or_F = fct_rev(S_or_F)) %>%
  group_by(Group, category, variable) %>%
  mutate(dif=diff(rel_importance),
         m=mean(rel_importance)) %>%
  ungroup() %>% 
  full_join(desc_tab, by="variable") %>% 
  mutate(descr=case_when(
    variable=="racenew"~"Demographic: Race",
    variable=="division"~"Demographic: Academic Division",
    variable=="gender_m_d"~"Demographic: Gender",
    T~descr
  ),
  subq=ifelse(is.na(subq),
              as.character(question),
              as.character(subq)),
  alt_descr = str_replace(descr,
                          "^(.*)\\s(\\(.*\\))(:.*)",
                          "\\1\\3 \\2"),
  short_descr = str_replace(alt_descr,
                            "^(.*):\\s(.*\\(.*\\)|.*)$",
                            "\\2"),
  category=case_when(
    category=="ARAS-P"~"Purpose",
    category=="ARAS-S"~"Species",
    category=="ARAS-PD"~"Pain/Distress",
    category=="AAS"~"Attitude",
    variable=="q24"~"Demographic",
    T~category
  ),
  category=factor(category,
                  levels=c("Attitude",
                           "Pain/Distress",
                           "Purpose",
                           "Species",
                       " ",
                       "Demographic",
                       "Personal",
                       "Sources",
                       "Rules",
                       "  ",
                       "General")))

dat_plot_score <- score_imp_long %>%
  bind_rows(., .id="Group") %>%
  mutate(Group=case_when(
    Group=="AAS6Score"~"AAS",
    Group=="ARAS7Score"~"ARAS-P",
    Group=="ARAS_S5Score"~"ARAS-S",
    T~"ARAS-PD"
  )) %>%
  group_by(Group, category, variable) %>%
  mutate(dif=diff(rel_importance),
         m=mean(rel_importance)) %>%
  ungroup() %>% 
  full_join(desc_tab, by="variable") %>% 
  mutate(descr=case_when(
    variable=="racenew"~"Demographic: Race",
    variable=="division"~"Demographic: Academic Division",
    variable=="gender_m_d"~"Demographic: Gender",
    T~descr
  ),
  subq=ifelse(is.na(subq),
              as.character(question),
              as.character(subq)),
  alt_descr = str_replace(descr,
                          "^(.*)\\s(\\(.*\\))(:.*)",
                          "\\1\\3 \\2"),
  short_descr = str_replace(alt_descr,
                            "^(.*):\\s(.*\\(.*\\)|.*)$",
                            "\\2"),
  category=case_when(
    category=="ARAS-P"~"Purpose",
    category=="ARAS-S"~"Species",
    category=="ARAS-PD"~"Pain/Distress",
    category=="AAS"~"Attitude",
    variable=="q24"~"Demographic",
    T~category
  ),
  category=factor(category,
                  levels=c("Attitude",
                           "Pain/Distress",
                           "Purpose",
                           "Species",
                           " ",
                           "Demographic",
                           "Personal",
                           "Sources",
                           "Rules",
                           "  ",
                           "General")))

save(list=c("score_imp", "group_imp",
            "score_imp_long", "group_imp_long",
            "group2_imp", "group2_imp_long",
            "dat_plot_group", "dat_plot_score",
            "dat_plot_group2"),
     file="./Data/var_imp.RData")

#### ___ ####
#### Plots ####

my_plot_func(dat_plot_group2, "ARAS_P", 12)
