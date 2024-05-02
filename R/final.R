#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)

#Data Import and Cleaning
data_numeric_tbl <- read_spss("../data/31119144.por") #using read_spss to import the spss file because this is the code I know how to use :)

data_character_tbl <- read_csv("../data/31119144.csv") #using read_csv to import as a tibble so I can use it later. 

data_clean_tbl <- data_numeric_tbl %>%
  as_tibble() %>% #converting to a tibble 
  select(CASEID, BBC2_1:BBC2_11, BBC3_1:BBC3_10, BBC4_1:BBC4_8, BBC5_1:BBC5_5, BBC6, BBC7, BBC10, PPGENDER, PPETHM, PPINC7, PPSTATEN, URB_SUB_,PARTYID4, WEIGHTS) %>% #choosing the columns I'll need later for data analysis 
  mutate(
    gender = factor(PPGENDER,
                    levels = c("1","2"),
                    labels = c("Male","Female")),
    pid = factor(PARTYID4,
                    levels= c("1","2","3","4"),
                    labels= c("Republican","Democrat","Independent","Something_else"))) %>%
  select(-PPGENDER, -PARTYID4) %>%
  mutate(across(everything(), as.numeric))
  #mutating all the columns to numeric so I can do data analysis on them later. 
 
#mis_questions <- data_numeric_tbl %>%
 # as_tibble() %>% #converting to a tibble 
 # select(CASEID, BBC3_1:BBC3_10) %>% #choosing the columns I'll need later for data analysis
 # mutate(across(everything(), as.numeric)) 


data_char_clean_tbl <- data_character_tbl %>%
  select(CaseID, BBC2_1:BBC2_11, BBC3_1:BBC3_10, BBC4_1:BBC4_8, BBC5_1:BBC5_5, BBC6, BBC7, BBC10, ppgender, ppethm, ppinc7, ppstaten, urb_sub_rur, partyid4, Weights) #choosing the same columns from the numeric one. I could combine these but I don't want to 

true_correct <- c("BBC3_2", "BBC3_3", "BBC3_5", "BBC3_6") #creating a vector of accuracy questions with true as the correct answer

false_correct <- c("BBC3_1", "BBC3_4", "BBC3_7", "BBC3_8", "BBC3_9", "BBC3_10") #creating a vector of accuracy questions with false as the correct answer. 

accuracy_tbl <- data_clean_tbl %>% 
  mutate(across(all_of(true_correct), ~ if_else(. == 1, 1, 0))) %>% #using the vector of true correct to recode the correct answers as 1 and everything else as 0 using ifelse to select 1 and keep as 1 and make everything else 0. 
  mutate(across(all_of(false_correct), ~ if_else(. == 2, 1, 0))) %>% #same procedure as before but converting 2(the correct answers) to 1 for the vector of false correct answers and everything else as zero
  mutate(accuracy_count = rowSums(select(.,BBC3_1: BBC3_10)))  #using rowsums to count the number of ones for each questions and sum them so I have a count of how many questions each participant got correct.  

#Visualization

#Analysis

m1<-glm(cbind(accuracy_count, 10)~pid + gender, family=binomial(), data=accuracy_tbl)
summary(m1) #will be deleted later, just needed to see if my code was working


#Publication