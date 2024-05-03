#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse) #need this so I can use tidyverse functions for data cleaning and visualization
library(haven) #needed this to read in the por data file
library(caret)# needed this for machine learning
library(rsconnect) #need this for shiny web app

#Data Import and Cleaning
data_raw_tbl <- read_spss("../data/31119144.por") #using read_spss to import the spss file because this is the code I know how to use 

data_clean_tbl <- data_raw_tbl %>%
  as_tibble() %>% #converting to a tibble 
  select(CASEID, BBC3_1:BBC3_10, BBC10, PPGENDER, PPINC7, PPSTATEN,PARTYID4) %>% #choosing the columns I'll need later for data analysis 
  mutate(across(BBC3_1:BBC3_10, as.numeric), #converting specific variables to numeric for later analyses
         across(BBC3_1:BBC3_10,~ifelse(.==-1, NA, .)), #converting skips to NAs
         across(BBC3_1:BBC3_10,~ifelse(.==3, NA, .))) %>% #converting don't knows to NAs because I don't want them in my analysis
  filter(rowMeans(is.na(across(BBC3_1:BBC3_10)))< .40) %>% #getting rid of participants who had NAs for 4/10 questions because I only want to keep participants who answered a majority of the questions
  mutate(state = factor(PPSTATEN,
                        levels= c("11","12","13","14","15","16","21","22","23","31","32","33","34","35","41","42","43","44","45","46","47","51","52","53","54","55","56","57","58","59","61","62","63","64","71","72","73","74","81","82","83","84","85","86","87","88","91","92","93","94","95"),
                        labels= c("ME","NH","VT","MA","RI","CT","NY","NJ","PA","OH","IN","IL","MI","WI","MN","IA","MO","ND","SD","NE","KS","DE","MD","DC","VA","WV","NC","SC","GA","FL","KY","TN","AL","MS","AR","LA","OK","TX","MT","ID","WY","CO","NM","AZ","UT","NV","WA","OR","CA","AK","HI")), #facotring states so I have the value and the label for use in my shiny app as well as renaming it to make it easier for later
    vote_2020 = factor(BBC10,
                       levels= c("-1", "1","2","3","4"),
                       labels= c("Skipped","Joe Biden","Donald Trump","Someone Else","Did not vote")), #facotring vote choice so I have the value and label for use in my shiny app and renaming it to make it easier for later 
    gender = factor(PPGENDER,
                    levels = c("1","2"),
                    labels = c("Male","Female")), #facotring gender for later use and renaming it
    pid = factor(PARTYID4,
                    levels= c("1","2","3","4"),
                    labels= c("Republican","Democrat","Independent","Something else"))) %>% #factoring party id for later us and renaming it. 
  filter(pid != "Something else", #filtering out people who do not identify as Democrat, Republican, or Independent because they are not necessary for the hypotheses. 
         vote_2020 != "Skipped",
         vote_2020 != "Did not vote",
         vote_2020 != "Someone Else") %>% #filtering out people who skipped, did not vote, or voted for someone besides Trump or Biden becuase they are not necessary for my hypotheses
  select(-PPGENDER, -PARTYID4, -PPSTATEN, -BBC10)  #deleting the unfactored versions of these variables, i used my names because those are easier for me

true_correct <- c("BBC3_2", "BBC3_3", "BBC3_5", "BBC3_6") #creating a vector of accuracy questions with true as the correct answer because it was easier for me to do this as a separate vecotr than to put this into the pipe

false_correct <- c("BBC3_1", "BBC3_4", "BBC3_7", "BBC3_8", "BBC3_9", "BBC3_10") #creating a vector of accuracy questions with false as the correct answer. 

accuracy_tbl <- data_clean_tbl %>%  
  mutate(across(BBC3_1:BBC3_10,~replace_na(.,0))) %>% #covnverting NAs to zero so I can make accuracy scores
  mutate(across(all_of(true_correct), ~ if_else(. == 1, 1, 0))) %>% #using the vector of true correct to recode the correct answers as 1 and everything else as 0 using ifelse to select 1 and keep as 1 and make everything else 0. 
  mutate(across(all_of(false_correct), ~ if_else(. == 2, 1, 0))) %>% #same procedure as before but converting 2(the correct answers) to 1 for the vector of false correct answers and everything else as zero
  mutate(accuracy_count = rowSums(select(.,BBC3_1: BBC3_10))) %>% #using rowsums to count the number of ones for each questions and sum them so I have a count of how many questions each participant got correct and creating an accuracy_count column 
  mutate(accuracy_score = accuracy_count/10) #creating accuracy scores by the amount they got right out the number of questions and creating a column for those scores. 

#write_csv(accuracy_tbl, "../data/accuracy_tbl.csv") saving the accuracy score tibble as a csv file

#Visualization
(accuracy_tbl %>% #piping the data in as opposed to putting it in the first parentheses to make it easier for saving
    ggplot(aes(x=CASEID, y= accuracy_score)) + #putting the case id on x and accuracy socre on y so we can see the range of scores
    geom_jitter() + #jitter as oppsoed to point because people got the same scores
    labs(title= "Jitterplot of Accuracy Scores",#naming the plot
         xlab= "Case ID Number", #labeling x axis
         ylab= "Accuracy Score Percentage")) %>%
  ggsave("../figs/fig1.png", ., width=1920, height=1080, units="px") #saving to figures using guidelines from class for figure format
##using this method of ggplot so I can save the figure to the figs folder using ggsave
(accuracy_tbl %>% #piping the data in as opposed to putting it in the first parentheses to make it easier for saving
    ggplot(aes(x= accuracy_score)) +
    geom_histogram() + 
    labs(title= "Histogram of Accuracy Scores", #naming the plot
         xlab= "Accuracy Scores", #labeling x axis
         ylab= "Frequency")) %>% #labeling y axis
  ggsave("../figs/fig2.png", ., width=1920, height=1080, units="px") #saving to figures using guidelines from class for figure format

(accuracy_tbl %>% #piping the data in as opposed to putting it in the first parentheses to make it easier for saving
  ggplot(aes(x= pid, y= accuracy_score, fill= gender)) + #putting pid on x as the predictor, accuracy_score on y as the outcome, and fill gender so I can visualize score differences by gender and pid. 
      geom_boxplot() +
      labs(title= "Boxplot of Accuracy Score by Party Identification and Gender",
           xlab= "Party Identification",
           ylab= "Accuracy Score Percentage")) %>%
  ggsave("../figs/fig3.png", ., width=1920, height=1080, units="px") #saving to figures using guidelines from class for figure format

(accuracy_tbl %>%
    ggplot(aes(x= pid, y= accuracy_score, fill= vote_2020)) + #putting pid on x as the predictor, accuracy_score on y as the outcome, and fill vote_2020 so I can visualize score differences by vote choice and pid. 
       geom_boxplot() +
       labs(title = "Boxplot of Accuracy Score by Party ID and Vote 2020", #giving figure a title
            xlab= "Party Identification", #labeling the x-axis
            ylab="Accuracy Score Percentage")) %>% #labeling the y- axis
  ggsave("../figs/fig4.png", ., width=1920, height=1080, units="px") #saving to figures using guidelines from class for figure format

#Analysis

#Descriptive Analysis
state_summary_tbl <- accuracy_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(state) %>% #grouping by state using the group_by function from dplyr to be consistent in using tidyverse code
  summarise(count_state =n(),
            avg_accuracy_state = mean(accuracy_score), 
            sd_accuracy_state= sd(accuracy_score)) 
#using summarise to get the count, mean, and sd for accuracy scores by state to be consistent in using tidyverse code

#write_csv(state_summary_tbl,"../figs/state_summary_tbl.csv") #saving table as a csv file 

pid_summary_tbl <- accuracy_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(pid) %>% #grouping by pid using the group_by function from dplyr to be consistent in using tidyverse code
  summarise(count = n(),
            avg_accuracy_pid = mean(accuracy_score),
            sd_accuracy_pid = sd(accuracy_score))
#using summarise to get the count, mean, and sd for accuracy scores by party id to be consistent in using tidyverse code

#write_csv(pid_summary_tbl,"../figs/pid_summary_tbl.csv") saving pid summary tibble as a csv file

gender_summary_tbl <- accuracy_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(gender) %>% #grouping by gender using the group_by function from dplyr to be consistent in using tidyverse code
  summarise(count= n(),
            avg_accuracy_gender = mean(accuracy_score),
            sd_accuracy_gender = sd(accuracy_score))
#using summarise to get the count, mean, and sd for accuracy scores by gender to be consistent in using tidyverse code

#write_csv(gender_summary_tbl,"../figs/gender_summary_tbl.csv") saving gender summary tibble as a csv file

vote_summary_tbl <- accuracy_tbl %>% #saving as a tibble so I can display the table in the publication section
  group_by(vote_2020) %>% #grouping by vote choice using the group_by function from dplyr to be consistent in using tidyverse code
  summarise(count = n(),
            avg_accuracy_vote = mean(accuracy_score),
            sd_accuracy_vote = sd(accuracy_score))
#using summarise to get the count, mean, and sd for accuracy scores by vote choice to be consistent in using tidyverse code

#write_csv(vote_summary_tbl,"../figs/vote_summary_tbl.csv") saving vote choice summary tibble as a csv file

###OLS regression analysis
pid_model <- lm(accuracy_score~pid, data= accuracy_tbl) 
summary(pid_model)

gender_model <- lm(accuracy_score~gender, data=accuracy_tbl)
summary(gender_model)

vote_2020_model <- lm(accuracy_score~vote_2020, data=accuracy_tbl)
summary(vote_2020_model)

interaction_model <- lm(accuracy_score~pid*gender*vote_2020, data= accuracy_tbl)
summary(interaction_model)
#### Machine Learning
holdout_indices <- createDataPartition(accuracy_tbl$accuracy_score,
                                       p = .50,
                                       list = T)$Resample1 #using data partition to split data in half to be used to create training and test datasets for machine learning
test_tbl <- accuracy_tbl[holdout_indices,] #creating the test/holdout data by selecting the holdout indices from thd accuracy_tbl
training_tbl <- accuracy_tbl[-holdout_indices,] #creating the training data by selecting not the holdout indices. 

training_folds <- createFolds(training_tbl$accuracy_score) #creating folds for indexing in machine learning

model1 <- train(
  accuracy_score ~ vote_2020*pid*gender, #predicting accuracy score by vote choice in 2020, party identification, and gender
  training_tbl, #using the training data
  method="lm", #used lm for OLS regression to compare to other models later
  na.action = na.pass, #so model will pass nas and actually run because otherwise it won't
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", #method cv to create cross-validation 
                           number=5,  #choosing ten folds
                           verboseIter=T, 
                           indexOut = training_folds) #using the folds created earlier
)   
model1
cv_m1 <- model1$results$Rsquared #identifying the Rsquared for the training model
holdout_m1 <- cor(
  predict(model1, test_tbl, na.action = na.pass), #running predict to run the model on the test data
  test_tbl$accuracy_score
)^2 #calculating the Rsquared for the holdout model 

model2 <- train(
  accuracy_score ~ vote_2020*pid*gender,
  training_tbl,
  method="glmnet", #method elastic net model
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=5, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
model2
cv_m2 <- max(model2$results$Rsquared) #this takes the largest Rsquared for model 2
holdout_m2 <- cor(
  predict(model2, test_tbl, na.action = na.pass), #running predict to run the model on the test data
  test_tbl$accuracy_score
)^2 #calculating Rsquared from the test data by wrapping it in cor()^2

model3 <- train(
  accuracy_score ~ vote_2020*pid*gender,
  training_tbl,
  tuneGrid= expand.grid(mtry= c(2,12,18), #changed mtry because my tibble only has 18 variables so I changed from 23 to 18 as the last mtry because without this the model kept failing
                        splitrule= 'variance',
                        min.node.size= 5),
  method="ranger", #method ranger for random forest model
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=5, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
model3
cv_m3 <- max(model3$results$Rsquared)
holdout_m3 <- cor(
  predict(model3, test_tbl, na.action = na.pass),
  test_tbl$accuracy_score
)^2 #calculating Rsquared from the test data by wrapping it in cor()^2

model4 <- train(
  accuracy_score ~ vote_2020*pid*gender,
  training_tbl,
  method="xgbLinear", #method xtreme gradient boosting
  na.action = na.pass,
  tuneLength = 1,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=5, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
model4
cv_m4 <- max(model4$results$Rsquared) #this takes the largest Rsquared for model 4
holdout_m4 <- cor(
  predict(model4, test_tbl, na.action = na.pass), #running the model with the test data 
  test_tbl$accuracy_score
)^2 #calculating Rsquared from the test data by wrapping it in cor()^2

summary(resamples(list(model1, model2, model3, model4)), metric="Rsquared")
dotplot(resamples(list(model1, model2, model3, model4)), metric="Rsquared")

#Publication

#function 
make_it_pretty <- function (formatme) {
  formatme <- formatC(formatme, format="f", digits=2) #only 2 digits after the point
  formatme <- str_remove(formatme, "^0") #removes the leading zero 
  return(formatme)
}
#using the make it pretty function for week10 machine learning project

#descriptive tibbles
gender_summary_tbl #viewing the summary data by gender

pid_summary_tbl #viewing the summary data by party id

View(state_summary_tbl)#viewing the summary data by state within the view since there's a lot of rows

vote_summary_tbl #viewing the summary data by 2020 vote choice 

#machine learning tibble
table1_tbl <- tibble(
  algo = c("regression","elastic net","random forests","xgboost"), #specifies which model 
  cv_rqs = c(
    make_it_pretty(cv_m1), #using the make_it_pretty function on the Rsquared from training model 1
    make_it_pretty(cv_m2), #using the make_it_pretty function on the Rsquared from training model 2
    make_it_pretty(cv_m3), #using the make_it_pretty function on the Rquared from training model 3
    make_it_pretty(cv_m4).#using the make_it_pretty function on the Rquared from training model 4
  ),
  ho_rqs = c(
    make_it_pretty(holdout_m1), #using the make_it_pretty_function on the Rsquared from the holdout model 1
    make_it_pretty(holdout_m2), #using the make_it_pretty_function on the Rsquared from the holdout model 2
    make_it_pretty(holdout_m3), #using the make_it_pretty_function on the Rsquared from the holdout model 3
    make_it_pretty(holdout_m4)#using the make_it_pretty_function on the Rsquared from the holdout model 4
  )
)

table1_tbl #viewing the table with the Rsquared from machine learning models

#Data Export
accuracy_tbl %>% 
  select(-BBC3_1:BBC3_10) %>% #deleting the specific misinformation questions because I only need the accuracy counts and percentages for my web app
  saveRDS("../shiny/final_shiny/import.RDS") #saving a skinny version of the file for shiny
