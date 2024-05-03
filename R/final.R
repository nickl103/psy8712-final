#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse) 
library(haven) #needed this to read in the por data file
library(caret)# needed this for machine learning

#Data Import and Cleaning
data_raw_tbl <- read_spss("../data/31119144.por") #using read_spss to import the spss file because this is the code I know how to use 

data_clean_tbl <- data_raw_tbl %>%
  as_tibble() %>% #converting to a tibble 
  select(CASEID, BBC3_1:BBC3_10, BBC10, PPGENDER, PPETHM, PPINC7, PPSTATEN, URB_SUB_,PARTYID4) %>% #choosing the columns I'll need later for data analysis 
  mutate(across(BBC3_1:BBC3_10, as.numeric), #converting specific variables to numeric for later analyses
         across(BBC3_1:BBC3_10,~ifelse(.==-1, NA, .)), #converting skips to NAs
         across(BBC3_1:BBC3_10,~ifelse(.==3, NA, .))) %>% #converting don't knows to NAs because I don't want them in my analysis
  filter(rowMeans(is.na(across(BBC3_1:BBC3_10)))< .40) %>% #getting rid of participants who had NAs for 4/10 questions
  mutate(state = factor(PPSTATEN,
                        levels= c("11","12","13","14","15","16","21","22","23","31","32","33","34","35","41","42","43","44","45","46","47","51","52","53","54","55","56","57","58","59","61","62","63","64","71","72","73","74","81","82","83","84","85","86","87","88","91","92","93","94","95"),
                        labels= c("ME","NH","VT","MA","RI","CT","NY","NJ","PA","OH","IN","IL","MI","WI","MN","IA","MO","ND","SD","NE","KS","DE","MD","DC","VA","WV","NC","SC","GA","FL","KY","TN","AL","MS","AR","LA","OK","TX","MT","ID","WY","CO","NM","AZ","UT","NV","WA","OR","CA","AK","HI")),
    vote_2020 = factor(BBC10,
                       levels= c("-1", "1","2","3","4"),
                       labels= c("Skipped","Joe Biden","Donald Trump","Someone Else","Did not vote")),
    gender = factor(PPGENDER,
                    levels = c("1","2"),
                    labels = c("Male","Female")),
    pid = factor(PARTYID4,
                    levels= c("1","2","3","4"),
                    labels= c("Republican","Democrat","Independent","Something else")),
    race = factor(PPETHM,
                  levels = c("1","2","3","4","5","6"),
                  labels = c("White","Black or African American","American Indian or Alaskan Native", "Asian","Native Hawaiian or other Pacific Islander", "Some other race")),
    location = factor(URB_SUB_,
                      levels= c("1", "2", "3"),
                      labels= c("Urban","Rural","Suburban")))%>%
  filter(pid != "Something else", #filtering out people who do not identify as Democrat, Republican, or Independent because they are not necessary for the hypotheses. 
         vote_2020 != "Skipped",
         vote_2020 != "Did not vote",
         vote_2020 != "Someone Else") %>% #filtering out people who skipped, did not vote, or voted for someone besides Trump or Biden becuase they are not necessary for my hypotheses
  select(-PPGENDER, -PARTYID4, -PPETHM, -URB_SUB_, -PPSTATEN, -BBC10)  #deleting the unfactored versions of these variables, i used my names because those are easier for me
 
 
#mis_questions <- data_numeric_tbl %>%
 # as_tibble() %>% #converting to a tibble 
 # select(CASEID, BBC3_1:BBC3_10) %>% #choosing the columns I'll need later for data analysis
 # mutate(across(everything(), as.numeric)) 

true_correct <- c("BBC3_2", "BBC3_3", "BBC3_5", "BBC3_6") #creating a vector of accuracy questions with true as the correct answer

false_correct <- c("BBC3_1", "BBC3_4", "BBC3_7", "BBC3_8", "BBC3_9", "BBC3_10") #creating a vector of accuracy questions with false as the correct answer. 

accuracy_tbl <- data_clean_tbl %>%  
  mutate(across(BBC3_1:BBC3_10,~replace_na(.,0))) %>% #covnverting NAs to zero so I can make accuracy scores
  mutate(across(all_of(true_correct), ~ if_else(. == 1, 1, 0))) %>% #using the vector of true correct to recode the correct answers as 1 and everything else as 0 using ifelse to select 1 and keep as 1 and make everything else 0. 
  mutate(across(all_of(false_correct), ~ if_else(. == 2, 1, 0))) %>% #same procedure as before but converting 2(the correct answers) to 1 for the vector of false correct answers and everything else as zero
  mutate(accuracy_count = rowSums(select(.,BBC3_1: BBC3_10))) %>% #using rowsums to count the number of ones for each questions and sum them so I have a count of how many questions each participant got correct.  
  mutate(accuracy_score = accuracy_count/10) #creating accuracy scores by the amount they got right out the number of questions


#Visualization
(accuracy_tbl %>% 
  ggplot(aes(x= pid, y= accuracy_score, fill= gender)) + 
      geom_boxplot() +
      labs(title= "Accuracy Score by Party Identification and Gender",
           xlab= "Party Identification",
           ylab= "Accuracy Score Percentage")) %>%
  ggsave("../figs/fig1.png", ., width=1920, height=1080, units="px")

(accuracy_tbl %>%
    ggplot(aes(x= pid, y= accuracy_score, fill= vote_2020)) +
       geom_boxplot() +
       labs(title = "Accuracy Score by Party ID and Vote 2020", 
            xlab= "Party Identification",
            ylab="Accuracy Score Percentage")) %>%
  ggsave("../figs/fig2.png", ., width=1920, height=1080, units="px")

#Analysis
state_summary_tbl <- accuracy_tbl %>%
  group_by(state) %>%
  summarise(avg_accuracy = mean(accuracy_score),
            count= n()) 

pid_summary_tbl <- accuracy_tbl %>%
  group_by(pid) %>%
  summarise(count = n(),
            avg_accuracy_pid = mean(accuracy_score),
            sd_accuracy_pid = sd(accuracy_score))

gender_summary_tbl <- accuracy_tbl %>%
  group_by(gender) %>%
  summarise(count= n(),
            avg_accuracy_gender = mean(accuracy_score),
            sd_accuracy_gender = sd(accuracy_score))

vote_summary_tbl <- accuracy_tbl %>%
  group_by(vote_2020) %>%
  summarise(count = n(),
            avg_accuracy_vote = mean(accuracy_score),
            sd_accuracy_vote = sd(accuracy_score))

#### Machine Learning
holdout_indices <- createDataPartition(accuracy_tbl$accuracy_score,
                                       p = .50,
                                       list = T)$Resample1 #using data partition to split data in half to be used to create training and test datasets for machine learning
test_tbl <- accuracy_tbl[holdout_indices,] #creating the test/holdout data by selecting the holdout indices from thd accuracy_tbl
training_tbl <- accuracy_tbl[-holdout_indices,] #creating the training data by selecting not the holdout indices. 

training_folds <- createFolds(training_tbl$accuracy_score) #creating folds for indexing in machine learning

model1 <- train(
  accuracy_score ~ vote_2020*pid*gender*state, #predicting accuracy score by vote choice in 2020, party identification, gender, and state
  training_tbl, #using the training data
  method="lm", #used lm for OLS regression to compare to other models later
  na.action = na.pass, #so model will pass nas and actually run because otherwise it won't
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", #method cv to create cross-validation 
                           number=5,  #choosing ten folds
                           verboseIter=T, 
                           indexOut = training_folds)
)   
model1
cv_m1 <- model1$results$Rsquared
holdout_m1 <- cor(
  predict(model1, test_tbl, na.action = na.pass),
  test_tbl$accuracy_score
)^2

model2 <- train(
  accuracy_score ~ vote_2020*pid*gender*state,
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
cv_m2 <- max(model2$results$Rsquared)
holdout_m2 <- cor(
  predict(model2, test_tbl, na.action = na.pass),
  test_tbl$accuracy_score
)^2

model3 <- train(
  accuracy_score ~ vote_2020*pid*gender*state,
  training_tbl,
  tuneGrid= expand.grid(mtry= c(2,12,18), #changed mtry because my tibble only has 20 variables so I changed from 23 to 18 as the last mtry because without this the model kept failing
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
)^2

model4 <- train(
  accuracy_score ~ vote_2020*pid*gender*state,
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
cv_m4 <- max(model4$results$Rsquared) #this take the largest Rsquared for model 4
holdout_m4 <- cor(
  predict(model4, test_tbl, na.action = na.pass), #running the model with the test data 
  test_tbl$accuracy_score
)^2 #calculating Rsquared from the test data by wrapping it in cor()^2

summary(resamples(list(model1, model2, model3, model4)), metric="Rsquared")
dotplot(resamples(list(model1, model2, model3, model4)), metric="Rsquared")

#Publication

#function 
make_it_pretty <- function (formatme) {
  formatme <- formatC(formatme, format="f", digits=2)
  formatme <- str_remove(formatme, "^0")
  return(formatme)
}

#descriptive tibbles
gender_summary_tbl #viewing the summary data by gender

pid_summary_tbl #viewing the summary data by party id

View(state_summary_tbl)#viewing the summary data by state within the view since there's a lot of rows

vote_summary_tbl #viewing the summary data by 2020 vote choice 

#machine learning tibble
table1_tbl <- tibble(
  algo = c("regression","elastic net","random forests","xgboost"),
  cv_rqs = c(
    make_it_pretty(cv_m1),
    make_it_pretty(cv_m2),
    make_it_pretty(cv_m3),
    make_it_pretty(cv_m4)
  ),
  ho_rqs = c(
    make_it_pretty(holdout_m1),
    make_it_pretty(holdout_m2),
    make_it_pretty(holdout_m3),
    make_it_pretty(holdout_m4)
  )
)

table1_tbl

#Data Export
accuracy_tbl %>%
  saveRDS("../shiny/import.RDS")
