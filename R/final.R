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
  mutate(across(BBC3_1:BBC3_10, as.numeric)) %>% #coverting specific variables to numeric for later analysis
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
         vote_2020 != c("-1","3","4")) %>% #filtering out people who skipped, did not vote, or voted for someone besides Trump or Biden becuase they are not necessary for my hypotheses
  select(-PPGENDER, -PARTYID4, -PPETHM, -URB_SUB_, -PPSTATEN, -BBC10)  #deleting the unfactored versions of these variables, i used my names because those are easier for me
 
 
#mis_questions <- data_numeric_tbl %>%
 # as_tibble() %>% #converting to a tibble 
 # select(CASEID, BBC3_1:BBC3_10) %>% #choosing the columns I'll need later for data analysis
 # mutate(across(everything(), as.numeric)) 

true_correct <- c("BBC3_2", "BBC3_3", "BBC3_5", "BBC3_6") #creating a vector of accuracy questions with true as the correct answer

false_correct <- c("BBC3_1", "BBC3_4", "BBC3_7", "BBC3_8", "BBC3_9", "BBC3_10") #creating a vector of accuracy questions with false as the correct answer. 

accuracy_tbl <- data_clean_tbl %>% 
  mutate(across(all_of(true_correct), ~ if_else(. == 1, 1, 0))) %>% #using the vector of true correct to recode the correct answers as 1 and everything else as 0 using ifelse to select 1 and keep as 1 and make everything else 0. 
  mutate(across(all_of(false_correct), ~ if_else(. == 2, 1, 0))) %>% #same procedure as before but converting 2(the correct answers) to 1 for the vector of false correct answers and everything else as zero
  mutate(accuracy_count = rowSums(select(.,BBC3_1: BBC3_10))) %>% #using rowsums to count the number of ones for each questions and sum them so I have a count of how many questions each participant got correct.  
  mutate(accuracy_score = accuracy_count/10) #creating accuracy scores by the amount they got right out the number of questions


#Visualization
ggplot(accuracy_tbl, 
      aes(x= pid, y= accuracy_score, fill= gender)) + 
      geom_boxplot() +
      labs(title= "Accuracy Score by Party Identification", xlab= "Party Identification", ylab= "Accuracy Score Percentage")

#Analysis
state_means <- accuracy_tbl %>%
  group_by(state) %>%
  summarise(avg_accuracy = mean(accuracy_score),
            count= n()) 

holdout_indices <- createDataPartition(accuracy_tbl$accuracy_score,
                                       p = .50,
                                       list = T)$Resample1
test_tbl <- accuracy_tbl[holdout_indices,]
training_tbl <- accuracy_tbl[-holdout_indices,]

training_folds <- createFolds(training_tbl$accuracy_score)

model1 <- train(
  accuracy_score ~ vote_2020 +pid +gender,
  training_tbl,
  method="lm",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
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
  accuracy_score ~ vote_2020 +pid +gender,
  training_tbl,
  method="glmnet",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
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
  accuracy_score ~ vote_2020 +pid +gender,
  training_tbl,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
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
  accuracy_score ~ vote_2020 +pid +gender,
  training_tbl,
  method="xgbLinear",
  na.action = na.pass,
  tuneLength = 1,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
model4
cv_m4 <- max(model4$results$Rsquared)
holdout_m4 <- cor(
  predict(model4, test_tbl, na.action = na.pass),
  test_tbl$accuracy_score
)^2

summary(resamples(list(model1, model2, model3, model4)), metric="Rsquared")
dotplot(resamples(list(model1, model2, model3, model4)), metric="Rsquared")

#Publication
make_it_pretty <- function (formatme) {
  formatme <- formatC(formatme, format="f", digits=2)
  formatme <- str_remove(formatme, "^0")
  return(formatme)
}

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