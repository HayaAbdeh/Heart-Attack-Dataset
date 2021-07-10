
#knitr::opts_chunk$set(echo = TRUE)

# Install all needed libraries if it is not present

if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(data.table)) install.packages("data.table")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(plotly)) install.packages("plotly")
if(!require(gbm)) install.packages("gbm")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(xgboost)) install.packages("xgboost")
if(!require(e1071)) install.packages("e1071")
if(!require(class)) install.packages("class")
if(!require(ROCR)) install.packages("ROCR")
if(!require(randomForest)) install.packages("randomForest")
if(!require(PRROC)) install.packages("PRROC")
if(!require(reshape2)) install.packages("reshape2")
if(!require(lubridate))install.packages("lubridate")
if(!require(knitr)) install.packages("knitr")
if(!require(recosystem)) install.packages("recosystem")
if(!require(tinytex)) install.packages("tinytex")
if(!require(webshot))install.packages("webshot")
if(!require(Hmisc))install.packages("Hmisc")
if(!require(GGally))install.packages("GGally")
if(!require(rpart.plot))install.packages("rpart.plot")


library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(ggplot2)
library(plotly)
library(gbm)
library(caret)
library(xgboost)
library(e1071)
library(class)
library(lightgbm)
library(ROCR)
library(randomForest)
library(PRROC)
library(reshape2)
library(data.table)
library(lubridate)
library(knitr)
library(recosystem)
library(tinytex)
library(webshot)
library(Hmisc)
library(GGally )
library(rpart)
library(rpart.plot)



heart_df <- read.csv('/Users/hammar/Documents/RGitProjects/Heart_attack_proj/heart.csv')
heart_df %>% head()


class(heart_df)

str(heart_df)

#defining meanningful names

names <- c("Age",
           "Sex",
           "Chest_Pain_Type",
           "Resting_Blood_Pressure",
           "Cholesterol_serum",
           "Fasting_Blood_Sugar",
           "Resting_ECG",
           "Maximum_Heart_Rate",
           "Exercise_Induced_Angina",
           "ST_Depression_Exercise",
           "Peak_Exercise_ST_Segment",
           "Num_Major_Vessels_Flourosopy",
           "Thalassemia",
           "Diagnosis_Heart_Disease")

#Lets keep the old names in another data frame
heart_df_oldnames <- heart_df

#now rename the columns of the dataframe

colnames(heart_df) <- names

#show the new names
names(heart_df)

#LEt us show the Data frame summary
summary(heart_df)


heart_df %>% 
  summarise(age_ranges = n_distinct(Age), 
            sex_types = n_distinct(Sex),
            cp_types = n_distinct(Chest_Pain_Type), 
            nom_trestbps = n_distinct(Resting_Blood_Pressure),
            nom_chol = n_distinct(Cholesterol_serum), 
            nom_fbs = n_distinct(Fasting_Blood_Sugar),
            types_restecg = n_distinct(Resting_ECG), 
            nom_thalach = n_distinct(Maximum_Heart_Rate),
            nom_exang = n_distinct(Exercise_Induced_Angina), 
            nom_oldpeak = n_distinct(ST_Depression_Exercise),
            types_slope = n_distinct(Peak_Exercise_ST_Segment),
            nom_caa = n_distinct(Num_Major_Vessels_Flourosopy),
            types_thal = n_distinct(Thalassemia),                                         Diagnosis_types = n_distinct(Diagnosis_Heart_Disease))


# Histogram for all  Categorical Variables in The Heart Dataset each column individually.


heart_df_cat1 <- heart_df %>% 
  select(Sex , Chest_Pain_Type, Fasting_Blood_Sugar , 
         Resting_ECG, Exercise_Induced_Angina , 
         Peak_Exercise_ST_Segment , Thalassemia ,
         Diagnosis_Heart_Disease)%>% 
  mutate(Sex = recode_factor(Sex, `0` = "female", `1` = "male" ),
         Chest_Pain_Type = recode_factor(Chest_Pain_Type,`0` = "typical",   
                                         `1` = "atypical",
                                         `2` = "non-angina", 
                                         `3`="asymptomatic"),
         Fasting_Blood_Sugar = 
           recode_factor(Fasting_Blood_Sugar,`0`="<= 120 mg/dl",
                         `1`="> 120 mg/dl"),
         Resting_ECG = recode_factor(Resting_ECG, `0` = "normal",
                                     `1` = "ST-T abnormality",
                                     `2` = "LV hypertrophy"),
         Exercise_Induced_Angina = recode_factor(Exercise_Induced_Angina,`0`="no",`1` = "yes"),
         Peak_Exercise_ST_Segment = recode_factor(Peak_Exercise_ST_Segment,`1`= "up-sloaping", 
                                                  `2` = "flat",
                                                  `3` = "down-sloaping"),
         Thalassemia = recode_factor(Thalassemia,`0` = "normal",
                                     `1` = "silent carrier",
                                     `2` = "fixed defect",
                                     `3` = "reversible defect"),
         
         Diagnosis_Heart_Disease = recode_factor(Diagnosis_Heart_Disease, 
                                                 `0` = "No Disease",`1` = "Have Disease") )






par(mar=c(1, 1, 1, 1))        
hist.data.frame(heart_df_cat1)



# Histogram for all numeric Variables in The Heart Dataset each column individually.
heart_df_num <- heart_df %>% select(Age,Resting_Blood_Pressure,Cholesterol_serum, 
                                    Maximum_Heart_Rate, ST_Depression_Exercise, Num_Major_Vessels_Flourosopy)

hist.data.frame(heart_df_num)




#visualize gender
heart_df %>% 
  drop_na() %>%
  group_by(Sex) %>%
  count() %>% 
  ungroup()


heart_df_gender <- heart_df %>% 
  select(Sex)%>% 
  mutate(Sex = recode_factor(Sex, `0` = "female", `1` = "male" )
  )

ggplot(heart_df_gender) +
  geom_bar(aes(x = Sex ,fill=Sex ) )

#Now let us calculate how many individuals that could be Diagnosed to suffer from heart disease 
heart_df %>% 
  drop_na() %>%
  group_by(Diagnosis_Heart_Disease) %>%
  count() %>% 
  ungroup() 

heart_df_diseased <- heart_df %>% 
  select(Diagnosis_Heart_Disease)%>% 
  mutate(Diagnosis_Heart_Disease = recode_factor(Diagnosis_Heart_Disease, `0` = "No Disease", `1` = "Have Disease" )
  )

ggplot(heart_df_diseased) +
  geom_bar(aes(x = Diagnosis_Heart_Disease ,fill=Diagnosis_Heart_Disease ) )


#Now let us show Diagnosis Heart Disease by gender for all types of chest pain

heart_df %>% filter(Diagnosis_Heart_Disease == 1) %>% group_by(Sex, Chest_Pain_Type) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(Sex, count,   fill = as.factor(Chest_Pain_Type)),stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Sex") + labs(fill = "Diagnosis_Heart_Disease") + 
  ggtitle("Sex vs. Count_of_diseased_individuals for various chest pain conditions") +
  scale_fill_manual(values=c("red", "blue", "darkgreen", "orange"))

#Show levels of Thalassemia

heart_df %>% 
  drop_na() %>%
  group_by(Thalassemia) %>%
  count() %>% 
  ungroup() 
heart_df_Thalassemia <- heart_df %>% 
  select(Thalassemia)%>% 
  mutate(Thalassemia =recode_factor(Thalassemia,`0` = "normal",
                                    `1` = "silent carrier",
                                    `2` = "fixed defect",
                                    `3` = "reversible defect"))

ggplot(heart_df_Thalassemia) +
  geom_bar(aes(x = Thalassemia ,fill=Thalassemia ) )

#show a diagram explains Chest pain type for diseased people with Age classifications

heart_df %>% filter(Diagnosis_Heart_Disease == 1) %>% group_by(Age, Chest_Pain_Type) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(Age, count,   fill = as.factor(Chest_Pain_Type)),stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Age") + labs(fill = "Diagnosis_Heart_Disease") + 
  ggtitle("Age vs. Count_of_diseased_individuals) for various chest pain conditions") +
  scale_fill_manual(values=c("red", "blue", "darkgreen", "orange"))


# to use the short names of the data frame, we use the data frame that we initiate previously with shortcut names.

#Correlation matrix using Pearson method, default method is Pearson
heart_df_oldnames %>% ggcorr(high       = "darkred",
                             low        = "steelblue",
                             label      = TRUE, 
                             hjust      = .75, 
                             size       = 3, 
                             label_size = 3,
                             nbreaks    = 5
) +
  labs(title = "Correlation Matrix",
       subtitle = "Pearson Method Use Pairwise Obervations")



#Correlation matrix using Kendall method
heart_df_oldnames %>% ggcorr(method     = c("pairwise", "kendall"),
                             high       = "darkred",
                             low        = "steelblue",
                             label      = TRUE, 
                             hjust      = .75, 
                             size       = 3, 
                             label_size = 3,
                             nbreaks    = 5
) +
  labs(title = "Correlation Matrix",
       subtitle = "Kendall Method Use Pairwise Observations")


#Machine Learning Basics

#create Data Partition
#set seed for reproducible results
set.seed(1)

test_index <- createDataPartition(y = heart_df$Diagnosis_Heart_Disease, times = 1, p = 0.1, list = FALSE)
train_heart_df <- heart_df[-test_index, ]
test_heart_df <- heart_df[test_index, ]

#dimension of the training data set
dim(train_heart_df)

#dimension of the test data set
dim(test_heart_df)


#Logistic Regression model
set.seed(1)

log_regr_hd_model = glm(Diagnosis_Heart_Disease~., data=train_heart_df, family='binomial')

summary(log_regr_hd_model)   


#correlations
cor(train_heart_df)
abs(cor(train_heart_df))>0.7

m_cor = melt(abs(cor(train_heart_df))>0.7)
head(m_cor)

#heatmap

ggplot(m_cor, aes(x = Var1, y=Var2, fill=as.numeric(value))) + geom_tile() +
  geom_text(aes(Var1, Var2, label=as.numeric(value)),color='black',size=2)+
  scale_color_gradient(low='blue',high='red') +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

log_regr_hd_model2 = glm(Diagnosis_Heart_Disease~ 
                           Age+ Sex+
                           Chest_Pain_Type+
                           Resting_Blood_Pressure +                         
                           Cholesterol_serum +                             
                           Fasting_Blood_Sugar +                            
                           Resting_ECG  +                                 
                           Maximum_Heart_Rate  +                          
                           Exercise_Induced_Angina  +                      
                           ST_Depression_Exercise  +                       
                           Peak_Exercise_ST_Segment +                      
                           Num_Major_Vessels_Flourosopy+                   
                           Thalassemia  ,
                         data=train_heart_df, family='binomial')

summary(log_regr_hd_model2)   

log_regr_hd_model2 = glm(Diagnosis_Heart_Disease~ 
                           Age+ Sex+
                           Chest_Pain_Type+
                           Resting_Blood_Pressure +                         
                           Cholesterol_serum +                
                           Resting_ECG  +                                 
                           Maximum_Heart_Rate  +                          
                           Exercise_Induced_Angina  +                      
                           ST_Depression_Exercise  +                       
                           Peak_Exercise_ST_Segment +                      
                           Num_Major_Vessels_Flourosopy+                   
                           Thalassemia  ,
                         data=train_heart_df, family='binomial')

summary(log_regr_hd_model2) 


log_regr_hd_model2 = glm(Diagnosis_Heart_Disease~ 
                           Age+ Sex+
                           Chest_Pain_Type+
                           Resting_Blood_Pressure +                         
                           Cholesterol_serum +                           
                           Resting_ECG  +                      
                           Exercise_Induced_Angina  +                      
                           ST_Depression_Exercise  +                       
                           Peak_Exercise_ST_Segment +                      
                           Num_Major_Vessels_Flourosopy+                   
                           Thalassemia  ,
                         data=train_heart_df, family='binomial')

summary(log_regr_hd_model2) 



log_regr_hd_model2 = glm(Diagnosis_Heart_Disease~ 
                           Age+ Sex+
                           Chest_Pain_Type+
                           Resting_Blood_Pressure +  
                           Resting_ECG  +              
                           Exercise_Induced_Angina  +                      
                           ST_Depression_Exercise  +                       
                           Peak_Exercise_ST_Segment +                      
                           Num_Major_Vessels_Flourosopy+                   
                           Thalassemia  ,
                         data=train_heart_df, family='binomial')

summary(log_regr_hd_model2)   



log_regr_hd_model2 = glm(Diagnosis_Heart_Disease~ 
                           Sex+
                           Chest_Pain_Type+
                           Resting_Blood_Pressure +    
                           Resting_ECG  +                
                           Exercise_Induced_Angina  +                      
                           ST_Depression_Exercise  +                       
                           Peak_Exercise_ST_Segment +                      
                           Num_Major_Vessels_Flourosopy+                   
                           Thalassemia  ,
                         data=train_heart_df, family='binomial')

summary(log_regr_hd_model2)   



#Predictions on The Training Set

predictTrain_set = predict(log_regr_hd_model2, type='response')


#Confusion matrix using threshold of 0.5
table(train_heart_df$Diagnosis_Heart_Disease, predictTrain_set>0.5)


#Accuracy on The training set
accuracy_LR_train <- (97+137)/nrow(train_heart_df)

accuracy_LR_train

#Predictions on Test set
predictTest_set = predict(log_regr_hd_model2, newdata=test_heart_df , type='response')


#Confusion matrix using threshold of 0.5
table(test_heart_df$Diagnosis_Heart_Disease, predictTest_set>0.5)

#Accuracy on The test set
accuracy_LR_test <- (8+15)/nrow(test_heart_df)

accuracy_LR_test

#plot ROCR curve
ROCRpred = prediction(predictTest_set, test_heart_df$Diagnosis_Heart_Disease)

#The area under curve
area = as.numeric(performance(ROCRpred, 'auc')@y.values)
area

ROCRperf = performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize=TRUE, main='ROCR Curve')

# We make a data frame to save the results of accuracy
results_table <- data.frame(Methods="Logistic Regression Model", Accuracy_of_Train_Sets=accuracy_LR_train ,Accuracy_of_Test_Sets = accuracy_LR_test )

#results_table




#Regression Trees
#initiate our tree

tree = rpart(Diagnosis_Heart_Disease ~ ., data=train_heart_df, method='class')


# show tree graph
prp(tree)


#predict_tree for train set
predict_tree_train = predict(tree, newdata=train_heart_df, type='class')
#predict_tree for test set
predict_tree = predict(tree, newdata=test_heart_df, type='class')

#confusion matrix for Trian set 
table(train_heart_df$Diagnosis_Heart_Disease, predict_tree_train)


#confusion matrix for Test set 
table(test_heart_df$Diagnosis_Heart_Disease, predict_tree)

#Accuracy for train set
accuracy_train_tree <- (102+133)/nrow(train_heart_df)
#Accuracy for test set
accuracy_test_tree <- (7+13)/nrow(test_heart_df)


accuracy_train_tree 
accuracy_test_tree

results_table <- results_table %>% add_row(Methods="Regression Tree Model", Accuracy_of_Train_Sets=accuracy_train_tree ,Accuracy_of_Test_Sets = accuracy_test_tree)

#results_table

predict_tree = predict(tree, newdata=test_heart_df)


ROCR_tree_test = prediction(predict_tree[,2],test_heart_df$Diagnosis_Heart_Disease)



ROCRperf = performance(ROCR_tree_test, 'tpr','fpr')
plot(ROCRperf,colorize=TRUE)

as.numeric(performance(ROCR_tree_test, 'auc')@y.values)

# Converting the dependent variables to factors
train_heart_df$Diagnosis_Heart_Disease <- as.factor(train_heart_df$Diagnosis_Heart_Disease)
test_heart_df$Diagnosis_Heart_Disease <- as.factor(test_heart_df$Diagnosis_Heart_Disease)


#QDA
qda_fit <- train( Diagnosis_Heart_Disease ~ ., method = "qda", data = train_heart_df)

qda_predict <- predict(qda_fit, test_heart_df)
confusionMatrix(qda_predict, test_heart_df$Diagnosis_Heart_Disease)


#Accuracy from the previous result
results_table <- results_table %>% add_row(Methods="QDA", Accuracy_of_Train_Sets= 0.8065 ,Accuracy_of_Test_Sets = 0.8065)

#results_table


#LDA
lda_fit <- train(Diagnosis_Heart_Disease ~ ., method = "lda", data = train_heart_df)

lda_predict <- predict(lda_fit, test_heart_df)
confusionMatrix(lda_predict, test_heart_df$Diagnosis_Heart_Disease)


#Accuracy from the previous result
results_table <- results_table %>% add_row(Methods="LDA", Accuracy_of_Train_Sets= 0.9032 ,Accuracy_of_Test_Sets = 0.9032)

#results_table



#KNN
ctrl <- trainControl(method = "cv", verboseIter = FALSE, number = 5)
knn_fit <- train(Diagnosis_Heart_Disease ~ ., 
                 data = train_heart_df, method = "knn", preProcess = c("center","scale"),
                 trControl = ctrl , tuneGrid = expand.grid(k = seq(1, 20, 2)))

plot(knn_fit)


knn_predict <- predict(knn_fit,newdata = test_heart_df )
knn_results <- confusionMatrix(knn_predict, test_heart_df$Diagnosis_Heart_Disease )

knn_results


#Accuracy from the previous result
results_table <- results_table %>% add_row(Methods="KNN", Accuracy_of_Train_Sets= 0.8387  ,Accuracy_of_Test_Sets = 0.8387 )

#results_table

#SVM

ctrl <- trainControl(method = "cv", verboseIter = FALSE, number = 5)

grid_svm <- expand.grid(C = c(0.01, 0.1, 1, 10, 20))

svm_fit <- train(Diagnosis_Heart_Disease ~ ., data = train_heart_df, method = "svmLinear", preProcess = c("center","scale"), tuneGrid = grid_svm, trControl = ctrl)

plot(svm_fit)

svm_predict <- predict(svm_fit, newdata = test_heart_df)
svm_results <- confusionMatrix(svm_predict, test_heart_df$Diagnosis_Heart_Disease)

svm_results


#Accuracy from the previous result
results_table <- results_table %>% add_row(Methods="SVM", Accuracy_of_Train_Sets= 0.8387  ,Accuracy_of_Test_Sets = 0.8387 )

#results_table

#RF

control<- trainControl(method = "cv", number = 5, verboseIter = FALSE)
grid <-data.frame(mtry = seq(1, 10, 2))
rf_fit <- train(Diagnosis_Heart_Disease ~ ., method = "rf", data = train_heart_df, ntree = 20, trControl = control,
                tuneGrid = grid)

plot(rf_fit)
rf_predict <- predict(rf_fit, newdata = test_heart_df)

rf_results <- confusionMatrix(rf_predict, test_heart_df$Diagnosis_Heart_Disease)

rf_results



#Accuracy from the previous result
results_table <- results_table %>% add_row(Methods="RF", Accuracy_of_Train_Sets= 0.8387  ,Accuracy_of_Test_Sets = 0.8387 )

#results_table

#GBM

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 10, 25, 30),n.trees = c(5, 10, 25, 50), shrinkage = c(0.1, 0.2, 0.3,  0.4, 0.5), n.minobsinnode = 20)

gbm_fit <- train(Diagnosis_Heart_Disease ~ ., method = "gbm", data = train_heart_df,  trControl = control, verbose = FALSE,tuneGrid = gbmGrid)

plot(gbm_fit)

gbm_predict <- predict(gbm_fit, newdata = test_heart_df)

gbm_results <- confusionMatrix(gbm_predict, test_heart_df$Diagnosis_Heart_Disease)

gbm_results

#Accuracy from the previous result
results_table <- results_table %>% add_row(Methods="GBM", Accuracy_of_Train_Sets= 0.8065  ,Accuracy_of_Test_Sets = 0.8065 )

#results_table


#Print the final results table
results_table
