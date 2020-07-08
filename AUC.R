####AUC 
### model 1 AUC 
prediction(test.predicted.lda$posterior[,2], test_set$default) %>% 
  performance(measure = "auc") %>% 
  .@y.values

#install.packages("ISLR"); 

# Load the ISLR package 
library(ISLR) 


default <- (as.data.frame(ISLR::Default)) 

summary(default) 
hist(default$balance) 

pairs(~balance+income, data = default) 
barplot(table(default$default)) 

#prepare Data 
# Splitting the dataset into the Training set and Test set 
# install.packages('caTools') 
library(caTools) 
set.seed(123) 
split = sample.split(default$default, SplitRatio = 4/5) 
training_set = subset(default, split == TRUE) 
test_set = subset(default, split == FALSE) 

#TRain Model 
#Packages.install("MASS") 
library("MASS") 

lda.m1 <- lda(default ~ balance + student, data = training_set) 
lda.m1 

#Prediction 
library(tibble) 
df <- tibble(balance = rep(c(1000, 2000), 2), 
             student = c("No", "No", "Yes", "Yes")) 
df 
df.pred <- predict(lda.m1, df) 
df.pred 


# number of non-defaulters 
sum(df.pred$posterior[, 1] >= .5) 
## [1] 3 

# number of defaulters 
sum(df.pred$posterior[, 2] > .5) 
## [1] 1 

# number of high-risk customers with 40% probability of defaulting 
sum(df.pred$poster
    
    ############### 
    #ROCR Curve 
    library(ROCR) 
    ROCRpred <- prediction(test.predicted.m3,test_set$default) 
    ROCRperf <- performance(ROCRpred, 'tpr','fpr') 
    ROCRperf 
    
    plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7)) 
    
    
    
    ##AUC 
    # model 1 AUC 
    library(ROCR) 
    library(pROC) 
    # Syntax (response, predictor): 
    #roc_obj_1 <- roc(test_set$default, test.predicted.m1 ) 
    #roc(roc_obj_1) 
    auc(test_set$default, test.predicted.m1 ) 
    roc_obj_1_1 <- roc(test_set$default, test.predicted.m1 , quiet = FALSE) 
    
    roc_obj_1_1 
    
    roc_obj_1_1 <- roc(test_set$default, test.predicted.m1 , quiet = TRUE) 
    roc_obj_1_1 
    roc_obj_2 <- roc(test_set$default, test.predicted.m2 ) 
    auc(roc_obj_2) 
    roc_obj_3 <- roc(test_set$default, test.predicted.m3 ) 
    auc(roc_obj_3)