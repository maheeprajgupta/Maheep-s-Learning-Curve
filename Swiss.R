library(dplyr) 
# Load the data 
data("swiss") 
# Inspect the data 
sample_n(swiss, 3) 
library(caret) 
# Split the data into training and test set 
set.seed(123) 
library(caTools) 
#######Leave one out cross validation - LOOCV 
# Define training control 
train.control <- trainControl(method = "LOOCV") 
# Train the model 
model <- train(Fertility ~., data = swiss, method = "lm", 
               trControl = train.control) 
# Summarize the results 
print(model) 

############################ 
##K-fold cross-validation 
# Define training control 
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10) 
# Train the model 
model <- train(Fertility ~., data = swiss, method = "lm", 
               trControl = train.control) 
# Summarize the results 
print(model) 

#############Repeated K-fold cross-validation 
# Define training control 

                 
set.seed(123) 
train.control <- trainControl(method = "repeatedcv", 
                                             number = 10, repeats = 3) 
# Train the model 
model <- train(Fertility ~., data = swiss, method = "lm",trControl = train.control) 
# Summarize the results 
print(model)