dataset_Adv = read.csv("50_startups.csv")
dataset_Adv
library(caTools)
set.seed(123)
split_adv = sample.split(dataset_Adv$sales, SplitRatio = 4/5)
training_set_adv= subset(dataset_Adv, split_adv == TRUE)
test_set=subset(dataset_Adv,split_adv == FALSE)
test_set
regressor = lm(formula = sales ~ newspaper, data = training_set_adv)
y_pred = predict(regressor,newdata = test_set)
summary(regressor)
summary(y_pred)
y_pred
MSE_Test = mean((test_set$sales - y_pred)^2)
MSE_Training = mean((training_set_adv$sales - y_pred)^2)
MSE_Test
MSE_Training
