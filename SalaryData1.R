dataset_x=read.csv("Salary_Data.csv")
dataset_x
library(caTools)
set.seed(123)
split = sample.split(dataset_x$Salary, SplitRatio = 2/3)
training_set= subset(dataset_x, split == TRUE)
test_set=subset(dataset_x,split == FALSE)
test_set
regressor = lm(formula = Salary ~ YearsExperience, data = training_set)
y_pred = predict(regressor,newdata = test_set)
print(y_pred)
y_pred
print(test_set)
summary(y_pred)
summary(regressor)
tang=21871.0+(9963.1*1.1)
print(tang)
