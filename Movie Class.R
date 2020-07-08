df = read.csv("Movie_classification.csv")
summary(df)

#Data Preprocessing 
summary(df) 
df$Time_taken[is.na(df$Time_taken)] <- mean(df$Time_taken,na.rm = TRUE)

library(caTools) 
set.seed(0) 
split =sample.split(df,SplitRatio = 0.8) 
trainc = subset(df,split == TRUE) 
testc = subset(df,split == FALSE)

#install required packages 
#install.packages('rpart') 
#install.packages('rpart.plot') 
library(rpart) 
library(rpart.plot)

