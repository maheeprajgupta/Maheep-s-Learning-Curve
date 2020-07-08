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
Packages.install("MASS") 
library("MASS")

lda.m1 <- lda(default ~ balance + student, data = training_set)
lda.m1
lda.m1$counts


df <- tibble(balance = rep(c(1000, 2000), 2), 
             student = c("No", "No", "Yes", "Yes")) 
df
df.pred <- predict(lda.m1, df) 
df.pred


test.predicted.lda <- predict(lda.m1, newdata = test_set) 
test.predicted.lda
lda.cm <- table(test_set$default, test.predicted.lda$class)
lda.cm

install.packages("tidyverse")
install.packages("broom")
install.packages("knitr")
install.packages("ggthemes")
set.seed(1)
library(dplyr)
library(ISLR)
library(tidyverse) 
library(broom) 
library(knitr)
library(ggthemes)
theme_set(theme_tufte(base_size = 14) + theme(legend.position = 'top'))
data('Default')

log_reg <- glm(default ~ income + balance, data = Default, family = 'binomial')


tidy(log_reg) %>% kable(digits = 5)

pred <- predict(log_reg, type = 'response')
pred_values <- ifelse(pred > 0.5, 'Yes', 'No')
accuracy_full <- mean(pred_values == Default$default)
baseline_full <- mean(Default$default == 'No')
accuracy_full
baseline_full

train_size <- nrow(Default) * 0.75
inTrain <- sample(1:nrow(Default), size = train_size)

train <- Default[inTrain,]
val <- Default[-inTrain,]

train
val

train_model1 <- glm(default ~ income + balance, data = train, family = 'binomial')
tidy(train_model1) %>%
  kable(digits = 5)

pred <- predict(train_model1, val, type = 'response')
pred_values <- ifelse(pred > 0.5, 'Yes', 'No')

accuracy <- mean(pred_values == val$default)
baseline <- mean(val$default == 'No')

acc <- list()
for (i in 1:6) {
  train_size <- nrow(Default) * (10-i)/10
  inTrain <- sample(1:nrow(Default), size = train_size)
  train <- Default[inTrain,]
  val <- Default[-inTrain,]
  
  train_model <- glm(default ~ income + balance, data = train, family = 'binomial')
  pred <- predict(train_model, val, type = 'response')
  pred_values <- ifelse(pred > 0.5, 'Yes', 'No')
  
  acc[[paste0(train_size/100, '%')]] <- mean(pred_values == val$default)
}
acc <- unlist(acc)

data_frame(acc = acc,
           trainsize = names(acc)) %>%
  ggplot(aes(trainsize, acc)) +
  geom_col() +
  coord_cartesian(ylim = c(min(acc, baseline), max(acc))) +
  geom_hline(yintercept = baseline, lty = 2) +
  labs(x = 'Percent of data in training set',
       y = 'Accuracy',
       title = 'Accuracies for different training set sizes',
       subtitle = 'Overlaid on horizontal baseline')

