library(Hmisc)
library(psych)
library(leaps)
library(dplyr)
library(caTools)
library(pROC)
library(ROCR)
library(MASS)
library(tibble)
BK_df = read.csv("BankCreditCard.csv")
summary(BK_df)
BK_df = BK_df[,-1]
summary(BK_df)


View(BK_df)
BK_simp = lm(Default_Payment ~ ., data = BK_df)
step(BK_simp, direction = "backward")
summary(BK_simp)
BK_simp_1 = lm(formula = Default_Payment ~ Credit_Amount + Gender + Academic_Qualification + 
     Marital + Age_Years + Repayment_Status_Jan + Repayment_Status_Feb + 
     Repayment_Status_March + Repayment_Status_April + Repayment_Status_May + 
     Repayment_Status_June + Jan_Bill_Amount + Feb_Bill_Amount + 
     Previous_Payment_Jan + Previous_Payment_Feb + Previous_Payment_April + 
     Previous_Payment_May, data = BK_df)
summary(BK_simp_1)

plot(BK_df$Credit_Amount)
hist(BK_df$Credit_Amount)
boxplot(BK_df$Credit_Amount)
scatter.smooth(BK_df$Credit_Amount)
glimpse(BK_df$Credit_Amount)
uv_ca = 3*quantile(BK_df$Credit_Amount, 0.99)
BK_df$Credit_Amount[BK_df$Credit_Amount > uv_ca] = uv_ca
summary(BK_df$Credit_Amount)
hist(BK_df$Credit_Amount)
View(BK_df)
BK_df = BK_df[-c(2198),]
View(BK_df$Credit_Amount)


summary(BK_df$Gender)
unique(BK_df$Gender)


summary(BK_df$Academic_Qualification)
table(BK_df$Academic_Qualification)
cor(BK_df$Academic_Qualification, BK_df$Default_Payment)


summary(BK_df$Marital)
table(BK_df$Marital)
cor(BK_df$Marital, BK_df$Default_Payment)


summary(BK_df$Age_Years)
hist(BK_df$Age_Years)
boxplot(BK_df$Age_Years)
cor(BK_df$Age_Years, BK_df$Default_Payment)

BK_df$Avg_Repayment_Status = (BK_df$Repayment_Status_Jan + BK_df$Repayment_Status_Feb + BK_df$Repayment_Status_March + BK_df$Previous_Payment_April + BK_df$Repayment_Status_May + BK_df$Repayment_Status_June)/6
View(BK_df[,25])
BK_df = BK_df[,-6:-11]
summary(BK_df$Avg_Repayment_Status)

uv_ars <- 3*quantile(BK_df$Avg_Repayment_Status, 0.99)
BK_df$Avg_Repayment_Status[BK_df$Avg_Repayment_Status > uv_ars] <- uv_ars

summary(BK_df$Avg_Repayment_Status)
boxplot(BK_df$Avg_Repayment_Status)
table(BK_df$Avg_Repayment_Status)
hist(BK_df$Avg_Repayment_Status)
cor(BK_df$Avg_Repayment_Status, BK_df$Default_Payment)

BK_df$Avg_Bill_Amount <- (BK_df$Jan_Bill_Amount + BK_df$Feb_Bill_Amount + BK_df$March_Bill_Amount + BK_df$April_Bill_Amount + BK_df$May_Bill_Amount + BK_df$June_Bill_Amount)/6
summary(BK_df$Avg_Bill_Amount)

uv_aba <- 3*quantile(BK_df$Avg_Bill_Amount, 0.99)
BK_df$Avg_Bill_Amount[BK_df$Avg_Bill_Amount > uv_aba] <- uv_aba
summary(BK_df$Avg_Bill_Amount)

lv_aba <- 0.3*quantile(BK_df$Avg_Bill_Amount, 0.01)
BK_df$Avg_Bill_Amount[BK_df$Avg_Bill_Amount < lv_aba] <- lv_aba
summary(BK_df$Avg_Bill_Amount)

BK_df <- BK_df[,-6:-11]
hist(BK_df$Avg_Bill_Amount)
boxplot(BK_df$Avg_Bill_Amount)
cor(BK_df$Avg_Bill_Amount, BK_df$Default_Payment)
str(BK_df)

BK_df$Avg_Previous_Payement <- (BK_df$Previous_Payment_Jan + BK_df$Previous_Payment_Feb + BK_df$Previous_Payment_March + BK_df$Previous_Payment_April + BK_df$Previous_Payment_May + BK_df$Previous_Payment_June)/6
summary(BK_df$Avg_Previous_Payement)

uv_app <- 3*quantile(BK_df$Avg_Previous_Payement, 0.99)
BK_df$Avg_Previous_Payement[BK_df$Avg_Previous_Payement > uv_app] <- uv_app
summary(BK_df$Avg_Previous_Payement)

lv_app <- 0.3*quantile(BK_df$Avg_Previous_Payement, 0.01)
BK_df$Avg_Previous_Payement[BK_df$Avg_Previous_Payement < lv_app] <- lv_app
summary(BK_df$Avg_Previous_Payement)

BK_df <- BK_df[,-6:-11]
hist(BK_df$Avg_Previous_Payement)
boxplot(BK_df$Avg_Previous_Payement)
cor(BK_df$Avg_Previous_Payement, BK_df$Default_Payment)
str(BK_df)

set.seed(123)
split = sample.split(BK_df,SplitRatio = 4/5)
BK_Train= subset(BK_df,split == TRUE)
BK_Test = subset(BK_df, split == FALSE)
summary(BK_Train)
summary(BK_Test)

BK_trial = regsubsets(Default_Payment ~., data = BK_df, nvmax = 25)
summary(BK_trial)
summary(BK_trial)$adjr2
which.max(summary(BK_trial)$adjr2)
coef(BK_trial,8)

Log_Trial = glm(Default_Payment ~ ., family = 'binomial', data = BK_df)
summary(Log_Trial)

Log_1 <- glm(Default_Payment ~ ., family = 'binomial', data = BK_Train)
summary(Log_1)

step(Log_1, direction = "backward")

Log_2 = glm(formula = Default_Payment ~ Credit_Amount + Gender + Academic_Qualification + 
              Marital + Age_Years + Avg_Bill_Amount + Avg_Previous_Payement, 
            family = "binomial", data = BK_Train)
summary(Log_2)

Log_3 = glm(formula = Default_Payment ~ Credit_Amount + Gender + Academic_Qualification + 
              Marital + Avg_Bill_Amount + Avg_Previous_Payement, 
            family = "binomial", data = BK_Train)
summary(Log_3)

Log_4 = glm(formula = Default_Payment ~ Credit_Amount + Avg_Previous_Payement, 
            family = "binomial", data = BK_Train)
summary(Log_4)

#Log_1 and Log_2 seem to be the better model

BK_Pred1 = predict(Log_1, type = 'response', newdata = BK_Test)
BK_Y_Pred1 = ifelse(BK_Pred > 0.5, 1, 0)
View(BK_Pred1)
View(BK_Y_Pred1)
BKcm1 = table(BK_Test[,6], BK_Y_Pred1 > 0.5)
BKcm1
BKac1 = sum(diag(BKcm1))/sum(BKcm1)
BKac1
BKprc1 <- diag(BKcm1)/colSums(BKcm1,2)
BKprc1
BKrc1 <- diag(BKcm1)/rowSums(BKcm1,2)
BKrc1
BKf1_score1 <- 2*BKprc1*BKrc1/(BKprc1+BKrc1)
BKf1_score1

BK_Pred2 = predict(Log_2, type = 'response', newdata = BK_Test)
BK_Y_Pred2 = ifelse(BK_Pred > 0.5, 1, 0)
View(BK_Pred2)
View(BK_Y_Pred2)
BKcm2 = table(BK_Test[,6], BK_Y_Pred2 > 0.5)
BKcm2
BKac2 = sum(diag(BKcm2))/sum(BKcm2)
BKac2
BKprc2 <- diag(BKcm2)/colSums(BKcm2,2)
BKprc2
BKrc2 <- diag(BKcm2)/rowSums(BKcm2,2)
BKrc2
BKf1_score2 <- 2*BKprc2*BKrc2/(BKprc2+BKrc2)
BKf1_score2

#Since there is no difference between the F1 score of models, Log 1 and Log 2,
#and since Log 2 has the lowest AIC value, we will stick with Log 2

auc(BK_Test$Default_Payment, BK_Pred1)
BKroc1 <- roc(BK_Test$Default_Payment, BK_Pred1 , quiet = FALSE)
BKroc1

auc(BK_Test$Default_Payment, BK_Pred2)
BKroc2 <- roc(BK_Test$Default_Payment, BK_Pred2 , quiet = FALSE)
BKroc2

BKROCRpred1 <- prediction(BK_Pred1,BK_Test$Default_Payment)
BKROCRperf1 <- performance(BKROCRpred1, 'tpr','fpr')
BKROCRperf1

BKROCRpred2 <- prediction(BK_Pred2,BK_Test$Default_Payment)
BKROCRperf2 <- performance(BKROCRpred2, 'tpr','fpr')
BKROCRperf2

plot(BKROCRperf1, colorize = TRUE, text.adj = c(-0.2,1.7))
plot(BKROCRperf2, colorize = TRUE, text.adj = c(-0.2,1.7))

BKlda.m1 <- lda(Default_Payment ~ Credit_Amount + Gender + Academic_Qualification + 
                  Marital + Age_Years + Avg_Bill_Amount + Avg_Previous_Payement, data = BK_Train)
BKlda.m1

BKlda.m2 <- lda(Default_Payment ~ Credit_Amount, data = BK_Train)
BKlda.m2

lda.pred1 = predict(BKlda.m1, BK_Train)
lda.pred1
  
BK.test.lda1 <- predict(BKlda.m1, newdata = BK_Test)
BK.test.lda1
BK.lda.cm1<- table(BK_Test$Default_Payment, BK.test.lda1$class)
BK.lda.cm1

lda.pred2 = predict(BKlda.m2, BK_Train)
lda.pred12

BK.test.lda2 <- predict(BKlda.m2, newdata = BK_Test)
BK.test.lda2
BK.lda.cm2<- table(BK_Test$Default_Payment, BK.test.lda2$class)
BK.lda.cm2