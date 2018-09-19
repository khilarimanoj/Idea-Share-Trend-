# ** OBJECTIVE ** 
#Import the csv file with past 3 months history for idea share trend
scrip.data <- read.csv("D:/Document/r studio file/idea_90.csv")

# created new column for High - Low
scrip.data$Movement <- scrip.data$Close.Price - scrip.data$Open.Price
scrip.data$Up_Down <- ifelse(scrip.data$Movement > 0,1,0)

#*********Model 1*****************

train_subset<- scrip.data[c(4:7,17)]

model1 <- glm(train_subset$Up_Down ~ ., family = binomial(link = 'logit'), data = train_subset[1:4])

summary(model1)

anova(model1, test = 'Chisq')

log_predict1 <- predict(model1,newdata = train_subset[1:4], type = "response")
mean(log_predict1)
plot(log_predict1)

log_predict1 <- ifelse(log_predict1 > 0.5,1,0)
log_predict1
scrip.data$Up_Down_Pred1_.5 <- log_predict1

#*********Model 2*****************
train_subset<- scrip.data[c(4,6,7,17)]

model2 <- glm(train_subset$Up_Down ~ ., family = binomial(link = 'logit'), data = train_subset[1:3])

summary(model2)

anova(model2, test = 'Chisq')

log_predict2 <- predict(model2,newdata = train_subset[1:3], type = "response")
log_predict2 <- ifelse(log_predict2 > 0.5,1,0)
log_predict2

scrip.data$Up_Down_Pred2_.5 <- log_predict2

#*********Model 3*****************
train_subset<- scrip.data[c(4,6,17)]

model3 <- glm(train_subset$Up_Down ~ ., family = binomial(link = 'logit'), data = train_subset[1:2])

summary(model3)

anova(model3, test = 'Chisq')

log_predict3 <- predict(model3,newdata = train_subset[1:2], type = "response")
log_predict3 <- ifelse(log_predict3 > 0.5,1,0)
log_predict3
scrip.data$Up_Down_Pred3_.5 <- log_predict3

#COMPARE ALL 3 MODELS
anova(model1, model2, model3, test = 'Chisq')

#******************************************************
conf <-table(train_subset$Up_Down , log_predict3, dnn = c("Actual", "Predicted"))
conf


# TP, FN, FP and TN using conf
TN <- conf[1, 1] # 
TP <- conf[2,2] #
FN <- conf[2,1] # 
FP <- conf[1, 2] #  

# Calculate and print the accuracy: acc
acc = (TP+TN)/(TP+FN+FP+TN)
print(acc)

# Calculate and print out the precision: prec 
prec = (TP)/(TP+FP)
print(prec)

# Calculate and print out the recall: rec (sensitivity)
rec = (TP)/(TP+FN)
print(rec)

#install.packages("ROCR")
library(ROCR)

#Ploting ROC 
train_subset<- scrip.data[c(4,6,7,17)]
p <- predict(model2,newdata=train_subset, type = "response")
pr <- prediction(p,train_subset$Up_Down)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
# Calculate and print out the AUC(Area under Roc Curve)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

