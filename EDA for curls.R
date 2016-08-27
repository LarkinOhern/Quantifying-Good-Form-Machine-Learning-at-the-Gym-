###get the data-training only since we are exploring

library(tibble)
library(dplyr)
library(caret)

train<-read.csv("trainingdata.csv", header = TRUE, sep=",")
View(head(train))
names(train)

dim(train)

####19K rows and 160 vars-thats a bunch need to do some var redux

table(train$classe)
str(train$classe)
summary(train$classe)
length(train$classe)
qplot(train$classe)
###classe A is over represented the rest appear evenly divided 

str(train$cvtd_timestamp)
####20 timestamps-how many observationsare we really looking at?
str(train$new_window)
table(train$new_window)
