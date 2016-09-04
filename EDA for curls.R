###get the data-training only since we are exploring

library(tibble)
library(dplyr)
library(caret)
library(tibble)

install.packages("purrr")
library(purrr)

train<-read.csv("trainingdata.csv", header = TRUE, sep=",", stringsAsFactors = F)
test<-read.csv("testdata.csv", header = TRUE, sep=",", stringsAsFactors = F)

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
table(train$cvtd_timestamp)

###we can see each subject some of type of rep
plot(train$user_name, train$classe)

str(train)

sum(complete.cases(train))
###only 406 cases have no na s so we cant just discard them 

summary(train)


###sure purrr to get nmeric/do some data cleanaing 
train$user_name<-factor(train$user_name)
train$classe<-factor(train$classe)
train$cvtd_timestamp<-as.Date(train$cvtd_timestamp)###this is jacked up 
head(train$cvtd_timestamp)
train$new_window<-factor(train$new_window)

train<-train %>% map_if(is.character, as.numeric) %>% tbl_df() 

###get rid of exccess marker vars 
train<-select(train,-X,-user_name,-raw_timestamp_part_1,-raw_timestamp_part_2,-cvtd_timestamp,-new_window,-num_window)

train<-select(train,roll_belt, pitch_belt, yaw_belt, total_accel_belt, gyros_belt_x, gyros_belt_y, gyros_belt_z,
              accel_belt_x,accel_belt_y, accel_belt_z, magnet_belt_x,magnet_belt_y, magnet_belt_z, roll_arm,
              pitch_arm, yaw_arm, total_accel_arm,gyros_arm_x, gyros_arm_y, gyros_arm_z, accel_arm_y, accel_arm_x,
              accel_arm_z, magnet_arm_x, magnet_arm_y, magnet_arm_z, roll_dumbbell, pitch_dumbbell,
              yaw_dumbbell, classe)

test<-select(test,roll_belt, pitch_belt, yaw_belt, total_accel_belt, gyros_belt_x, gyros_belt_y, gyros_belt_z,
             accel_belt_x,accel_belt_y, accel_belt_z, magnet_belt_x,magnet_belt_y, magnet_belt_z, roll_arm,
             pitch_arm, yaw_arm, total_accel_arm,gyros_arm_x, gyros_arm_y, gyros_arm_z, accel_arm_y, accel_arm_x,
             accel_arm_z, magnet_arm_x, magnet_arm_y, magnet_arm_z, roll_dumbbell, pitch_dumbbell, yaw_dumbbell
)

#####creat training a test set-we'll use the gien testset only once for validation

inTrain<-createDataPartition(y=train$classe, p=.7, list=FALSE)
training<-train[inTrain,]
testing<-train[-inTrain,]



####lets try clusters


modFit_rpart<-train(classe~.,method="rpart", data=training, na.action=na.pass)

fit<-lm(classe~., data=training,na.action = na.pass)

###keeps running out of memory....
training_comp<-na.omit(training)
dim(training_comp)

training_sample<-training[sample(1:nrow(training),1000, replace = FALSE),]

####why is accuracy going down as sample goes up?
modFit<-train(classe~magnet_belt_x+magnet_belt_y+total_accel_dumbbell+min_pitch_belt,method="rpart", data=training, na.action = na.pass)
fit<-lm(classe~magnet_belt_x, data=training_comp,na.action = na.pass)

modFit_rpart<-train(classe~.,method="rpart", data=training, na.action = na.pass, preProcess=c("center","scale"))

modFit_rpart_impute<-train(classe~.,method="rpart", data=training, preProcess=c("center","scale"))

install.packages("randomForest")
library(randomForest)

training_roughfix<-na.roughfix(training)

library(caret)

modFit_rf<-train(classe~.,method="rf", data=training, na.action = na.pass)
modFit<-train(classe~.,method="gbm",  data=training, na.action = na.pass)

modFit_rf_fix<-train(classe~.,method="rf", data=training_roughfix, na.action=na.pass)

install.packages("mice")
library(mice)

training_imp<-mice(training, m=5, seed = 1234)

str(training_imp$data)

training_imp$nmis
training_imp$predictorMatrix
training_imp$post
training_imp$iteration
training_imp$imp


training_imp1<-complete(training_imp, action=5)

n_missing<-icn(training_imp1)



str(training_imp1)

modFit_rpart<-train(classe~.,method="rpart", data=training_imp1, na.action = na.pass, preProcess=c("center","scale"))

modFit_cforest<-train(classe~.,method="cforest", data=training_imp1, na.action = na.pass)

sum(complete.cases(training_imp1))

modFit_rpart$results

na.mat<-md.pattern(training)

training_complete<-na.omit(training)
View(na.mat)
head(names(training_sample))

library(rattle)
fancyRpartPlot(modFit$finalModel)
modFit$results

rpart_predict<-predict(modFit$finalModel,(testing))

tail(rpart_predict)

###PCA-lets shrink this huge list of predictors 
library(psych)

##lets try svm
library(e1071)

modFit_svm<-svm(classe~., data=training, na.action =na.pass)

na_count_D<-sapply(subset(training, classe=="D"), function(y) sum(length(which(is.na(y)))))
na_count_C<-sapply(subset(training, classe=="C"), function(y) sum(length(which(is.na(y)))))
na_count_B<-sapply(subset(training, classe=="B"), function(y) sum(length(which(is.na(y)))))
na_count_A<-sapply(subset(training, classe=="A"), function(y) sum(length(which(is.na(y)))))
na_count_E<-sapply(subset(training, classe=="E"), function(y) sum(length(which(is.na(y)))))


na_count_C

qplot(na_count_A, names(train))


###prediction

View(predict(modFit_rpart, test))

train_svm<-svm(classe~.,training)

train_svm$tot.nSV

predict(train_svm, test)

svm_testing_preds<-predict(train_svm, testing)

svm_table<-table(svm_testing_preds, testing$classe)

barplot(table(svm_testing_preds))

barplot(table(testing$classe))



head(svm_table)

svm_final<-predict(train_svm, test)
View(svm_final)

confusionMatrix(svm_table)

library(e1071)


#1-Not A
#3-Not c
#6 Not D
#8 Not A
