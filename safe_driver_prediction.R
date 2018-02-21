###############     Include libraries     ######################

rm(list = ls())
setwd("E:/My documents/Sem-3/D/wd/")

library("dplyr")
library("ggplot2")
library("ROSE")
library("caret")
library("MLmetrics")

###############    Preparing initial data set   ##################

train <- read.csv("train.csv", header = TRUE, na.strings = c(""," ","-1","-1.0"))
str(train)

test <- read.csv("test.csv", header = TRUE, na.strings = c(""," ","-1","-1.0"))
str(test)

#Let us combine train and test datasets for pre-processing

test$target <-  0 #creating a target variable in test data
test$data <-  "test" #creating another variable to identify the test data rows
test <-  test[, c(1, 60, 59, 2:58)] #reforming with newly created variables

train$data <- "train" #creating another variable to identify the train data rows
train <-  train[, c(1, 60, 2:59)] #reforming with newly created variables

data <- as.data.frame(rbind(train, test)) #combining test and train data
str(data)

rm(train, test)

###############    Data pre processing   #################

###Converting data types of variables

var_groups = read.csv("var_groups.csv") #loading the self made csv file
head(var_groups) #head of the var_groups

#finding the intersecting names between combined_data and var_groups
names = intersect(colnames(data), var_groups[["names"]])
#Changing the data types according to the data description
for(var_name in names){
  var_type = subset(var_groups, names %in% var_name, select=type)
  if(var_type == "numeric")
    data[,var_name] = as.numeric(data[,var_name])
  else if(var_type == "binary" || var_type == "categorical")
    data[,var_name] <- as.factor(data[,var_name])
  else if(var_type == "ordinal")
    data[,var_name] <- as.ordered(data[,var_name])
}
rm(var_groups,names,var_type,var_name,i) #removing un-neccesary things to save RAM


### Missing value analysis

sum(is.na(data))
missing_values <-  as.data.frame(colSums(is.na(data)))
names(missing_values) <- "count"

#bar plot of no.of missing values in each coloumn
missing_values %>% 
  ggplot(aes(row.names(missing_values) ,count)) +
  geom_col(fill = "chartreuse2") +
  ggtitle("Missing Values") +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab(NULL) +
  coord_flip()

rm(missing_values)

#Creating a vector of variables having more than 5% as missing values
vector <- data[, lapply( data, function(m) sum(is.na(m)) / length(m) ) >= .05 ]
#removing the columns in vector from the main data
data <-  data[,!(colnames(data) %in% colnames(vector))]
rm(vector) 

#Now lets impute remaining columns with missing data

#Designing a function to impute factor columns with mode.
mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

#imputing missing values in factor columns with mode and in numerical columns with Mean.

data$ps_ind_02_cat[is.na(data$ps_ind_02_cat)]<-mode(data$ps_ind_02_cat) #imputing with mode
data$ps_ind_04_cat[is.na(data$ps_ind_04_cat)]<-mode(data$ps_ind_04_cat)
data$ps_ind_05_cat[is.na(data$ps_ind_05_cat)]<-mode(data$ps_ind_05_cat)
data$ps_car_01_cat[is.na(data$ps_car_01_cat)]<-mode(data$ps_car_01_cat)
data$ps_car_02_cat[is.na(data$ps_car_02_cat)]<-mode(data$ps_car_02_cat)
data$ps_car_07_cat[is.na(data$ps_car_07_cat)]<-mode(data$ps_car_07_cat)
data$ps_car_09_cat[is.na(data$ps_car_09_cat)]<-mode(data$ps_car_09_cat)
data$ps_car_11[is.na(data$ps_car_11)]<-mode(data$ps_car_11)
data$ps_car_12[is.na(data$ps_car_12)]<-mean(data$ps_car_12,na.rm=T) #imputing with mean

sum(is.na(data))  #checking for missing values

rm(mode)

################     Building model and prediction of test data     ##################

train <- data %>% filter(data == "train")
train$data <- NULL

test <- data %>% filter(data == "test")
test$data <- NULL
test$target <- NULL

rm(data)

#Let us observe the target variable in train dataset
table(train$target)/nrow(train)

#Let us create balanced dataset from train dataset using ROSE package
df <- ovun.sample(target~.,data = train, method = "both", N = 90000, p =.5, seed = 1)$data  #balanced dataframe
table(df$target)/nrow(df)

#dividing train dataset into train and test
s <- sample(nrow(df),round(nrow(df)*0.7),replace=FALSE)
tr <- df[s,]
te <- df[-s,]

#Building random forest model
#Random forest takes only 53 levels. So, removing "ps_car_11_cat" variable as it is having more factor levels
tr$ps_car_11_cat <- NULL
te$ps_car_11_cat <- NULL

rm(df,s)

library("randomForest")

model_rf = randomForest(as.factor(target) ~. , data = tr) # Fit Random forest
summary(model_rf)

pred_rf <- predict(model_rf,te) #predictin using the model
summary(pred_rf)

#accuracy of the model
confusionMatrix(pred_rf,te$target)

#Plot the model and variable importance
plot(model_rf, main = "Model_RF") #Plot for number of trees and error

#png("Var_imp.png")
varImpPlot(model_rf) #Plot for Important variable

library(pROC)
#ROC plot for the model
aucrf <- roc(as.numeric(te$target), as.numeric(pred_rf),  ci=TRUE)
#png("ROC.png")
plot(aucrf, ylim=c(0,1), print.thres=TRUE, main=paste('Random Forest AUC:',round(aucrf$auc[[1]],3)),col = 'blue')

###  predicting the test dataset ####

test_rf = test[,-ps_car_11_cat]
pred_rf_test <- predict(model_rf,test,type="prob")

summary(pred_rf_TEST)

final_data = data.frame(test$id ,pred_rf_TEST[,2]) #Prob of driver will claim insurance
sum(is.na(final_data)) #checking for missing values
colnames(final_data) <- c("id", "target") 
rownames(final_data) = NULL #just changing the indeces

#Final submission file in mentioned format.

write.csv(final_data,file="Final_submission.csv")
