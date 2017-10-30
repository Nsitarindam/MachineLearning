path <- "C:/Users/arisharma/Downloads/train"
setwd(path)
library(data.table)

train <- fread("train.csv",na.strings = c(""," ","?","NA",NA))
test <- fread("test.csv",na.strings = c(""," ","?","NA",NA))
dim(train)
str(train)

#encoding target values to 0,1
train[ ,income_level:=ifelse(income_level =="-50000",0,1)]
test[,income_level := ifelse(income_level == "-50000",0,1)]

#As seen in str() above, the columns in the both data set aren't as per column classes given on data set page. 
#updating column data types 

#separating All the categorical variables as per the data set page 
factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
#separating All the numerical variables as per the data set page
numcols <- setdiff(1:40,factcols)
#changing classes of varibales as desired
train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

#substet categorical variables
cat_train <- train[ ,factcols,with=FALSE]
cat_test <- test[,factcols,with=FALSE]
#subset numerica variables
num_train <- train[ ,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE]

#removing train to free space
rm(train,test)

library(ggplot2)
#library(plotly)

tr <- function(a){
  ggplot(data = num_train, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
}

tr(num_train$age)
tr(num_train$wage_per_hour)
tr(num_train$capital_gains)
tr(num_train$capital_losses)
tr(num_train$dividend_from_Stocks)
tr(num_train$num_person_Worked_employer)
tr(num_train$weeks_worked_in_year)

#Write code for normalizing data
#########################
##add target variable to num variable data
num_train[,income_level := cat_train$income_level]

ggplot(data=num_train,aes(x = age, y=wage_per_hour))+geom_point(aes(colour=income_level))+scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))


##################################
library(leaps)
subset_analysis <- regsubsets(income_level~ .,num_train,really.big = FALSE,nvmax = 5)
subset_analysis_trans <- regsubsets(income_level~ .,trans_train,really.big = FALSE,nvmax = 5)
summary(subset_analysis_trans)

summary(subset_analysis)

##################################
table(is.na(num_train))
library(caret)
dim(num_train)
ax <-findCorrelation(x = cor(num_train[ ,1:7]), cutoff = 0.7)
num_train <- num_train[,-ax,with=FALSE] 
num_test[,weeks_worked_in_year := NULL]


mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100
mvte <- sapply(cat_test, function(x){sum(is.na(x)/length(x))}*100)
View(mvtr)
cat_train <- subset(cat_train, select = mvtr < 5 )
cat_test <- subset(cat_test, select = mvte <5)

#For the rest of missing values, a nicer approach would be to label them as 'Unavailable'.
cat_train <- cat_train[,names(cat_train) := lapply(.SD, as.character),.SDcols = names(cat_train)]
for (i in seq_along(cat_train)) set(cat_train, i=which(is.na(cat_train[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_train <- cat_train[, names(cat_train) := lapply(.SD,factor), .SDcols = names(cat_train)]

cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, as.character), .SDcols = names(cat_test)]
for (i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value="Unavailable")
#convert back to factors
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, factor), .SDcols = names(cat_test)]


for(i in names(cat_train)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}


num_train[,.N,age][order(age)]
num_train[,.N,wage_per_hour][order(-N)]
num_train[,.N,capital_gains][order(-N)]
num_train[,.N,capital_losses][order(-N)]

library(tree)
library(randomForest)
model <- tree(income_level~.,data = num_train)
set.seed(1)
rand_f <- randomForest(income_level ~.,data = num_train,mtry=6,ntree=5)
plot(model)
text(model,pretty=0)
num_train[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_train[,age := factor(age)]

num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,labels = c("young","adult","old"))]
num_test[,age := factor(age)]

#Bin numeric variables with Zero and MoreThanZero
num_train[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_train[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_train[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]


num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]
num_test[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]
num_test[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]
num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

#combining numeric and categorical variables
d_train <- cbind(num_train,cat_train)
rm(cat_train,num_train)
d_train <- d_train[ ,1:36]

#load library for machine learning
library(mlr)
#creating tasks using mlr package
train.task <- makeClassifTask(data = d_train,target = "income_level")
test.task <- makeClassifTask(data=d_test,target = "income_level")

#remove zero variance features
train.task <- removeConstantFeatures(train.task)
test.task <- removeConstantFeatures(test.task)

#get variable importance chart
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)


#Resolving Unbalanced classification issue 
#undersmapling
train.under <- undersample(train.task,rate = 0.1) #keep only 10% of majority class
table(getTaskTargets(train.under))

#Oversampling
train.over <- oversample(train.task,rate=15) #make minority class 15 times
table(getTaskTargets(train.over))

#SMOTE
train.smote <- smote(train.task,rate = 10,nn = 3) 
table(getTaskTargets(train.smote))

#list of algorithms available in MLR package
listLearners("classif","twoclass")[c("class","package")]

#naive Bayes
naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
naive_learner$par.vals <- list(laplace = 1)

#10fold CV - stratified
folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)

#cross validation function
fun_cv <- function(a){
  crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
  crv_val$aggr
}

fun_cv (train.task) 
# acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
# 0.7337249       0.8954134     0.7230270    0.2769730

fun_cv(train.under) 
# acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
# 0.7637315      0.9126978     0.6651696     0.3348304

fun_cv(train.over)
# acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
#   0.7861459     0.9145749     0.6586852    0.3413148

fun_cv(train.smote)
# acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
#   0.8562135     0.9168955    0.8160638     0.1839362

##train.smote gives the highest true positive rate and true negative rate

#train and predict
nB_model <- train(naive_learner, train.smote)
nB_predict <- predict(nB_model,test.task)

#evaluate
nB_prediction <- nB_predict$data$response
dCM <- confusionMatrix(d_test$income_level,nB_prediction)
# Accuracy : 0.8174
# Sensitivity : 0.9862
# Specificity : 0.2299

#calculate F measure
precision <- dCM$byClass['Pos Pred Value']
recall <- dCM$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure 


#xgboost
set.seed(2002)
xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
xgb_learner$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 150,
  print.every.n = 50
)

#define hyperparameters for tuning
xg_ps <- makeParamSet( 
  makeIntegerParam("max_depth",lower=3,upper=10),
  makeNumericParam("lambda",lower=0.05,upper=0.5),
  makeNumericParam("eta", lower = 0.01, upper = 0.5),
  makeNumericParam("subsample", lower = 0.50, upper = 1),
  makeNumericParam("min_child_weight",lower=2,upper=10),
  makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
)

#define search function
rancontrol <- makeTuneControlRandom(maxit = 5L) #do 5 iterations

#5 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)

#tune parameters
xgb_tune <- tuneParams(learner = xgb_learner, task = train.task, resampling = set_cv, measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, control = rancontrol)
# Tune result:
# Op. pars: max_depth=3; lambda=0.221; eta=0.161; subsample=0.698; min_child_weight=7.67; colsample_bytree=0.642
# acc.test.mean=0.948,tpr.test.mean=0.989,tnr.test.mean=0.324,fpr.test.mean=0.676

Now, we can use these parameter for modeling using xgb_tune$x which contains the best tuned parameters.

#set optimal parameters
xgb_new <- setHyperPars(learner = xgb_learner, par.vals = xgb_tune$x)

#train model
xgmodel <- train(xgb_new, train.task)

#test model
predict.xg <- predict(xgmodel, test.task)

#make prediction
xg_prediction <- predict.xg$data$response

#make confusion matrix
xg_confused <- confusionMatrix(d_test$income_level,xg_prediction)
Accuracy : 0.948
Sensitivity : 0.9574
Specificity : 0.6585

precision <- xg_confused$byClass['Pos Pred Value']
recall <- xg_confused$byClass['Sensitivity']

f_measure <- 2*((precision*recall)/(precision+recall))
f_measure
#0.9726374 



