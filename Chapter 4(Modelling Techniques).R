library(AppliedPredictiveModeling)
library(caret)
data("GermanCredit")
??chapters
scriptLocation()
GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)]
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL
data=GermanCredit
##divide into test and train sets
##set seed to reproduce results
set.seed(200)
fortrain = createDataPartition(data$Class,p=0.8)[[1]]
germantrain=data[fortrain,]
germantest = data[-fortrain,]

set.seed(1056)
svmfit=train(Class~.,data=germantrain,method="svmRadial",preProc=c("center","scale"),
             tuneLength = 10, trControl = trainControl(method="repeatedcv",
                                                       repeats=5,
                                                       classProbs=TRUE))
svmfit
plot(svmfit, scales = list(x=list(log=2)))
##use the model to predict the test data
predicted=predict(svmfit, newdata=germantest,type="prob")
head(predicted)
predictedclasses = predict(svmfit,germantest)
predictedclasses

##use the SVM model
set.seed(1056)
svmfit <- train(Class ~ .,
                data = germantrain,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneLength = 5,
                trControl = trainControl(method = "repeatedcv", 
                                         repeats = 5,
                                         classProbs = TRUE))
svmfit
predictedclasses=predict(svmfit,germantest)
predictedclasses
set.seed(1056)
glmfit = train(Class~.,
               germantrain,
               method="glm",
               trControl=trainControl(method="repeatedcv",repeats=5))
glmfit
glmpredictclass = predict(glmfit,germantest)
glmpredictclass
##cpmpare the 2 models
resamp = resamples(list(SVM=svmfit,Logistic=glmfit))
summary(resamp)
modeldiff=diff(resamp)
summary(modeldiff)
