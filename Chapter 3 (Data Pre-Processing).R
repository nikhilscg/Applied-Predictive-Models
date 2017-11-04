library(AppliedPredictiveModeling)
library(dplyr)

data("segmentationOriginal")
##filter for the TRain data set
segmentationOriginal %>%
  filter(Case=="Train") -> segdata
##remove the cellID,Class,Case columns
cellID <- segdata$Cell
class<- segdata$Class
case<-segdata$Case
segdata<-segdata[,-(1:3)]
##Remove the columns which have status in it as its a binary version of the predictors and not useful
statuscolnames = grep("Status",names(segdata))
statuscolnames
shortsegdata = segdata[,-(statuscolnames)]  
##data transformation
##check for skewness and correct
library(e1071)
##for 1 predictor check skewness
skewness(shortsegdata$AngleCh1)
##we can use the apply fiunction to check for skewness for all predictors as they are all numeric
#in Apply, 1 means apply to rows and 2 means apply to columns
skewvalues = apply(shortsegdata,2,skewness)
## transform the data based on skewness
##use caret package
library(caret)
Ch1AreaTrans = BoxCoxTrans(shortsegdata$AreaCh1)
Ch1AreaTrans
##original data
head(shortsegdata$AreaCh1)
##after transformation
predict(Ch1AreaTrans,head(shortsegdata$AreaCh1))
##to check as per the formula
(819^-0.9-1)/(-0.9)
##PCA, use prcomp after scaling and centering the data
pcaObject = prcomp(shortsegdata,center=TRUE,scale. = TRUE)
pcaObject
##calculate the percent variance accounted for by the PCAs
percentvariance = pcaObject$sdev^2/sum(pcaObject$sdev^2)*100
percentvariance[1:3]
## transformed values are stored in x
head(pcaObject$x[,1:5])
##rotation stores the loading for each variable
head(pcaObject$rotation[,1:3])
##transform the data using preprocess
trans = preProcess(shortsegdata,method=c("BoxCox","center","scale","pca"))
trans
?predict
##apply the transformations using the predict function
transformed = predict(trans,shortsegdata)
head(transformed[,1:5])
##find predictors whose variance is near to 0
nearZeroVar(shortsegdata)
##if any predictors needed to be removed, it would have returned column identifiers 
#for predictors which needed to be removed
##to remove predictors based on between-predictor correlations
correlations = cor(shortsegdata)
dim(correlations)
correlations[1:4,1:4]
##build the correlation plot
library(corrplot)
corrplot(correlations,order="hclust")
?corrplot
##remove the predictors with high inter correlations
highcorr = findCorrelation(correlations,cutoff=0.75)
head(highcorr)
filtershortseg = shortsegdata[,-highcorr]
##creating dummy variables
## use dummyVars
##use the car dataset
library(caret)
car
