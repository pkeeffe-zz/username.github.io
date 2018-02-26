library(caret)
library(dplyr)
library(rpart)

setwd('d:/dev/coursera/Machine Learning/Assignment')
test_final <- read.csv('pml-testing.csv')
train_full <- read.csv('pml-training.csv')

#Split the training set into it's own test/training set for cross-validation
set.seed(3121)
inTrain = createDataPartition(train_full$X, p = 0.75, list = FALSE)

training <- train_full[inTrain,]
testing <-  train_full[-inTrain,]

# First thing to notice is that there are large amounts of missing values
# 300 observations have null values across 67 variables - we will remove these variables.

t<-data.frame(colSums(is.na(training)))
t$name <- rownames(t)
names(t) <- c('na_count','measure')
t<- filter (t,na_count>0)
drop <- t$measure
training <- training[,!(names(training)%in% drop)]


comp_training <- training[complete.cases(training),]
trn <- sapply(comp_training,is.numeric)
trn <- comp_training[,trn]

# Perform PCA Pre-processing to reduce the number of predictors
pca_object <- prcomp(trn, center = TRUE, scale. = TRUE)
pca_var <- pca_object$sdev^2
pca_var_perc <- pca_var * 100 / sum(pca_var)
pca_cum <- cumsum(pca_var_perc)

M <- abs(cor(numeric_training))
diag(M) <- 0
which(M>0.8,arr.ind = T)


#Lets take a look at the columns and see which ones have large numbers of missing values
preProc <- preProcess(trn,method='pca', pcaComp = 7)
#trn$classe <- training$classe
trainPC1 <- predict(preProc,trn)
modelFit <- train(x=trainPC1, y=training$classe,method='rf')

#Lets test the model
testPC <- predict(preProc,testing)
confusionMatrix(testing$classe,predict(modelFit,testPC))


pred <- predict(preProc,test_final)
predict(modelFit,newdata =  pred)



