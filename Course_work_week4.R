# downloading data files
Url_train_data <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
File_train_data <- "pml-training.csv"
if (!file.exists(File_train_data)) {
        download.file(Url_train_data, File_train_data, mode = "wb")
}
Url_test_data <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
File_test_data <- "pml-testing.csv"
if (!file.exists(File_test_data)) {
        download.file(Url_test_data, File_test_data, mode = "wb")
}

# reading files
Train_Data <- read.csv("pml-training.csv")
Test_Data <- read.csv("pml-testing.csv")

# explore training data set
dim(Train_Data)
str(Train_Data[,1:10])
table(Train_Data$classe)

# NA % count
mean(is.na(Train_Data))

Train_Data <- Train_Data[,-c(1:7)]

#deleting variables with near zero variance
library(caret)
NZV <- nearZeroVar(Train_Data,saveMetrics=TRUE)
Train_Data <- Train_Data[, (NZV$nzv==FALSE)]

#deleting columns with NAs
Count_NA <- sapply(Train_Data, function(x) sum(length(which(is.na(x)))))
table(Count_NA)
Train_Data <- Train_Data[, (Count_NA < 19216)]

dim(Train_Data)

### Set seed for reproducible results
set.seed(12062017)
training <- createDataPartition(Train_Data$classe, p=0.7, list=FALSE)
Train_split <- Train_Data[training,]
Test_split <- Train_Data[-training,]

#Building the models
trainctrl <- trainControl(method = "cv", 5, verboseIter = TRUE)
RF_fitted <- train(classe ~ ., data=Train_split, method="rf", trControl = trainctrl)
GBM_fitted <- train(classe ~ ., data=Train_split, method="gbm", trControl = trainctrl)

#Testing on Test_split
RF_test_on_plit <- predict(RF_fitted, Test_split)
confusionMatrix(RF_test_on_plit, Test_split$classe)$overall[1]

GBP_test_on_split <- predict(GBM_fitted, Test_split)
confusionMatrix(GBP_test_on_split, Test_split$classe)$overall[1]
