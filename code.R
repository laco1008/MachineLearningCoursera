library(caret)
library(tidyverse)

download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "training.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "testing.csv")

training <- read.csv("training.csv")
validation <- read.csv("testing.csv")

library(DataExplorer)
create_report(training, y = "classe")

# Function to filter columns based on NA percentage
select_less_NA <- function(data_frame, threshold) {
        num_rows <- nrow(data_frame)
        na_counts <- colSums(is.na(data_frame))
        selected_columns <- names(data_frame)[na_counts/num_rows <= threshold]
        return(data_frame[selected_columns])
}


training <- training[,-(1:7)]
training[training == ""] <- NA



training <- select_less_NA(training, 0.0)

inTrain <-  createDataPartition(y=training$classe, times=1, p=0.70, list=FALSE)
train <- training[inTrain,]
test <- training[-inTrain,]

fit1 <- train(classe ~ ., method = "rf", data = train, preProcess = "pca")
fit2 <- train(classe ~ ., method = "gbm", data = train, verbose = FALSE, preProcess = "pca")
fit3 <- train(classe ~ ., method = "treebag", data = train, preProcess = "pca")

combo <- data.frame(fit1 = predict(fit1, test[,-53]), fit2 = predict(fit2, test[,-53]), fit3 = predict(fit3, test[,-53]), true = test[,53])





confusionMatrix(factor(test[,53]), predict(fit3, test[,-53]))



data.frame(num = c(1:20), pred = predict(fit1, validation))

