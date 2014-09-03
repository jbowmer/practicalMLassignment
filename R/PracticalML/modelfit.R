
setwd("/Users/Jake/R/PracticalML")

#training data
urlTrain = "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainCSV = download.file(urlTrain, "train.csv")
training = read.csv("train.csv", stringsAsFactors = FALSE)

training$classe = as.factor(training$classe)


#test data
urlTest = "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testCSV = download.file(urlTest, "test.csv")
testing = read.csv("test.csv", stringsAsFactors = FALSE)
testing$problem_id = as.factor(testing$problem_id)


#the classe variable is the last variable in the dataset and has 5 levels, ABCDE. Classification problem.
#Class A indicates correct technque

#Further split testing data for cross validation purposes:
split = createDataPartition(training$X, p = 0.7, list = FALSE)
preTraining = training[split,]
trainValidation = training[-split,]

#remove first 7 columns from preTraining

preTraining = preTraining[preTraining$new_window == "no", ]
preTraining = preTraining[, 7:160]


#remove non zero variance variables.
nonZero = nearZeroVar(preTraining, saveMetrics = TRUE)

#remove these from the data set
goodvars = row.names(nonZero[nonZero$nzv == FALSE,])

preTraining = preTraining[, goodvars]

#Fit a random forest model.
modFit = train(classe ~ ., data = preTraining, method = "rf",
               trControl = trainControl(method = "cv",
                                        number = 3,
                                        allowParallel = TRUE))

#Apply to cv holdout data, trainValidation

pred = predict(modFit, trainValidation)


#Turns out correct technique is reasonable common and classes are reasonably balanced. 
#Accuracy is therefore a reasonable measure of performance for any model.

confusionMatrix(pred, trainValidation$classe)


#Final Prediction and write it into a file.

finalPred = predict(modFit, testing)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i], file = filename, quote = FALSE,
                row.names = FALSE, col.names = FALSE)
  }
}
pml_write_files(finalPred)

#works. Make sure the markdown file has a discussion of out of sample accuracy etc.


