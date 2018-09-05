runSVM <- function(file_name){
  library(e1071)
  library(xlsx)
  trainingInput <- read.xlsx(file_name, 1, header=T)
  ncols.trainingInput <- ncol(trainingInput)
  trainingInput <- trainingInput[ , 2 : ncols.trainingInput]
  
  trainingOutput<- read.xlsx(file_name, 2, header=T)
  trainingOutput <- matrix(c(trainingOutput[ , 2]), ncol=1)
  
  testData <- read.xlsx(file_name, 3, header=T)
  ncols.testData <- ncol(testData)
  testData <- testData[, 2 : ncols.testData]
  
  colnames(trainingInput) <- paste0("x", 1:ncol(trainingInput))
  colnames(trainingOutput) <- paste0("y", 1:ncol(trainingOutput))
  
  trainingdata <- cbind(trainingInput,trainingOutput)
  
  m <- svm(trainingInput,trainingOutput)
  pred <- predict(m, testData)
  
  return (pred)
}

calculaMAPE <- function(file_name, num_obs, epsilon = 0.1, kernel = "linear") {
  library(e1071)
  library(xlsx)
  
  trainingInput <- read.xlsx(file_name, 1, header=T)
  ncols.trainingInput <- ncol(trainingInput)
  trainingInput <- trainingInput[ , 2 : ncols.trainingInput]
  
  trainingOutput<- read.xlsx(file_name, 2, header=T)
  trainingOutput <- matrix(c(trainingOutput[ , 2]), ncol=1)
  real_value <- trainingOutput[10]
  
  testData <- read.xlsx(file_name, 3, header=T)
  ncols.testData <- ncol(testData)
  testData <- testData[, 2 : ncols.testData]
  
  colnames(trainingInput) <- paste0("x", 1:ncol(trainingInput))
  colnames(trainingOutput) <- paste0("y", 1:ncol(trainingOutput))
  
  trainingInput <- trainingInput[1:num_obs, ]
  trainingOutput <- trainingOutput[1:num_obs, ]
  
  m <- svm(trainingInput,trainingOutput, epsilon = epsilon, kernel = kernel)
  pred <- predict(m, testData)
  
  mape <- (real_value - pred)/real_value
  print(paste("Erro Médio: ", mape))
  print(paste("Resultado: ", pred))
  
  #write.xlsx(pred, file=file_name, sheetName= paste("Test Result", x), col.names=F, row.names=F, append=T)
  
  return(pred)
}
