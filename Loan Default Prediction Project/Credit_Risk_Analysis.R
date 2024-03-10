
#Pre-requsites before running the program
install.packages("ModelMetrics")
install.packages("gower")
install.packages("e1071")
install.packages("corrplot")
install.packages("fBasics")
install.packages("Amelia")
install.packages("rlang")
install.packages("MASS")
install.packages("faraway")
install.packages("Hmisc")
install.packages("latticeExtra")
install.packages("ROCR")
install.packages("DescTools")


################################################################################################################################################################################################
#CREATE THE REQUIRED FUNCTIONS - START
################################################################################################################################################################################################


################################################################################################################################################################################################
#LOAD THE REQUIRED LIBRARIES
################################################################################################################################################################################################

loadlibraries<- function(){
  library(readxl)
  library(plyr)
  library(rlang)
  library(dplyr)
  library(caTools)
  library(mlbench)
  library(ggplot2)
  library(caret)
  library(e1071)
  library(tidyr)
  library(corrplot)
  library(fBasics)
  library(Amelia)
  library(mlbench)
  library(dummies)
  library(MASS)
  library(Hmisc)
  library(DescTools)
  library(ROCR)
}

#Function to clean the memory object, load the required libraries and ensure reproducibility
initializeEnvironment <- function(){
  
  #Remove objects from memory
  rm(list=ls())
  print("Removed all the memory objects")

  # Load the required libraries
  loadlibraries()
  print("Loaded the required libraries")
  
  # Ensure results are repeatable or reproducible
  set.seed(2345)

}


#Function to accept and set the base working directory for the training/verification data files.
setWorkingDirectory <- function(baseDir){
  if(baseDir ==''){
      baseDir <- readline(prompt = "Enter the base directory where training/new data files are located : ") 
  }
  base::setwd(baseDir)  
  print(paste(" working directory set to :",baseDir))

}


#Function to accept the data files for training the model or  making predictions
#If not entered, it assumes the default values as defined below 
isTrainingMode <- function(trainModel){
  if(trainModel==''){
    trainModel <- readline(prompt = "Do you like to train  the model(Y/N)? :")   
    trainModel <- "Y"
  }  
  return (trainModel)
}


#Function to accept the data files for training the model or  making predictions
#If not entered, it assumes the default values as defined below 
getDataFile <- function(trainModel, dataFileName){
  if(dataFileName==''){
    dataFileName <- readline(prompt = "Enter the data file name or press enter to use default file :")
    if(trainModel== "Y"){
      dataFileName <- "CreditData_Training.xls"
      print(paste("Data file being used for training the model -> ",dataFileName))
    } else {
      dataFileName <- "CreditData_verify.xlsx"
      print(paste("Data file being used for predictions -> ",dataFileName))
    }
  }
  return (dataFileName)
}


# function to load the data from an excel file
# Input param: dataFile represents path of the data file
getDataFromExcel <- function(dataFile) {
  return(read_excel(dataFile))  
}

# function to print the column names and summarize the data set
# Input param: tempData represents the data set
summarizeData <- function(tempData){
  print("No of observations and features in the dataset")
  print(dim(tempData))
  
  print("Names of the feature  in the dataset")
  print(base::colnames(tempData))
  
  print("Dataset summary")
  print(base::summary(tempData))
}

#Function to check the data types of the data set
# Input param: tempData represents the data set
checkDataTypes <- function(tempData){
  base::sapply(tempData,class)
}


# Function to transform the data types of the credit data set
# Input param: tempData represents the data set
transformDataTypes <- function(tempData){
  tempData <- base::transform(tempData, 
               CHK_ACCT = as.factor(CHK_ACCT),
               HISTORY= as.factor(HISTORY),
               NEW_CAR=as.factor(NEW_CAR),
               USED_CAR=as.factor(USED_CAR),
               FURNITURE=as.factor(FURNITURE),
               RADIO_TV=as.factor(RADIO_TV),
               EDUCATION=as.factor(EDUCATION),
               RETRAINING =as.factor( RETRAINING),
               SAV_ACCT =as.factor( SAV_ACCT),
               EMPLOYMENT =as.factor( EMPLOYMENT),
               MALE_DIV =as.factor( MALE_DIV),
               MALE_SINGLE =as.factor( MALE_SINGLE),
               MALE_MAR_or_WID =as.factor( MALE_MAR_or_WID),
               CO_APPLICANT =as.factor( CO_APPLICANT),
               GUARANTOR =as.factor( GUARANTOR),
               PRESENT_RESIDENT =as.factor( PRESENT_RESIDENT),
               REAL_ESTATE=as.factor(REAL_ESTATE),
               PROP_UNKN_NONE=as.factor(PROP_UNKN_NONE), 
               OTHER_INSTALL    =as.factor( OTHER_INSTALL),
               RENT=as.factor( RENT),
               OWN_RES=as.factor(OWN_RES),
               JOB =as.factor( JOB),
               TELEPHONE        =as.factor( TELEPHONE),
               FOREIGN=as.factor( FOREIGN),
               DEFAULT=as.factor( DEFAULT))
  
  return (tempData)

}

# Function to visualize the missing values 
# Input param: tempData represents the data set
visualizeMissingData <- function(tempData){
  Amelia::missmap(tempData, col=c("red", "green"), legend=TRUE,main = "Missing values Map of Credit Data set")
}

# Generic function to drop the features or columns from the data set
# Input param: tempData represents the data set
# Input param: dropFeatures represents the columns to be dropped
dropFeaturesFormDataset <- function(tempData, dropFeatures){
  tempData <- tempData[,!(names(tempData) %in% dropFeatures)]
  return (tempData)

}

# Generic function to handle the outliers in the numeric features or columns in the data set
# Input param: tempData represents the data set
# Input param: featureName represents the feature in the data set
# Input param: percentile represents the percentile value that will be used to replace the outliers with
handleOutlier<- function(tempData, featureName, percentile){
  index <- grep(featureName, colnames(tempData))
  clampedValue <- quantile(tempData[,index],percentile)
  tempData[,index][which(tempData[,index]>clampedValue)]<-clampedValue
  return (tempData)
  
}

# Generic function to visualize the numeric features data distribution using box plots
# Input param: tempData represents the data set
# Input param: features represents the names of the features in the data set
VisualizeBoxPlots <- function(tempData, features ){
  par(mfrow=c(length(features), 1))
  for(i in features) {
    boxplot(tempData[,i], main=i, col="green", border="blue", horizontal = TRUE)
  }
} 

# Generic function to standardize the features, removes the original features and appends the standardized features
# Input param: tempData represents the data set
# Input param: normalizeFeatures represents the names of the features in the data set that needs to be normalized
standardizeFeatures <- function(tempData, normalizeFeatures){
  normalizeData <- tempData[,(names(tempData) %in% normalizeFeatures)]

  #Standardization of features
  preproc1 <- preProcess(normalizeData[,normalizeFeatures], method=c("center", "scale"))
  normalizedData <- predict(preproc1, normalizeData[,normalizeFeatures])

  #Drop normalized features 
  tempData <- tempData[,!(names(tempData) %in% normalizeFeatures)]

  #Bind normalized data to original data
  tempData <- cbind(normalizedData, tempData)
  return (tempData)  

}

# Generic function to create the dummy features for factor features
# Input param: tempData represents the data set
# Input param: factorFeatures represents the features in the data set for which dummy features needs to be created
# Input param: removeFeatures represents the features in the data set that needs to be removed after creating the dummy features
createDummyFeatures <- function(tempData, factorFeatures, removeFeatures){
  
  factorFeaturesData <- tempData[,(names(tempData) %in% factorFeatures)]
  factorFeaturesDataDummy <- dummy.data.frame(factorFeaturesData,all = FALSE)

  factorFeaturesDataDummy <- factorFeaturesDataDummy[,!(names(factorFeaturesDataDummy) %in% removeFeatures)]
  
  
  #Remove factor features from training data and bind dummy varaibles
  tempData <- tempData[,!(names(tempData) %in% factorFeatures)]

  #Bind the tempData with dummy variables 
  tempData <- cbind(factorFeaturesDataDummy,tempData)
  
  return (tempData)

}

# Generic function to remove the insignificant features using stepwise method, build ,save and return the logistic model  
# Input param: tempData represents the data set
buildAndSaveModel <- function(tempData, creditDataModel){
    logFitInitialModel <- glm(DEFAULT ~ ., data = tempData, family = "binomial")
    step <- MASS::stepAIC(logFitInitialModel,direction="both")
    finalModel <- eval(step$call)
    saveRDS(finalModel, creditDataModel)
    return (finalModel)
}

# Generic function to load the previously saved model 
loadModel <- function(creditDataModel){
  return (readRDS(creditDataModel))
}

#Function to get the prediction instance on training data
getPredictionInstance <- function(tempData){
  predictionObj <- ROCR::prediction(tempData$predictedProb,tempData$DEFAULT)
  return (predictionObj)
}

# Generic function to return a confusion matrix for training data
# Input param: model represents the predictive model
# Input param: tempData represents the data set
# Input param: threshold reprsents the cut-off probability to consider the case positive
getPredictionsForTrainingData<- function(model, tempData, threshold){
  tempData$predictedProb <- predict(model,type="response")  
  predictions <- ifelse(tempData$predictedProb >= threshold, 1, 0)
  #confusion matrix. Rows(Horizantal) represent actual data and Columns(vertical) represent predicted data. 
  cMatrix<-table(tempData$DEFAULT, predictions )
  return (list("workData" = tempData, "cMatrix"= cMatrix))
}

# Generic function to return a confusion matrix for new  data
# Input param: model represents the predictive model
# Input param: tempData represents the new data set. This could be just one or multiple observations 
# Input param: threshold reprsents the cut-off probability to consider the case positive
getPredictionsForNewData <- function(model, tempData, threshold){
  tempData$predictedProb <- predict(model,newdata=tempData[,-length(tempData)],type="response")
  predictions <- ifelse(tempData$predictedProb >= threshold, 1, 0)
  #confusion matrix. Rows(Horizantal) represent actual data and Columns(vertical) represent predicted data. 
  cMatrix<-table(tempData$DEFAULT, predictions )
  return (list("workData" = tempData, "cMatrix"= cMatrix))
}

#function to check the goodness of fit of the model
goodnessOfFit <- function(tempData, finalModel){
  
  #Check the fitness of model using chisquare value
  1-pchisq(finalModel$deviance,finalModel$df.residual)

  #calculates the c-statistic for the model  
  #Higher value of c-statistic indicate that the model is good. Values over 0.8 indicate a strong model.
  #DescTools::Cstat(tempData$predicted_prob,tempData$DEFAULT)
}


# Generic function to compute the Performance metrics of th model.
# Input param: confusionMatrix 
# Input param: dataSetType reprsents training/new data  
performanceMetrics <- function(confusionMatrix, dataSetType){
  accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
  print(paste('Accuracy of ', dataSetType , accuracy))
  
  errorRate <- 1-accuracy
  print(paste('Error rate ', dataSetType,  errorRate))
  
  TP<- confusionMatrix[2,2]
  print(paste('True Positives ', TP))
  
  FP<-confusionMatrix[1,2]
  print(paste('False Positive ', FP))
  
  FN<-confusionMatrix[2,1]
  print(paste('False Negative ', FN))
  
  TN<-confusionMatrix[1,1]
  print(paste('True Negative ', TN))
  
  
  # precision is a measure of correctness achieved in positive prediction. Indicates the percentage of positive cases out of the total positive cases predicted.
  # Precision is very low i.e. Model is identifying more positivies than exists. this is fine if we don't want to miss any positives or we are okay to spend a bit more but not miss any positives
  precision <- (TP)/( TP + FP)
  print(paste('Precision, percentage of positive cases out of the predicted positive cases.', precision))
  
  #Sensitivity(or True Positive Rate(TPR) or Recall) is a measure of percentage of  predicted positive cases   out of all  positive cases 
  #We have to aim for high Sensitivity  want to catch most positives(such as disease exists)
  sensitivity <- TP/(TP+FN)
  print(paste('sensitivity, percentage of  predicted positive cases   out of all  the positive cases: ', sensitivity))
  
  #Specificity(True Negative Rate) is a measure of percentage of  negative cases  out of all  negative cases
  #We have to aim for high specifictiy  
  Specificity = TN / (TN + FP)
  print(paste('Specificity,percentage of  predicted negative cases   out of all  negative cases ', Specificity))
  
  #Misclassification rate also known as error rate
  misOrErrorRate<- (FP+FN)/(TP+TN+FP+FN)
  print(paste('Overall mis classification rate ', misOrErrorRate))
 
}

# Generic function to visualize the predictions of the model.
# This function cab be used on any confusion matrix
visualizeConfusionMatrix <- function(confusionMatrix){
  TP<- confusionMatrix[2,2]
  FP<-confusionMatrix[1,2]
  FN<-confusionMatrix[2,1]
  TN<-confusionMatrix[1,1]
  matrixData <- matrix(c(TN, FP, FN, TP), nrow = 2, byrow = TRUE)
  rownames(matrixData) <- c("True Good Credit", "True Bad Credit")
  colnames(matrixData) <- c("Predicted Good Credit", "Predicted Bad Credit")
  ctable <- as.table(matrixData)
  fourfoldplot(ctable, color=c("red","green"), conf.level = 0, margin = 1, main = "Confusion Matrix")
  
}

# Generic function to display the ROC curver of the model
plotROCCurve <- function(predictionObj){
  pred <- predictionObj
  pe <- performance(pred, "tpr", "fpr")
  au <- performance(pred, "auc")@y.values[[1]]
  pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
  p <- ggplot(pd, aes(x=fpr, y=tpr))
  p <- p + geom_line(colour="red")
  p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
  p <- p + ggtitle("ROC Curve")
  p <- p + theme(plot.title=element_text(size=10))
  p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
  p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                    label=paste("AUC =", round(au, 2)))
  print(p)

}

# Function to prepare and return the data set 
# This function will be called for both training and verification data to prepare the data for building model.
# 
prepareData <- function(trainingMode, dataFileName){
  
  #Get the filename of the data set
  if(dataFileName == ''){
    dataFileName <- getDataFile(trainingMode)
  }
    
  
  #Load the data set
  workData <- getDataFromExcel(dataFileName)
  
  if(trainingMode == "Y"){
    summarizeData(workData)  
  }
  #Change column names 
  workData  <- workData  %>% dplyr::rename("RADIO_TV" = "RADIO/TV")
  workData  <- workData  %>% dplyr::rename("CO_APPLICANT" = "CO-APPLICANT") 
  if(trainingMode == "Y"){
    summarizeData(workData)  
  }
  
  #Change data Types 
  if(trainingMode == "Y"){
    checkDataTypes(workData)
  }
  
  workData <- transformDataTypes(workData)
  
  if(trainingMode == "Y"){
    checkDataTypes(workData)
  }

  
  #CheckMissingData 
  if(trainingMode == "Y"){
    visualizeMissingData(workData)
  }
  
  #Drop the features that are not required.
  dropFeatures <- c("OBS.","MALE_DIV","MALE_SINGLE","MALE_MAR_or_WID","CO_APPLICANT","GUARANTOR","PROP_UNKN_NONE","NUM_DEPENDENTS","TELEPHONE","FOREIGN","JOB");
  workData <- dropFeaturesFormDataset(workData, dropFeatures)
  if(trainingMode == "Y"){
    summarizeData(workData)
  }
  
  
  #Handle Outliers by clamping values greater that certain percentile
  outlierCols <- c("DURATION","AMOUNT","AGE")
  if(trainingMode == "Y"){
    VisualizeBoxPlots(workData,outlierCols)
  }
  
  workData <- handleOutlier(workData,"^DURATION$", 0.95)
  workData <- handleOutlier(workData,"^AMOUNT$", 0.95)
  workData <- handleOutlier(workData,"^AGE$", 0.97)
  
  if(trainingMode == "Y"){
    summarizeData(workData)
  }
  
  if(trainingMode == "Y"){
    VisualizeBoxPlots(workData,outlierCols)
  }
  
  #Standardize numeric features
  normalizeFeatures <- c("DURATION","AGE","AMOUNT","INSTALL_RATE","NUM_CREDITS")
  workData <- standardizeFeatures(workData, normalizeFeatures)
  if(trainingMode == "Y"){
    summarizeData(workData)
  }
  
  
  #Create dummy features for factors and remove one feature from each factor
  factorFeatures <- c("CHK_ACCT","HISTORY","SAV_ACCT","EMPLOYMENT","PRESENT_RESIDENT","REAL_ESTATE","OTHER_INSTALL","RENT","OWN_RES")
  removeFeatures <- c("CHK_ACCT3", "HISTORY4","SAV_ACCT4", "EMPLOYMENT4", "PRESENT_RESIDENT4", "REAL_ESTATE1", "OTHER_INSTALL1","RENT1", "OWN_RES1")
  workData <- createDummyFeatures(workData, factorFeatures, removeFeatures)
  
  if(trainingMode == "Y"){
    summarizeData(workData)
  }
  
  return (workData)
  
}






################################################################################################################################################################################################
#CREATE THE REQUIRED FUNCTIONS END
################################################################################################################################################################################################


################################################################################################################################################################################################
#LOAD,EXPLORE and CREATE A MODEL
################################################################################################################################################################################################

#Processes the training data
#Builds the model using the training data
doProcessTrainingData <- function(workData, threshold, creditDataModel){
  finalModel <- buildAndSaveModel(workData, creditDataModel)  
  
  #Get Predictions on training data
  result <- getPredictionsForTrainingData(finalModel, workData , threshold)
  
  #Check goodness of fit on training data
  goodnessOfFit(workData, finalModel)
  
  print("Confusion matrix for the training data")
  result$cMatrix
  
  #Evaluate the model on training data
  performanceMetrics(result$cMatrix,"Training Data Set")
  
  #Display th confustion matrix for training data
  visualizeConfusionMatrix(result$cMatrix)
  
  #Plot ROC Curve for traning data
  predictionObj<- getPredictionInstance(result$workData)
  plotROCCurve(predictionObj)

}

#Processes the verification  data
#Loads  the trained model to make predictions
doProcessVerificationData <- function(workData, threshold, creditDataModel){
  finalModel <- loadModel(creditDataModel)
  
  result <- getPredictionsForNewData(finalModel, workData , threshold)
  
  print("Confusion matrix for the verification data")
  result$cMatrix
  
  #Evaluate the model on verification data
  performanceMetrics(result$cMatrix,"Verification Data Set")
  
  #Display th confustion matrix for verification data
  visualizeConfusionMatrix(result$cMatrix)
  
  #Plot ROC Curve for verification data
  predictionObj<- getPredictionInstance(result$workData)
  plotROCCurve(predictionObj)

}


main <- function(trainingMode, baseDir,dataFileName,threshold){
  # Initialize the R environment
  initializeEnvironment()
  
  # set base working directory from where data files will be loaded
  setWorkingDirectory(baseDir)
  
  #trainingMode is a flag that tells  the program whether we are training the model or predicting the loan defaults using the model. 
  trainingMode <- isTrainingMode(trainingMode)
  
  #Prepare data for building the model or for predicting the loan defaults
  workData <- prepareData(trainingMode,dataFileName )
  
  
  #Build the final Model and get confusion matrix
  # we can accept the threshold from user if needed, for now, it is hardcoded in the program
  threshold <- 0.4
  result <- NULL
  
  #Name of the file to save the model
  creditDataModel <- "GECreditModel.rds"
  
  #If training mode, build and save the model 
  if(trainingMode == "Y"){
    doProcessTrainingData(workData, threshold, creditDataModel)
  }
  
  #Load the model from file and predict on new data in prediction mode.
  if(trainingMode == "N"){
    doProcessVerificationData(workData, threshold, creditDataModel)
  }

}

#This is the main funtion to build the model on the training data as well as  to predict the loan defaults on the verification data.
#The program can run either in training mode or prediction mode. 
#In the training mode, training data set will be used to explore, prepare, build and check the fitness of the model  
#in the prediction mode, verification data set will be used to predict the loan defaults. 
# The main benefit of the program is by switching the paratmeters passed into the program, we can either train the model or predict using the saved model. 
# The following paramters are passed to the main program

# "Y" Indicates to train the model using training data set. 
# "N" Indicates to predict using the model 
trainingMode <- "N"

# Indicates the directory of the data files
baseDir <- "C:/Rao/Documentation/myDocs/Personal/MS_SNHU/DAT-690 Capstone Project/Final Project"  

# training or verification data set file name
# Use the appropriate data set depending on whether we want to train the model or perform predictions
dataFileName <- "CreditData_verify.xlsx"
#dataFileName <- "CreditData_Training.xls"

#threshold indicates the cut-off probability to consider the case as positive
#Probability above the  threshold value are considered as positives
#Sensitivity and specificity of the model can be modified by changing the threshold.
threshold <- 0.4


main(trainingMode, baseDir,dataFileName,threshold )





