library(caret)
library(tidyverse)

# Download datasets

train_filename <- 'pml-training.csv'
test_filename <- 'pml-testing.csv'
  
if (!file.exists(train_filename)){
    
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",train_filename, method = "curl")
  filedownloadtime <- paste(file.info(train_filename)$ctime, Sys.timezone(), sep = " ")
  print(paste("The Weights training set was downloaded:", filedownloadtime,sep = " "))
    
} else {
    
  filedownloadtime <- paste(file.info(train_filename)$ctime, Sys.timezone(), sep = " ")
  print(paste("The Weights training set was downloaded:", filedownloadtime, sep = " "))
    
}

if (!file.exists(test_filename)){
  
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",test_filename, method = "curl")
  filedownloadtime <- paste(file.info(test_filename)$ctime, Sys.timezone(), sep = " ")
  print(paste("The Weights testing set was downloaded:", filedownloadtime,sep = " "))
  
} else {
  
  filedownloadtime <- paste(file.info(test_filename)$ctime, Sys.timezone(), sep = " ")
  print(paste("The Weights testing set was downloaded:", filedownloadtime, sep = " "))
  
}

# EDA

testing <- read_csv('pml-testing.csv')
training <- read_csv('pml-training.csv')

str(training)
# Lots of summary statistics with almost all NA
training_small <- training %>%
  select(-matches('var|kurtosis|skewness|max|min|amplitude|avg|stddev')) %>%
  mutate(classe = as.factor(classe)) %>%
  group_by(classe)

#Due to the specifications in the article, we will use the gyroscope as our predictor
training_gyros <- training_small %>%
  select(matches('classe|gyros'))

#Random Forest

set.seed(7654)
model_rf <- train(classe~.,data=training_gyros, method='rf')

pred_rf <- predict(model_rf, testing)

# K-Fold Cross Validation 
