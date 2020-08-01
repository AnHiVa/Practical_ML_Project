# Practical_ML_Project
Coursera Practical Machine Learning  Course Project

[RPubs website](https://rpubs.com/AnHiVa/pml-project)

# Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

# Data
The training data for this project are available here: [Training data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)
The test data are available here: [Test data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har.

# Goal
The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

Cross validation of decision tree and random forest models will be performed.

# Result
Based on the results, the Random Forest algorithm had a better outcome than Decision Trees. The Random Forest accuracy was 97.87% (0.9753, 0.9818) with a 95% confidence level. While the Decicsion Tree model has an accuracy of 56.95% (0.5584, 0.5805) with a 95% confidence level. Therefore, the Random Forest model is selected to perform the predicition for the assignment test set. With an expected out of sample error of 5%.
