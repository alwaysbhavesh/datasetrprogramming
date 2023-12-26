#Install and load library
install.packages("caret")
install.packages("rpart")
install.packages("cluster")


library(rpart)
library(caret)
library(class)
library(e1071)
library(cluster)

df<-read.csv("C:\\Users\\91935\\Downloads\\heart.csv")
view(df)
str(df)
df=df[!duplicated(df),]
set.seed(123)
train_indices<-sample(1:nrow(df),0.8*nrow(df))
train_data<-df[train_indices,]
test_data<-df[-train_indices,]

# Train a KNN model
knn_model<-knn(train=train_data,
               test=test_data,
               cl=train_data$DEATH_EVENT,
               k=3
)

# Evaluate the model
confusion_matrix <- table(knn_model, test_data$DEATH_EVENT)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

#Naive Bayes Model
# Train a Naive Bayes model
nb_model <- naiveBayes(DEATH_EVENT ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(nb_model, newdata = test_data)

# Evaluate the model
confusion_matrix <- table(predictions, test_data$DEATH_EVENT)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Decision Tree Model

# Train a Decision Tree model

dt_model <- rpart (DEATH_EVENT ~., data =train_data)

# Make predictions on the test set 
predictions_dt <- predict(dt_model, newdata = test_data)

# Evaluate the model

confusion_matrix_dt <- table(predictions_dt, test_data$DEATH_EVENT) 
print(confusion_matrix_dt)

# Calculate accuracy

accuracy_dt <- sum(diag(confusion_matrix_dt)) / sum(confusion_matrix_dt) 
print (paste("Decision Tree Accuracy:", accuracy_dt))


# Train a Logistic Regression model
logistic_model <- glm(DEATH_EVENT ~ ., data = train_data,)

# Make predictions on the test set
predictions_logistic <- predict(logistic_model, newdata = test_data)

logistic_predictions <- ifelse(predictions_logistic > 0.5, 1, 0)

# Evaluate the Logistic Regression model
confusion_matrix_logistic <- table(logistic_predictions, test_data$DEATH_EVENT)


accuracy_logistic <- sum(diag(confusion_matrix_logistic)) / sum(confusion_matrix_logistic)
print(paste("Logistic Regression Accuracy:", accuracy_logistic))


# Apply K-Means for clustering (example with two clusters)
kmeans_clusters <- kmeans(df[], centers = 2)

# Print cluster assignments
print(kmeans_clusters$cluster)

# Confusion Matrix
cm <- table(df$DEATH_EVENT, kmeans_clusters$cluster)

#Accuracy
accuracy_logistic <- sum(diag(cm)) / sum(cm)
print(paste("K Mean Clusturing Accuracy:", accuracy_logistic))

other=read.csv("https://github.com/alwaysbhavesh/datasetrprogramming/blob/de8569a4cb3b041d9b8a9e2f31d1e76932157c1c/Untitled%20spreadsheet%20-%20Sheet1.csv")
View(other)



# Install required packages
install.packages("modeest")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("tidyverse")

# Load the packages
library(modeest)
library(ggplot2)
library(corrplot)
library(tidyverse)

# Load Dataset
example = read.csv("C:\\Users\\91935\\Downloads\\Z-Alizadeh sani dataset.csv")
View(example)
head(example)
tail(example)
dim(example)
names(example)
glimpse(example)

# Checking for duplicates
sum(duplicated(example))

# Removing duplicates
example = example[!duplicated(example),]

# Rechecking for duplicates
sum(duplicated(example))

# EXPLORATORY DATA ANALYTICS

# 1) DESCRIPTIVE STATISTICS:-   mean,mode,median,variance,std. deviation
mean_age = mean(example$Age)
print(paste("The mean of age is : ", mean_age,"years"))

median_age = median(example$Age)
print(paste("The median of age is : ", median_age,"years"))

mode_cholestrol = mlv(example$Age)
print(paste("The mode is: ",mode_cholestrol))

Variance = var(example$Age)
print(paste("The variance is: ",Variance))

Standard_Dev = sd(df$Age)
print(paste("The Standard Deviation is: ",Standard_Dev))

range(df$Age)

# 2) Univariate Analysis
# Histogram Example with a numeric variable 'age'
ggplot(example,aes(BMI))+geom_histogram(bins=30)
ggplot(example,aes(Age))+geom_histogram(bins=30)
ggplot(example,aes(BP))+geom_histogram(bins=30)

# 3) Bivariate Analysis

#Scatter Plot
ggplot(example,aes(Length,Weight,col=Sex))+geom_point()+labs(title = "Scatter Plot of Weight vs Height",x="Height",y="Weight")


#Bar Plot
ggplot(example, aes(Sex)) +
  geom_bar() +
  labs(title = "Distribution of Gender and Heart Disease")


#boxplot for 'age'
ggplot(example, aes(Age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age")

#PairWise Plot
pairs(example[,c("Age","BP","Weight","Length")])

#Correlation Matrix
correlation<-cor(example[,c("Age","BP","Weight","Length")])
corrplot(correlation,method="color")
