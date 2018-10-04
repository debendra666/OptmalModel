#Multiple Regression Using R
# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
#Building the Optimal Model By Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)#using whole the dataset to see the statically linkage
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend ,
               data = dataset)#using whole the dataset to see the statically linkage
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend ,
               data = dataset)#using whole the dataset to see the statically linkage
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)#using whole the dataset to see the statically linkage
summary(regressor)#Markting.Spend close to P-value of 6% but we remove it 
y_pred = predict(regressor,newdata = test_set)
y_pred


