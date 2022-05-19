'''
  Student: Emily E Carvajal C
  MIDTERM 
    * The challenge consists in dealing with a regression problem (quantitative) using a parametric and a nonparametric approach.
    
    parametric approach -> assumption that f is linear 
      Disadvantages: 
        - model usually does not match unknown form of f
        - if choosen model far from real estimation is poor
          - more flexibles models address this poor choosing but 
            more flexibility require more parameters
            this can lead to--- overfitting
            
    non-parametric approach -> no assumptions over f 
                               choose a model as close as possible to the data without been to wiggly
      advantage: - potential to accurately fit the data
      disadvantage: - very large number of observations required to accurately estimate for f
    
    In particular,  the former (parametric) will consist in estimating a linear regression model for the train data contained in the train_ch.csv.
    The latter (non-parametric) will consist in using KNN (properly tuned) to predict the response values for the test observations (please use FNN library).
    
    * Your task is to use the  training data to build your models and to test them on the test_ch.csv observations.
    * You will receive a training set (train_ch.csv) of 1000 observations and a test set (test_ch.csv) of 100, with nine ---independent variables---.
    * Your submission will consist of a zip file StudentRegistrationNumber_FamilyName.zip containing the following files:
      1. A RData file containing: the output of lm() and FNN::knn.reg() in fit_lm and fit_knn, and two numeric arrays pred_knn and pred_lm, one for each method, containing the 100 predicted values for the test data.
      2. A presentation in PDF with up to 6 pages, in which you describe your analysis, how you obtained and evaluated the model.
      3. The R macro named solution.R used to obtain the results.
'''

# reading contents of csv file
train <- read.csv("/home/emily/Desktop/StatisticalLearning/mid-term/train_ch.csv")

train <- train[2:11]

# PARAMETRIC APPROACH

# linear regression

# pre-processing so each predictor contribute equally
train_standardize <- as.data.frame(scale(train[1:10]))
train_standardize["Y"] <- list(train$Y)

# forward selection

# fit linear model with a formula and data
# formula-> typical model form response ~ terms
linear_model <- lm( Y ~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9, data=train_standardize)
summary(linear_model)

# plot model
par(mfrow=c(2,2))
plot(linear_model)
par(mfrow=c(1,1))

# Assumptions in a linear regression model
# - linearity - checked
# - homoscedasticity - checked
# - independence - checked
# - normality - checked

# potential problems:
# - non-linearity of the response-predictor relationship - present
# - correlation of error terms
# - non-constant variance of error terms
# - outliers
# - high-leverage points
# - collinearity


# Evaluation methods
# - residual standard error (RSE):
#     *estimate of standard deviation of epsilon (irreducible error)
#     *average amount the response will deviate form the true regression line
#     *measure of the lack of fit of the model to the data
# interpretation: the tinnier the better
#                 big value indicate model does not fit the data well
# - R-squared:
#     *measures the lack of fit of the regression to the data
#     *measure in the units of Y
#     *proportion of variance explained
# interpretation: value between 0-1, the closer to 1 the better
# - F-statistics:
#     *h0 (null- hypothesis) there is no relationship between X and Y
#     *ha (alternative hypothesis)  there is some relationship between X and Y
#     *formulas to compute with RSS and TSS
# interpretation:
#     *if h0 is true -> no relationship -> f-statistics close to 1
#     *if ha is true -> relationship -> we expect f-statistics > 1 
# - p-value:
#     *same h0 and ha different formulas 
# interpretation:
#     *small p-value mean there is an association between the predictor and the response -> P < 0.001

# from summary:
#   most predictors have no significant p-values
#   there must be a correlation between predictors

# CHECKING LINEAR REGRESSION ASSUMPTIONS

# INDEPENDENCE OF VARIABLES - CORRELATION

# checking correlation between predictors
corr <- cor(train[1:9])# just predictors
which( corr>0.1 & corr<1, arr.ind=TRUE)

# from the correlation matrix we can say that the following pairs of predictors are related:
#   - v1 and v5
#   - v1 and v7
#   - v5 and v7
# we will remove the variables with the non-significant p-values (v1 and v7)

linear_model <- lm( Y ~ v2 + v3 + v4 + v5 + v6 + v8 + v9, data=train_standardize)
summary(linear_model)

# CHECKING NORMALITY

# checking whether your dependent variables follow a normal distribution

hv2 <- hist(train_standardize$v2, plot=FALSE)
hv3 <- hist(train_standardize$v3, plot=FALSE)
hv4 <- hist(train_standardize$v4, plot=FALSE)
hv5 <- hist(train_standardize$v5, plot=FALSE)
hv6 <- hist(train_standardize$v6, plot=FALSE)
hv8 <- hist(train_standardize$v8, plot=FALSE)
hv9 <- hist(train_standardize$v9, plot=FALSE)

xlimit <- range(c(hv2$breaks, hv3$breaks, hv4$breaks, hv5$breaks, hv6$breaks, hv8$breaks, hv9$breaks))
ylimit <- range(c(hv2$counts, hv3$counts, hv4$counts, hv5$counts, hv6$counts, hv8$counts, hv9$counts))

# one by one
plot(hv2, col='lightblue', border='white', xlim=xlimit, ylim=ylimit)
# the predictor v2 do not follow a normal distribution

plot(hv3, col='antiquewhite3', border='white', add=T)
# the predictor v3 do not follow a normal distribution

plot(hv4, col='darkseagreen2', border='white', add=T)
# the predictor v4 do not follow a normal distribution

plot(hv5, col='aliceblue', border='white', add=T)
# the predictor v5 do not follow a normal distribution

plot(hv6, col='lemonchiffon3', border='white', add=T)
# the predictor v6 do not follow a normal distribution

plot(hv8, col='honeydew3', border='white', add=T)
# the predictor v8 follow a normal distribution

plot(hv9, col='lightsteelblue', border='white', add=T)
# the predictor v9 follow a normal distribution

# library
library('ggplot2')
library('dplyr')
library('hrbrthemes')

# Build dataset with different distributions
data <- data.frame(
  type = c( rep("v2", 1000), rep("v3", 1000), rep("v4", 1000), rep("v5", 1000), rep("v6", 1000), rep("v8", 1000), rep("v9", 1000)),
  value = c( train_standardize$v2, train_standardize$v3, train_standardize$v4, train_standardize$v5, train_standardize$v6, train_standardize$v8, train_standardize$v9)
)

# Represent it
p <- data %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +#62875af19090
  scale_fill_manual(values=c("#69b3a2", "#404080", "#cfb3ef", "#cb878f",  "#dedc87", "#f1e986", "#ff7070", "#62875a" )) +
  theme_ipsum() +
  labs(fill="")

# for the slides plot 
p

# We will remove the ones that have non-significant p-value and do not follow 
# a normal distribution as checked before, these are:
# v2, v4, v6

linear_model <- lm( Y ~ v3 + v5 + v8 + v9, data=train_standardize)
summary(linear_model)

# LINEARITY

# the relationship between the independent and dependent
# variable must be linear. Plot to see if the distribution
# of data points could be described with a straight line

# Libraries
library('tidyverse')
library('hrbrthemes')
library('viridis')
library('forcats')

par(mfrow=c(2,2))
ggplot(train_standardize, aes(x=train_standardize$v3, y=train_standardize$Y)) + 
  geom_point()
ggplot(train_standardize, aes(x=train_standardize$v5, y=train_standardize$Y)) + 
  geom_point()
ggplot(train_standardize, aes(x=train_standardize$v8, y=train_standardize$Y)) + 
  geom_point()
ggplot(train_standardize, aes(x=train_standardize$v9, y=train_standardize$Y)) + 
  geom_point()

# We will remove the ones that have non-significant p-value and
# can not be described with a straight line, these are:
# - None

# model taking into account the previous criteria
linear_model <- lm(Y ~ v3 + v5 + v8 + v9, data=train_standardize)
summary(linear_model)

# HOMOSCEDASTICITY - HOMOGENEITY OF VARIANCE

# prediction error does not change significantly
# over the range of prediction of the model

par(mfrow=c(2,2))
plot(linear_model)
par(mfrow=c(1,1))

# Problem recognized: non-linearity
# In the residual vs Fitted plot
# there is no visible randomness in fitted versus residual values in the plot
# a non-linear model would be better to describe the data
# moreover the residual describe a quadratic behavior
# the relationship can be model as a polynomial regression 

# we try the three predictors with a quadratic term
# the one that give us significant p-value is v3 as follow

linear_model <- lm(Y ~ v3 + I(v3^2) + v5 + v8 + v9, data=train_standardize)

summary(linear_model)

par(mfrow=c(2,2))
plot(linear_model)
par(mfrow=c(1,1))

# now the residual vs fitted plot have more randomness in the data
# which tell us the model fit more the data

# Given that now all the assumptions fulfill for the predictors
# From the summary of the model we can notice the p-values of the v8 and v9 predictor
# are not significant and can be removed from the model because there is no relationship

linear_model <- lm(Y ~ v3 + I(v3^2) + v5, data=train_standardize)
summary(linear_model)

par(mfrow=c(2,2))
plot(linear_model)
par(mfrow=c(1,1))

# final model, linear_model

# NONPARAMETRIC APPROACH

# k nearest neighbors

# import packages
#install.packages('FNN')
library(FNN)

# seed
set.seed(42)

# preprocessing data
train_standard <- as.data.frame(scale(train[1:9]))

# 
# knn with different values of k to find the best
# also trying to avoid the curse of dimensionality
# we are going to check the knn with different combinations
# of predictors 

# function to interpret the array of combinations
functionComb <- function(matrix){
  index <- 1
  result <- c(1:9)*0
  results <- data.frame(matrix(ncol=9, nrow=0))
  for (i in 1:nrow(matrix)){
    for (j in 1:length(matrix[i,])) {
      if (matrix[i,j]==1){
        result[index] <- j
        index <- index + 1
      }
    } 
    results[i, ] <- c(result)
    result <- c(1:9)*0
    index <- 1
  }
  return(results)
}

# quantity of predictors
p <- 9
list_comb <- rep(list(0:1), p)
combination_01 <- expand.grid(list_comb)[-1, ]
combinations <- functionComb(combination_01)
combinations_list <- split(combinations, seq(nrow(combinations)))

# empty list for values of mse
mse = c()

# the maximum value of k will be the one recommended
# for n observations -> sqrt(n)/2
k_values = c(1:15)

#KNN

# data frame to save the k value, mse and combination of predictors
compare <- data.frame(matrix(ncol=3, nrow=0))
names <- c("vars", "k", "mse")
colnames(compare) <- names

zero <- c(0)

# for each combination of predictors do the knn for values of k between 1 and sqrt(n)/2 = 15
# also as no test set is given cross validations leave-one-out of the
# training set is perform
for (combination in combinations_list){
  for (i in 1:length(k_values)){
    combination <- combination [! combination %in% zero]
    combination <- unlist(combination, use.names = FALSE)
    # run the knn
    knn_pred = knn.reg(train = train_standard[, combination], test = NULL, y = train$Y, k=k_values[i])
    # save the mse
    row <- data.frame(paste(unlist(combination), collapse=''), k_values[i], mean((knn_pred$residuals)^2))
    names(row) <- c("vars", "k", "mse")
    compare <- rbind(compare, row)
  }
}

# get the minimum error from the returned table
minimum <- compare[which(compare$mse == min(compare$mse)), ]
minimum

# expected best model
knn_model <- knn.reg(train = train_standard$v3,  test = NULL, y=train$Y, k=5)
MSqEr <- mean((knn_model$residuals)^2)
MSqEr

# getting the data

test_data =  read.csv("/home/emily/Desktop/StatisticalLearning/mid-term/test_ch.csv")

test_data = test_data[,2:10]

# pre-processing so each predictor contribute equally
standardize <- function(x, mean, stdv) {
  return((x - mean) / stdv )
}

test_standardize <- as.data.frame(test_data[, c('v3','v5')])

# as we are just going to use this two predictors, we just standardize them
test_standardize["v3"] <- list(standardize(test_data$v3, mean(train$v3), sd(train$v3)))
test_standardize["v5"] <- list(standardize(test_data$v5, mean(train$v5), sd(train$v5)))
test_standardize["Y"] <- list(test_data$Y)


fit_lm <- lm(Y ~ v3 + I(v3^2) + v5, data=train_standardize)
pred_lm <- predict(fit_lm, test_standardize)

fit_knn <- knn.reg(train = as.data.frame(train_standardize$v3), test=as.data.frame(test_standardize$v3), y = train_standardize$Y, k = 5)
pred_knn <- fit_knn$pred

save(fit_lm, pred_lm, fit_knn, pred_knn, file="FinalData.RData")



