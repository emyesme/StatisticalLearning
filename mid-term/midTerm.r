'''
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
test <- read.csv("/home/emily/Desktop/StatisticalLearning/mid-term/test_ch.csv")

# PARAMETRIC APPROACH

# linear regression
# can have more that one predictor

# we want to normalize to make each predictor contribute equally to the analysis
train_standardize <- as.data.frame(scale(train[2:10]))
train_standardize["Y"] <- list(train$Y)
train_standardize["X"] <- list(train$Y)

# fit linear model with a formula and data
# formula-> typical model form response ~ terms
linear_model <- lm( Y ~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9, data=train_standardize)
summary(linear_model)

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
#   most predictors have no significant p-values therefore those predictors are not significant
#   must be correlation between predictors

# CHECKING LINEAR REGRESSION ASSUMPTIONS

# INDEPENDENCE OF OBSERVATIONS - CORRELATION

# checking correlation between predictors
cor(train[2:10])# just predictors

# from the correlation matrix we can say that the following pairs of predictors are related:
#   - v1 and v5
#   - v1 and v7
#   - v5 and v7

# CHECKING NORMALITY

# checking whether your dependent variables follow a normal distribution

hist(train$v1)
# the predictor v1 do not follow a normal distribution

hist(train$v2)
# the predictor v2 do not follow a normal distribution

hist(train$v3)
# the predictor v3 do not follow a normal distribution

hist(train$v4)
# the predictor v4 do not follow a normal distribution

hist(train$v5)
# the predictor v5 do not follow a normal distribution

hist(train$v6)
# the predictor v6 do not follow a normal distribution

hist(train$v7)
# the predictor v7 do not follow a normal distribution

hist(train$v8)
# the predictor v8 follow a normal distribution

hist(train$v9)
# the predictor v9 follow a normal distribution


# LINEARITY

# the relationship between the independent and dependent
# variable must be linear. Plot to see if the distribution
# of data points could be described with a straight line

plot(Y ~ v1, data = train)
# a straight line can not describe the data points for Y ~ v1

plot(Y ~ v2, data = train)
# a straight line can not describe the data points for Y ~ v2

plot(Y ~ v3, data = train)#
# a straight line can describe the data points for Y ~ v3

plot(Y ~ v4, data = train)
# a straight line can not describe the data points for Y ~ v4

plot(Y ~ v5, data = train)
# a straight line can not describe the data points for Y ~ v5

plot(Y ~ v6, data = train)
# a straight line can not describe the data points for Y ~ v6

plot(Y ~ v7, data = train)
# a straight line can not describe the data points for Y ~ v7

plot(Y ~ v8, data = train)#
# a straight line can describe the data points for Y ~ v8

plot(Y ~ v9, data = train)#
# a straight line can describe the data points for Y ~ v9


# model taking into account the previous criteria
linear_model_cor <- lm(Y ~ v3 + v8 + v9, data=train_standardize)
summary(linear_model_cor)


# HOMOSCEDASTICITY - HOMOGENEITY OF VARIANCE

# prediction error does not change significantly
# over the range of prediction of the model

par(mfrow=c(2,2))
plot(linear_model_cor)
par(mfrow=c(1,1))

# From the residual vs Fitted plot
# there is no visible randomness in fitted versus residual values in the plot
# a non-linear model would be better to describe the data
# moreover the residual describe a quadratic behavior

# we try the three predictors with a quadratic term
# the one that give us significant value is v3 as follow

linear_model_v3 <- lm(Y ~ v3 + I(v3^2) + v8 + v9, data=train_standardize)
# v8 and v9 have no significance in the p-value result
# so can be remove it as they are not associated with the Y as predictors
summary(linear_model_v3)

par(mfrow=c(2,2))
plot(linear_model_v3)
par(mfrow=c(1,1))

# now the residual vs fitted plot have more randomness in the data
# which tell us the model fit more the data


#to compare models

#analysis of variance
anova(linear_model, linear_model_cor)


# NONPARAMETRIC APPROACH

# k nearest neighbors

# import packages
#install.packages('FNN')
library(FNN)

knn_model <- knn.reg(train, y=train$Y, test=NULL, k=3, algorithm=c("kd_tree", "cover_tree", "brute"))

plot(train$Y, knn_model$pred, xlab="x", ylab=expression(hat(y)))
