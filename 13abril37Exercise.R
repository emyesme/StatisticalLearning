'''
3.7 exercises

4. without knowing about the data is difficult behave about the rss.

a. in this case we would expect the least square line is close to the
true regression line. and the linear rss will be lower that the cubic rss.

the model is more flexible with the cubic regression. fitting the noise
in the data with those extra components, leading to overfitting making
the rss lower for the training not for the test

polinomial lower rss because of high flexibility

b. again the polinomial lower rss because its flexibility for the 
training data

we dont have enough information for the test data
trade between variance and bias is the topic.
'''

# 10. #
library(ISLR2)

head(Carseats)

# convert the categorical columns to numericals
str(Carseats)
Carseats$Urban<-as.numeric(Carseats$Urban)
Carseats$US<-as.numeric(Carseats$US)

# fit linear model with a formula and data
# formula-> typical model form response ~ terms
fit <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(lm)

'''
coef->price (tinny value -0.054459) if the price increase by 1000.0 usd
and the other predictors are constant then
the sales will decrease by 54.459 usd

coef->urban as its not significant the p-value. there is no relevant
information

coef->(1.200573) a store in the use sell 1200 more carseats
in average than a store in other that is abroad

'''
#c. write out the model in equation form
# handle the qualitative variables properly

# Sale = B_0 * Price +B_1 * Urban + B_2 * US + epsilon
# sale = (-0.054459) * Price + (-0.021916)*Urban + (1.200573)*US + epsilon

#d. because the p-value of the urban predictor is not relevant
# so we can omite it

#e. 
fit1<- lm(Sales ~ Price + US, data=Carseats)
summary(fit1)

#f. how well do the models in a and e fit the data

# to compare models
anova(fit, fit1)

#g. obtain a 95% confidence intervals for the coefficient(s)
confint(fit1)

#h. 
plot(predict(fit1), rstudent(fit1))


par(mfrow = c(2,2))
plot(fit1)

hatvalues(fit1)[(order(hatvalues(fit1), decreasing=T))][1]
'''
order the high leverage values, to know the highest and extract it 
'''

# 14. (pag 137/612)

#a. perform

set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100)/ 10
#creating a linear model in which y is a function of x1 and x2
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

# write out the form of the linear model.
# What are the regression coefficients?

# coefficients are 2 (intercept), 2, 0.3
# y=2 + 2 * x_1 + 0.3 * x_2 + epsilon

#b. correlation between x1 and x2
cor(x1, x2)

plot(x1, x2)

# variables seem highly correlated

#c. 

# squares regression
lm.fit <- lm(y ~ x1 + x2)
summary(lm.fit)

# what are b_o, b_1 and b_2, only b_0 (intercept) 
# is close to the real value
# x_1 is still acceptable to be b_1 but x2 prediction 
# have high p-value therefore is no significant

#d.
lm.fit2 <- lm(y ~ x1)
summary(lm.fit2)

# null hypothesis rejected because the p-value is really low
# good x1 value

# e.
lm.fit3 <- lm(y ~ x2)
summary(lm.fit3)

# null hypothesis rejected because the p-valueis really low
# good x2 value

# f. 
# collinearity problems.
# if the two predictors are correlated. collinearity
# its going to be difficult to know which affect the model
# the effect of one can be masked by the other

# no, the results do not contradict each other
# as the predictors x1 and x2 
#  


x1 <- c(x1, 0.1)
x2 <- c(x1, 0.8)
y <- c(y,6)

lm.fit4 <- lm(y ~ x1+ x2)
summary(lm.fit4)

lm.fit5 <- lm(y ~ x1)
summary(lm.fit5)


lm.fit6 <- lm(y ~ x2)
summary(lm.fit6)

# 

par(mfrow=c(2,2))
plot(lm.fit4)
plot(lm.fit5)
plot(lm.fit6)

