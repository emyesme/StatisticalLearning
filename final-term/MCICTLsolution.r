
# reading contents of csv file
train <- read.csv("/home/emily/Desktop/StatisticalLearning/final-project/MCICTLtrain.csv")

#ids
id <- train[1]

#labels
# from AD, CTL to 0 1
levels(train$Label) <- c(1,0)
label <- train$Label

#features
features <- train[2:594]

# preprocessing -> scaling features
features_prepro <- as.data.frame(scale(features))

# preprocessing -> outliers
library(ggplot2)

# ggplot(stack(features_prepro), aes(x = ind, y = values)) +
#  geom_boxplot()
# decide to not remove them
# preprocessing -> checking correlation

#https://stackoverflow.com/questions/18275639/remove-highly-correlated-variables
# removing highly correlated variables
tmp <- cor(features_prepro)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0

nocorr_features <- features_prepro[, !apply(tmp, 2, function(x) any( x>=0.7 & x<=0.99 , na.rm = TRUE))]

ncol(nocorr_features) #37
nocorr_features$label <- train$Label

# when we have a large number of predictors in the model
# there will generally be many that have little or no effect on the response

# VARIABLE SELECTION

# subset selection
# identify a subset of the p predictors that we believe
# to be related to the response

# here we perform forward and backward subset selection
# and the predictors will be reduce to the union of the results

# first check if there is nan values
sum(is.na(nocorr_features))
# no missing values

# best subset selection library
library(leaps)

# REGSUBSET FORWARD
regfit.fwd = regsubsets( label ~ ., data = nocorr_features, nvmax = 300, method = "forward")
regfwd.summary <- summary(regfit.fwd)

# get models features with base on best adjr2, cp, bic
fwd.max.adjr2 <- which.max(regfwd.summary$adjr2)
model.fwd.adjr2 <- summary(regfit.fwd)$which[fwd.max.adjr2,-1]

fwd.min.cp <- which.min(regfwd.summary$cp)
model.fwd.cp <- summary(regfit.fwd)$which[fwd.min.cp,-1]

fwd.min.bic <- which.min(regfwd.summary$bic)
model.fwd.bic <- summary(regfit.fwd)$which[fwd.min.bic,-1]

# Get model predictors from the previous model extractions
predictors.fwd.adjr2 <- list(names(which(model.fwd.adjr2 == TRUE)))
predictors.fwd.cp <- list(names(which(model.fwd.cp == TRUE)))
predictors.fwd.bic <- list(names(which(model.fwd.bic == TRUE)))

# get all the columns names from the 3 previous lists
predictors.fwd <- c(predictors.fwd.adjr2[[1]], predictors.fwd.cp[[1]], predictors.fwd.bic[[1]])
predictors.fwd <- unique(predictors.fwd)
length(predictors.fwd)# 18

# REGSUBSET BACKWARD
regfit.bwd = regsubsets(label~., data = nocorr_features, nvmax = 300, method = "backward")
regbwd.summary <- summary(regfit.bwd)

# plotting rss,adjr2,cp,bic to decide model

# par(mfrow = c (2 , 2))
# plot(regbwd.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l" )
# points (minrss, regbwd.summary$rss[minrss], col="red" , cex=2 , pch=20)

# plot(regbwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l" )
# points (maxadjr2, regbwd.summary$adjr2[maxadjr2], col="red" , cex=2 , pch=20)

# plot(regbwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l" )
# points (mincp, regbwd.summary$cp[mincp], col="red" , cex=2 , pch=20)

# plot(regbwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l" )
# points (minbic, regbwd.summary$bic[minbic], col="red" , cex=2 , pch=20)

# get models features with base on best adjr2, cp, bic
bwd.maxadjr2 <- which.max(regbwd.summary$adjr2)
model.bwd.adjr2 <- summary(regfit.bwd)$which[bwd.maxadjr2,-1]

bwd.mincp <- which.min(regbwd.summary$cp)
model.bwd.cp <- summary(regfit.bwd)$which[bwd.mincp,-1]

bwd.minbic <- which.min(regbwd.summary$bic)
model.bwd.bic <- summary(regfit.bwd)$which[bwd.minbic,-1]

# Get outcome variable+
#form <- as.formula(object$call[[2]])
#outcome <- all.vars(form)[1]

# Get model predictors from the previous model extractions
predictors.bwd.adjr2 <- list(names(which(model.bwd.adjr2 == TRUE)))
predictors.bwd.cp <- list(names(which(model.bwd.cp == TRUE)))
predictors.bwd.bic <- list(names(which(model.bwd.bic == TRUE)))

# get all the columns names from the 3 previous lists
predictors.bwd <- c(predictors.bwd.adjr2[[1]], predictors.bwd.cp[[1]], predictors.bwd.bic[[1]])
predictors.bwd <- unique(predictors.bwd)
length(predictors.bwd)# 16

# saving predictors of best performing at backward and forward best subset selection
predictors <- c(predictors.fwd, predictors.bwd)
predictors <- unique(predictors)
length(predictors)

# REGULARIZATION
# ridge regression, lasso, elastic net

# get columns result of the regsubset
train_regsubset <- nocorr_features[, c(predictors)]
train_regsubset$label <- train$Label

# generating train and validation

# sample_size <- floor(0.7 * nrow(train_regsubset))

# ids <- sample(seq_len(nrow(train_regsubset)), size = sample_size)

# train_train_all <- train_regsubset[ ids, ]
# train_val_all <- train_regsubset[ -ids, ]

x <- model.matrix(label ~ ., train_regsubset)[, -1]
y <- as.matrix(train_regsubset$label)


train_train <- sample(1:nrow(x), nrow(x)/2)
train_val <- ( -train_train)
y.val <- as.numeric(y[train_val])

# library
library(glmnet)


# lasso with cross validation

lasso.cv <- cv.glmnet(x[train_train, ], y[train_train], alpha = 1, family="binomial")

lasso.mod <-glmnet(x[train_train, ], y[train_train], lambda = lasso.cv$lambda.min, alpha=1, family = "binomial")

lasso.pred <- predict(lasso.mod, s = lasso.cv$lambda.min, newx = x[train_val, ])

lasso.error <- mean((lasso.pred  - y.val)^2)
lasso.error

# discard features? yes
lasso.coef <- predict(lasso.mod, s = lasso.cv$lambda.min, type="coefficients")[1:19, ]
lasso.coef <- lasso.coef[ lasso.coef != 0]
length(lasso.coef)
# lasso.predictors <- names(lasso.coef[-1])

# elasticnet

library(caret)
library(dplyr)

elastic.cv <- train(
  label ~. , data = train_regsubset[train_train, ], method = "glmnet",
  trControl = trainControl("cv", number = 20),
  tuneLength = 10
)
# The final values used for the model were alpha = 0.5 and lambda = 0.0002776493.
elastic.model <- glmnet(x, y, alpha = elastic.cv$bestTune$alpha, lambda=elastic.cv$bestTune$lambda)

elastic.pred <- predict(elastic.model, s = elastic.cv$bestTune$lambda, newx = x[train_val, ])

elastic.error <- mean((elastic.pred  - y.val)^2)
elastic.error # best error for elastic

# discard features?
elastic.coef <- predict(elastic.model, s = elastic.cv$bestTune$lambda, type = "coefficients")[1:19, ]
elastic.coef <- elastic.coef[ elastic.coef != 0]
length(elastic.coef) #19
elastic.predictors <- names(elastic.coef[-1])
length(elastic.predictors)

library(pROC)

# final selected features
train_features <- train_regsubset[, c(elastic.predictors)]
train_features$label <- make.names(as.factor(train$Label))

# train and validation set to test metrics after applying models

sample_size <- floor(0.7 * nrow(train_features))
ids <- sample(seq_len(nrow(train_features)), size = sample_size)

train_train_all <- train_features[ ids, ]
train_val_all <- train_features[ -ids, ]

# seeds
seeds <- vector(mode = "list", length = 11)
for(i in 1:11) seeds[[i]] <- sample.int(1000, 22)

# train control
ctrl = trainControl( method = "cv",
                     number = 10,
                     seeds = seeds,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

library(mltools)

fit_models <- function(training, validation, trainControl) {
  names <- c("model", "auc","auc test", "mcc")
  metrics <- data.frame(matrix(ncol=4, nrow=0))
  colnames(metrics) <- names
  features_val <- validation[, names(validation) != "label"] 
  
  # logistic regression
  glm.model <- train( label ~ . ,
                      data = training,
                      method = "glm",
                      metric = "ROC",
                      trControl = trainControl )
  
  glm.pred <- predict(glm.model, newdata =features_val, type = "raw")
  
  row <- data.frame("glm",
                    glm.model$result$ROC,
                    auc(validation$label, as.numeric(glm.pred)),
                    mcc(glm.pred, as.factor(validation$label)))
  
  names(row) <- c("model", "auc", "auc test", "mcc")
  metrics <- rbind(metrics, row)
  
  # linear discriminant analysis
  library(MASS)
  lda.model <- train( label ~ . ,
                      data = training,
                      method = "lda",
                      metric = "ROC",
                      trControl = ctrl )
  
  
  lda.pred <- predict(lda.model, newdata =features_val, type = "raw")
  
  row <- data.frame("lda",
                    max(lda.model$result$ROC),
                    auc(validation$label, as.numeric(lda.pred)),
                    mcc(lda.pred, as.factor(validation$label)))
  
  names(row) <- c("model", "auc", "auc test", "mcc")
  metrics <- rbind(metrics, row)
  
  #
  
  lda2.model <- train( label ~ . ,
                       data = training,
                       method = "lda2",
                       metric = "ROC",
                       trControl = ctrl )
  
  lda2.pred <- predict(lda2.model, newdata =features_val, type = "raw")
  
  row <- data.frame("lda2",
                    lda2.model$result$ROC,
                    auc(validation$label, as.numeric(lda2.pred)),
                    mcc(lda2.pred, as.factor(validation$label)))
  
  names(row) <- c("model", "auc", "auc test", "mcc")
  metrics <- rbind(metrics, row)
  
  # logistic regression
  knn.model <- train( label ~ . ,
                      data = training,
                      method = "knn",
                      metric = "ROC",
                      trControl = ctrl )
  
  knn.pred <- predict(knn.model, newdata =features_val, type = "raw")
  
  row <- data.frame("knn",
                    max(knn.model$result$ROC),
                    auc(validation$label, as.numeric(knn.pred)),
                    mcc(lda2.pred, as.factor(validation$label)))
  
  names(row) <- c("model", "auc", "auc test", "mcc")
  metrics <- rbind(metrics, row)
  
  #qda
  
  qda.model <- train( label ~ . ,
                      data = training,
                      method = "qda",
                      metric = "ROC",
                      trControl = ctrl )
  
  qda.pred <- predict(qda.model, newdata =features_val, type = "raw")
  
  row <- data.frame("qda",
                    max(qda.model$result$ROC),
                    auc(validation$label, as.numeric(qda.pred)),
                    mcc(qda.pred, as.factor(validation$label)))
  
  names(row) <- c("model", "auc", "auc test", "mcc")
  metrics <- rbind(metrics, row)
  
  # logreg
  
  library(caTools)
  logreg.model <- train( label ~ . ,
                         data = training,
                         method = "LogitBoost",
                         metric = "ROC",
                         trControl = ctrl )
  
  logreg.pred <- predict(logreg.model, newdata =features_val, type = "raw")
  
  row <- data.frame("logregb",
                    max(logreg.model$result$ROC),
                    auc(validation$label, as.numeric(logreg.pred)),
                    mcc(logreg.pred, as.factor(validation$label)))
  
  names(row) <- c("model", "auc", "auc test", "mcc")
  metrics <- rbind(metrics, row)
  
  
  # svm
  svm.model <- train( label ~ . ,
                      data = training,
                      method = "svmLinear2",
                      metric = "ROC",
                      trControl = ctrl )
  
  svm.pred <- predict(svm.model, newdata =features_val, type = "raw")
  
  row <- data.frame("svm",
                    max(svm.model$result$ROC),
                    auc(validation$label, as.numeric(svm.pred)),
                    mcc(svm.pred, as.factor(validation$label)))
  
  names(row) <- c("model", "auc", "auc test", "mcc")
  metrics <- rbind(metrics, row)
  
  svmweights.model <- train( label ~ . ,
                             data = training,
                             method = "svmLinearWeights",
                             metric = "ROC",
                             trControl = ctrl )
  
  svmweights.pred <- predict(svmweights.model, newdata =features_val, type = "raw")
  
  row <- data.frame("svmw",
                    max(svmweights.model$result$ROC),
                    auc(validation$label, as.numeric(svmweights.pred)),
                    mcc(svmweights.pred, as.factor(validation$label)))
  
  names(row) <- c("model", "auc", "auc test", "mcc")
  metrics <- rbind(metrics, row)
  
  # random forest
  forest.model <- train( label ~ . ,
                         data = training,
                         method = "rf",
                         metric = "ROC",
                         trControl = ctrl )
  
  forest.pred <- predict(forest.model, newdata =features_val, type = "raw")
  
  row <- data.frame("rf",
                    max(forest.model$result$ROC),
                    auc(validation$label, as.numeric(forest.pred)),
                    mcc(forest.pred, as.factor(validation$label)))
  
  names(row) <- c("model", "auc", "auc test", "mcc")
  metrics <- rbind(metrics, row)
  
  # mda
  
  mda.model <- train( label ~ . ,
                      data = training,
                      method = "mda",
                      metric = "ROC",
                      trControl = ctrl )
  
  mda.pred <- predict(mda.model, newdata =features_val, type = "raw")
  
  row <- data.frame("mda",
                    max(mda.model$result$ROC),
                    auc(validation$label, as.numeric(mda.pred)),
                    mcc(mda.pred, as.factor(validation$label)))
  
  names(row) <- c("model", "auc", "auc test", "mcc")
  metrics <- rbind(metrics, row)
  
  return (metrics)
}

metrics <- fit_models(train_train_all, train_val_all, ctrl)
metrics



