
library(e1071)   # misc functions in stat, prob
library(Rcpp)    # R and C++ integration
library(caret)    # Classfication And REgression Training
library(pROC)

df <- read.csv("/Users/ryancoughlin/Documents/nfl-win-predictions/cleaned_535_project_data.csv")

df <- df[-c(1, 2, 3, 4, 11, 12, 13, 21, 22)]
# partition data
set.seed(2)   # random sample can be reproduced by setting a value  for seed
train.index <- sample(c(1:dim(df)[1]), dim(df)[1]*0.6)  
train_df <- df[train.index,]
validate_df <- df[-train.index,]

train_df <- train_df[, -c(1)]
validate_df <- validate_df[, -c(1)]

train_df <- train_df %>% mutate_all(~(scale(.) %>% as.vector))
validate_df <- validate_df %>% mutate_all(~(scale(.) %>% as.vector))

train_df$winning_record <- ifelse(train_df$wins > train_df$losses, 1, 0)
validate_df$winning_record <- ifelse(validate_df$wins > validate_df$losses, 1, 0)


# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression. (link = "logit")

logit.reg <- glm(winning_record ~ ., data = train_df, family = "binomial") 
options(scipen=999)  # avoid e notation
summary(glm(winning_record ~ ., data = train_df, family = "binomial") )


# binomial(link = "logit")
# gaussian(link = "identity")
# Gamma(link = "inverse")
# inverse.gaussian(link = "1/mu^2")
# poisson(link = "log")
# quasi(link = "identity", variance = "constant")
# quasibinomial(link = "logit")
# quasipoisson(link = "log")

# with interaction term

# logit.reg <- glm(Personal.Loan ~ Age+Experience+ Family +Age:Family, data = train.df, family = "binomial") 
# summary(logit.reg)

# specification    x1*x2 is the same as x1+x2+x1:x2
# logit.reg <- glm(Personal.Loan ~ Experience +Age*Family, data = train.df, family = "binomial") 
# summary(logit.reg)



# some libraries



#To assess the relative importance of individual predictors in the model, 
#we can also look at the absolute value of the t-statistic for 
#each model parameter. This technique is utilized by the varImp function 
#in the caret package

#varImp(logit.reg)
#plot( varImp(logit.reg) )

names(logit.reg)  # the names available in logit.reg, can be referred to by "logit.reg$"

# evaluate
# predict(logit.reg, validate_df) assumes a linear regression prediction
# adding type = "response" means log(odds) is linear regression, hence
# the prob follows a logistic regression model

pred <- predict(logit.reg, validate_df, type = "response")
pred_class <- as.factor(ifelse(pred > 0.5, 1, 0))
# the same as
# pred1 <-  predict(logit.reg, validate_df[,-8], type = "response") 

#install.packages("pastecs")
library(pastecs)
stat.desc(pred)


#confusion Matrix
c.mat <- table(ifelse(pred > 0.5, 1, 0), validate_df[,18])    #2 way table: row by column
c.mat
sum(diag(c.mat))/sum(c.mat) # this gives accuracy 
sensitivity(c.mat) # sensitivity
specificity(c.mat) # specificity
c.mat[1]# TN
c.mat[2] # FN
c.mat[3] # FP
c.mat[4] # TP
(c.mat[2] + c.mat[3])/sum(c.mat)# misclassification error
roc_obj <- roc(validate_df$winning_record, pred)
auc(roc_obj)

#training metrics
c.mat <- table(ifelse(pred > 0.5, 1, 0), train_df[,18])    #2 way table: row by column
c.mat
sum(diag(c.mat))/sum(c.mat) # this gives accuracy 
sensitivity(c.mat) # sensitivity
specificity(c.mat) # specificity
c.mat[1]# TN
c.mat[2] # FN
c.mat[3] # FP
c.mat[4] # TP
(c.mat[2] + c.mat[3])/sum(c.mat)# misclassification error
roc_obj <- roc(validate_df$winning_record, pred)
auc(roc_obj)



# number of 0 and 1 in validate_df    #one way table
table(validate_df[,18])

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, validate_df[, -18], type = "response")

# first 5 actual and predicted records
data.frame(actual = validate_df$winning_record[1:5], predicted = logit.reg.pred[1:5])
# same as
# data.frame(actual = validate_df$Personal.Loan[1:5], predicted = pred[1:5])

library(gains)


# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, validate_df[, -18], type = "response")

gain <- gains(validate_df$winning_record, logit.reg.pred, groups=10)
class(gain)
names(gain)

data.frame(c(0,gain$cume.pct.of.total*sum(validate_df$winning_record)) ,
           c(0,gain$cume.obs) )

data.frame( c(0,sum(validate_df$winning_record)) , c(0, dim(validate_df)[1]) )

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(validate_df$winning_record))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(validate_df$winning_record))~c(0, dim(validate_df)[1]), lty=2)

# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(validate_df$winning_record)

gain$mean.resp
gain$mean.resp*200
mean(validate_df$winning_record)
heights


midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")

# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)


#-- variable selection

library(MASS)

logit.reg <- glm(winning_record ~ ., data = na.omit(train_df), family = "binomial") 


step<-stepAIC(logit.reg, direction='backward', trace=FALSE)  #forward
step$anova


logit.reg <- glm(winning_record ~ ., data = na.omit(train_df), family = "binomial") 


