rm(list=ls())
# install packages to get the hill function
install.packages('evir')
#install.packages('Robyn')
#library('Robyn')
library('evir')

install.packages('minpack.lm')
library(minpack.lm)

if(!require('Robyn')) {
  install.packages('Robyn')
  library('Robyn')
}

if(!require('EnvStats')) {
  install.packages('EnvStats')
  library('EnvStats')
}

#df = read.csv("C:/Users/tatel/OneDrive/Documents/python/attribution/simulated_data.csv")

df = read.csv("C:/Users/tatel/OneDrive/Documents/python/attribution/arima_dataSim_data.csv")

head(df)

################################################################################
# DATA PREP
# According to Bayesian Methods for Media Mix Modeling
# Data is normally aggregated on a weekly or monthly basis
# additionally, we need to make sure all values are positive

################################################################################
#head(df)
# Since values cannot be negative we use abs function

df$sales = abs(df$sales)
df$display_spend = abs(df$display_spend)
df$search_spend = abs(df$search_spend)
df$tv_spend = abs(df$tv_spend)



################################################################################
# HOLT WINTERS Experimentation 
# Explore decomposing the trend and using lagged features

################################################################################
?HoltWinters

head(df)

# flatten the data structure
b = as.vector(unlist(abs(df$sales)))


#head(temp_df)

temp_ts = ts(b, frequency=7)

plot(decompose(temp_ts))

holt = HoltWinters(temp_ts, alpha=NULL, beta=NULL, gamma=NULL, seasonal="additive")

names(holt)

plot(holt$fitted)
# view the sales trend
plot(holt$fitted[,3])

#head(holt$fitted)

plot(holt$fitted[,2])    # Level: strips out the trend and seasonality

level = holt$fitted[,2]

trend = holt$fitted[,3] # indicates trends in the time series data

plot(holt$fitted[,4])    # indicates the seasonality -- which isn't useful as we are only using a weeks worth of data


level = as.vector(level)
trend = as.vector(trend)

# NOTE: HoltWinters sacrifice a week of data to infer relationships
# arguments imply differing number of rows: 730, 723
# solution is to add 7 NAs ot the beginning

# Add multiple NAs to the beginning
level_na = c(rep(NA, 7), level) # Adds 7 NAs to account for sacrificed data
trend_na = c(rep(NA, 7), trend)

# join the level to our dataframe
df = cbind(df, level_na)

# join the trend to the dataset
df = cbind(df, trend_na)

#############################################################################

# create our synthetic date range ######

# define the start
date = as.Date("2021/08/04")

# define the length
len = nrow(df)
# generate range of dates
dates = seq(date, by="day", length.out = len)

df = cbind(dates, df)
head(df, n=20)
tail(df)
# add a column to show week number + year
df$week_num = strftime(df$dates, format="%Y-%V")

# aggregate the data by week
agg_df = aggregate(cbind(df$tv_spend, df$search_spend, df$display_spend, df$sales, df$level_na,
                         df$trend_na) ~ week_num,
          data=df, FUN=sum, na.rm=TRUE)

# rename our columns
colnames(agg_df)[2] = "tv_spend"
colnames(agg_df)[3] = "search_spend"
colnames(agg_df)[4] = "display_spend"
colnames(agg_df)[5] = "sales"
colnames(agg_df)[6] = "level_holt"
colnames(agg_df)[7] = "trend_holt"

head(agg_df)    # NOTE it drops the first week since it has NA rows

tail(agg_df)


################################################################################
# create lagged sales features
## We only want to include last week's sales into our model 
################################################################################
library(dplyr)    # import our package to lag the feature


agg_df$prior_level = lag(agg_df$level_holt, n=1)
agg_df$prior_trend = lag(agg_df$trend_holt, n=1)

# drop NA records
agg_df = na.omit(agg_df)
head(agg_df)

################################################################################
# analyze the shape of the time series
par(mfrow=c(2,3))
plot(df$media_1_spend, type="l")
plot(df$media_2_spend, type="l")
plot(df$media_3_spend, type="l")

# Our data is stationary
par(mfrow=c(2,3))
plot(df$sales, type="l", main="Sales")
plot(df$tv_spend, type="l", main="TV Spend")
plot(df$search_spend, type="l", main="Search Spend")
plot(df$display_spend, type="l", main="Display Spend")

#################################################################################



#par(mfrow=c(3,2))
          
#plot(df$media_1_spend, df$sales)
#plot(df$media_1_traffic, df$sales)
#plot(df$media_2_spend, df$sales)
#plot(df$media_2_traffic, df$sales)
#plot(df$media_3_spend, df$sales)
#plot(df$media_3_traffic, df$sales)


cor(agg_df$search_spend, agg_df$sales)

# hill function implementation
?hill

media_1.spend_hill = hill(df$media_1_spend)


names(res)

plot(res$y)


# Adstock function implementation

# Adstock with Geometric decay allows us to transform time series of media spend
# this allows us to account for any carry over effects

adstock_geometric(rep(100, 5), theta = 0.3)

adstock.media_1_spend = adstock_geometric(df$media_1_spend, theta=0.3)

names(adstock.media_1_spend)

plot(adstock.media_1_spend$x_decayed)


################################################################################
# combining the adstock + hill function to model decay and shape effects
################################################################################

hill(df$media_1_spend)

names(media_1.spend_hill)

plot(media_1.spend_hill$y)

# first apply the adstock transformation to time series of media spend

# then apply the shape transformation

adstock.hill = hill(adstock.media_1_spend$x_decayed)

names(adstock.hill)

plot(adstock.hill$y)

################################################################################
# Adstock function raw modeling

# general adstock model = S_t = alpha + Beta * A_t
# resource: https://medium.com/@kyliefu/implementation-of-the-advertising-adstock-theory-in-r-62c2cc4b82fd
################################################################################

# 1.) Geometric Simple Decay Method
# A_t = T_t + lambda* A_{t-1}

GeometricSimpleAdstock = function(advertising, lambda){
  adstock = as.numeric(stats::filter(advertising, lambda, method="recursive"))
  
  return(adstock)
  
  }

# calling the simple adstock function on media 1 spend
geoSimple.media_1_spend = GeometricSimpleAdstock(df$media_1_spend, lambda=0.5)

# 2.) Geometric Log Decay Model
# A_t = log*T_t + lambda * A_{t-1}

GeometricLogAdstock = function(advertising, lambda){
  
  adstock = as.numeric(stats::filter(log(advertising), lambda,
                                     method="recursive"))
  
  return(adstock)
  
}


geoLog.media_1_spend = GeometricLogAdstock(df$media_1_spend, lambda=0.5)


# 3.) Delayed Simple Decay Model

# we use matrix multiplication to accomplish this
"""
Return the advertising adstock using Delayed Decay Model.
---
Inputs:
advertising: A sequence.
N: Length of the advertising sequence.
lambda: Adstock decay rate.
theta: The delay of the peak effect.
L: The maximum duration of carryover effect.
---
Returns:
adstock: Advertising adstock
"""


DelayedSimpleAdstock = function(advertising, N, lambda, theta, L){
  
  weights = matrix(0, N, N)
  for(i in 1:N){
    for(j in 1:N){
      k = i - j
      if (k < L && k >= 0){
        weights[i, j] = lambda ** ((k - theta) ** 2)
      }
    }
  }
  adstock = as.numeric(weights %*% matrix(advertising))
  return (adstock)
}


# in this function, we generate a weight matrix W, and apply matrix multiplication to derive the output sequence.
# to illustrate, we run a toy example below:

#N = 8
#lambda = 0.9
#theta = 1
#L = 4
#weights = matrix(0, N, N)   # backbone of function
#for (i in 1:N){
#  for (j in 1:N){
#    k = i - j
#    if (k < L && k >= 0){
#      weights[i, j] = lambda ** ((k - theta) ** 2)
#    }
#  }
#}


#weights

delayedAdstock.media_1_spend = DelayedSimpleAdstock(df$media_1_spend, N=730, lambda=0.5, 1, 4)

delayedAdstock.media_1_spend

# plotting all the charts
df$media_1_spend # og data point
geoSimple.media_1_spend
geoLog.media_1_spend
delayedAdstock.media_1_spend



res = cbind(df$media_1_spend, geoSimple.media_1_spend, geoLog.media_1_spend, delayedAdstock.media_1_spend)


matplot(res, type="b", pch=1, col=1:4)
legend("topright", legend=1:4, col=1:4, pch=1)



###############################################################################
# find the Optimal Adstock Rate
# How this function works:

# the NLS function in R finds the optimal adstock rate lambda by minimizing the model's sum of squared error.
# we want the lambda between 0 and 1, hence this is a constrained optimization problem
# sometimes the nls function returns a lambda value that is out of this range, we need the nlsLM function to apply the constraint 
#############################################################################

'''
Find the optimal Adstock model.

--- 
Inputs:
df: Data Frame.
t: Model type. Options: "GeoSimple", "Delayed", "GeoLog".
L: The maximum duration of carryover effect.
theta: The delay of the peak effect.

---
Returns:
A model.

NOTE: this estimates the lambda parameter for a single media spend
'''

# we want to fit Eq.1 and one of the three models to find the optimal Adstock rate.

# NOTE: 


adstock_model = function(df, t, L = 13, theta = 1){
  
  if (t == 'GeoSimple'){
    model = nls(data = agg_df, sales ~ alpha + beta * GeometricSimpleAdstock(tv_spend, lambda), 
                 start = c(alpha = 1, beta = 1, lambda = 0))
    
    if (summary(model)$coef[3, 1] < 0 || summary(model)$coef[3, 1] >= 1){
      
      model = nlsLM(data = agg_df, sales ~ alpha + beta * GeometricSimpleAdstock(tv_spend, lambda),
                     start = list(alpha = 1, beta = 1, lambda = 0), lower = c(alpha = -Inf, beta = -Inf, lambda = 0), upper = c(alpha = Inf, beta = Inf, lambda = 1)) 
    }
    
  } else if (t == 'Delayed'){
    
    N = dim(df)[1]
    model = nlsLM(data = agg_df, sales ~ alpha + beta * DelayedSimpleAdstock(tv_spend, N, lambda, theta, L), 
                   start = list(alpha = 1, beta = 1, lambda = 0),
                  lower = c(alpha = -Inf, beta = -Inf, lambda = 0),
                  upper = c(alpha = Inf, beta = Inf, lambda = 1)) 
    
  } else if (t == 'GeoLog'){
    
    model = nls(data = agg_df, sales ~ alpha + beta * GeometricLogAdstock(tv_spend, lambda), 
                 start = c(alpha = 1, beta = 1, lambda = 0))
    
    if (summary(model)$coef[3, 1] < 0 || summary(model)$coef[3, 1] >= 1){
      
      model = nlsLM(data = agg_df, sales ~ alpha + beta * GeometricLogAdstock(tv_spend, lambda), 
                     start = list(alpha = 1, beta = 1, lambda = 0), 
                    lower = c(alpha = -Inf, beta = -Inf, lambda = 0), 
                    upper = c(alpha = Inf, beta = Inf, lambda = 1)) 
    }
  }
  
  return(model)
}
mod = adstock_model(agg_df, 'Delayed') 
names(summary(mod))


# represents our optimal lambda parameter
lambda.param = summary(mod)$parameters[3] 

#################################################################################
# create our delayed factors from training
#head(agg_df)

delayed.tv_spend = DelayedSimpleAdstock(agg_df$tv_spend, N=104, lambda=0.3281, L = 8, theta = 4)
delayed.search_spend = DelayedSimpleAdstock(agg_df$search_spend, N=104, lambda=0.45180, L = 8, theta = 4)
delayed.display_spend = DelayedSimpleAdstock(agg_df$display_spend, N=104, lambda=0.45180, L = 8, theta = 4)


################################################################################
# combine output back into original data frame
################################################################################


adstock_rate = data.frame(
  delayed.tv_spend = delayed.tv_spend,
  delayed.search_spend = delayed.search_spend,
  delayed.display_spend = delayed.display_spend
)

adstock_rate
###############################################################################
# Hill Function

"""
S = Slope; greater than 0
K = Half Saturation Point; greater than 0
beta = Coefficient for channel; greater than 0
x = media spend
"""
# resources
# https://rpubs.com/dtkaplan/646
# https://medium.com/data-science/carryover-and-shape-effects-in-media-mix-modeling-paper-review-fd699b509e2d#2449
###############################################################################

beta_hill = function(x, S, K, hill_beta){
  
  return (hill_beta - (K**S*hill_beta)/(x**S+K**S))
}


# MODEL TRAINING ###############################################################

beta_model <- nlsLM(
  sales ~ alpha + beta_hill(agg_df$display_spend, S, K, hill_beta),
  data = agg_df,
  start = list(alpha = 1, S = 0.2, K = 0.2, hill_beta = .5),
  control = nls.lm.control(maxiter = 1000)
)


summary(beta_model)




# we want to apply the hill function to relevant coefficients
# apply the exponential function
exp.func = function(x, alpha){
  
  return (1-exp(-alpha * x))
}




# create our exponential saturation coefficients
exp.tv_spend = exp.func(agg_df$tv_spend, alpha=0.25)
exp.search_spend = exp.func(agg_df$search_spend, alpha=0.25)
exp.display_spend = exp.func(agg_df$display_spend, alpha=0.25)

# put the result of the hill function into a dataframe #########################

exp_decay_rate = data.frame(
  exp.tv_spend = exp.tv_spend,
  exp.search_spend = exp.search_spend,
  exp.display_spend = exp.display_spend
)

# turn off par(mfrow)
#dev.off()


plot(exp.tv_spend, type="l")


# Building a 3rd model #########################################################

# test incorporating a more advanced ###########################################
# train + fit the saturation models
beta_tv_spend = beta_hill(agg_df$tv_spend, S=2.876e-01, K=4.589e-18, hill_beta=5.745e-01)
beta_search_spend = beta_hill(agg_df$search_spend, S=2.994e-01, K=4.024e-16, hill_beta=5.802e-01)
beta_display_spend = beta_hill(agg_df$display_spend, S=3.420e-01, K=9.895e-16, hill_beta=6.161e-01)

plot(beta_tv_spend, type="l")

# build the dataframe
beta_decay_rate = data.frame(
  beta_tv_spend = beta_tv_spend,
  beta_search_spend = beta_search_spend,
  beta_display_spend = beta_display_spend
)


# bind our dataframes together
model_df = cbind(exp_decay_rate, adstock_rate, beta_decay_rate, agg_df)

#model_exp_df = cbind(exp_decay_rate, adstock_rate)

# model w/ beta decay rate
#model_decay_df = cbind(beta_decay_rate, adstock_rate)

# combine with our og dataframe
#model_df = cbind(model_rate_df, agg_df)

# 3rd model
#model_df.2 = cbind(model_decay_rate_df, agg_df)

################################################################################

#####################################
# Create training + test data sets
#####################################

train_set = model_df[model_df$week_num < 2023,]
test_set = model_df[model_df$week_num >= 2023,]


# if(!require('mosaic')) {
#   install.packages('mosaic')
#   library('mosaic')
# }



#nums = seq(from=1, to=15, by=0.5)


#res = beta_hill(nums, S=0.5, K=1, hill_beta=0.3)
#res.1 = beta_hill(nums, S=0.95, K=0.748, hill_beta=0.3925)
#res.2 = beta_hill(nums, S=0.5, K=2, hill_beta=0.3)
#res.3 = beta_hill(nums, S=2, K=1.867, hill_beta=1.144)


#plot(res, type="l")

#data = cbind(res, res.1)
#data.1= cbind(data, res.2)
#data_df = cbind(data.1, res.3)
#matplot(data_df, type="l", pch=1, col=1:4)

# application of the hill (shape) function first then adstock function

# possible this data needs to be normalized first

normalize = function(x.var){
  return ((x.var - min(x.var)) / (max(x.var) - min(x.var)))
}


norm.media_1_spend = normalize(df$media_1_spend)

# hill.media_1_spend = beta_hill(df$media_1_spend, S=0.5, K=1, hill_beta=0.3)
# hill.media_2_spend = beta_hill(norm.media_1_spend, S=0.95, K=0.748, hill_beta=0.395)
# hill.media_3_spend = beta_hill(norm.media_1_spend, S=0.03179, K=0.0000000000000098, hill_beta=1.310)

# hill.func.res = cbind(hill.media_1_spend, hill.media_2_spend)

# plot(hill.media_1_spend, type="l")

# matplot(hill.func.res, type="l", pch=1, col=1:3)

# application of the hill function + ad stock function

# hill.media_1_spend = beta_hill(geoSimple.media_1_spend, S=0.5, K=1, hill_beta=0.3)


#fitModel(sales ~ alpha + beta * beta_hill(media_1_spend, S, K, beta),
#         data = df,
#         start = c(alpha = 1, S=0.1, K=0.1, beta = 0.1),
#         control = list(maxiter=500))

#df$media_1_spend

# remove NA rows
#agg_df = na.omit(agg_df)

###############################################################################
# raw initial modeling
#############################################################################
head(train_set)

# MLR
model.1 = lm(sales ~ tv_spend + search_spend + display_spend, data=train_set)
summary(model.1)

###############################################################################
# 2nd model
# explores using the exponential (media saturation), adstock (delay)
##############################################################################
# remove the week num date
#model_df = na.omit(model_df)
#model_df$week_num = NULL
# remove the level_holt
#model_df$level_holt = NULL


model.2 = lm(sales ~ exp.tv_spend + exp.search_spend + exp.display_spend + delayed.tv_spend
             + delayed.search_spend + delayed.display_spend + tv_spend + search_spend
             + display_spend, data=train_set)
summary(model.2)


################################################################################
# ANOVA TESTING
# 
################################################################################
anova(model.1, model.2)

###############################################################################
# 3rd model using the more advanced hill function, adstock decay
###############################################################################
# remove NA rows
#model_df.2 = na.omit(model_df.2)
# remove date
#model_df.2$week_num = NULL

#str(model_df.2)

model.3 = lm(sales ~ beta_search_spend + beta_display_spend + beta_tv_spend + delayed.tv_spend
             + delayed.search_spend + delayed.display_spend + tv_spend + search_spend
             + display_spend, data=train_set)
summary(model.3)


anova(model.1, model.3)

###############################################################################
# 4th model using: hill function, adstock decay and holt winters level
###############################################################################

model.4 = lm(sales ~ beta_search_spend + beta_display_spend + beta_tv_spend + delayed.tv_spend
             + delayed.search_spend + delayed.display_spend + tv_spend + search_spend
             + display_spend + prior_trend + prior_level, data=train_set)

summary(model.4)

################################################################################
# ANOVA TESTING
# Test to see if the addition of Holt Winters add any predictive power
################################################################################
anova(model.3, model.4)

###############################################################################
# testing addition of adstock decay
#############################################################################

# prep data
geoSimple.media_1_spend
geo_df = cbind(geoSimple.media_1_spend, df)

#cor(df$media_1_spend, df$sales)

geo.model.2 = lm(df$sales ~ df$media_1_spend + geoSimple.media_1_spend)
summary(geo.model.1)

delayed.model.1 = lm(df$sales ~ df$media_1_spend + delayed.media_1_spend)
summary(delayed.model.1)


#model.2 = lm(geo_df$sales ~ geo_df$media_1_spend)
#summary(model.2)

# ANOVA TESTING TO SEE IF THE ADDITION OF AD STOCK ADDS any predictive power

anova(model.1, hill.function.model.3)

"""
Given the p-value is close to zero we reject the null hypothesis in favor of the alternative hypthosis
Likely indicating that the geometric transformation to the media spend coefficient has explanatory power
"""


################################################################################
# PREDICTIONS
################################################################################

## Predictions with model 1 #############
# filter for only relevant columns in our dataset
test.1 = subset(test_set, select = c(tv_spend, search_spend, display_spend))

pred.1 = predict(model.1, newdata = test.1)
pred.1.res = data.frame(
  cbind(test_set$sales, pred.1)
)

## Predictions with model 2 #############
test.2 = subset(test_set, select = c(tv_spend, search_spend, display_spend,
                                     exp.tv_spend, exp.search_spend, exp.display_spend,
                                     delayed.tv_spend, delayed.search_spend, delayed.display_spend))

pred.2 = predict(model.2, newdata = test.2)

pred.2.res = data.frame(
  cbind(test_set$sales, pred.2)
)

## Predictions with model 3 #############
test.3 = subset(test_set, select = c(tv_spend, search_spend, display_spend,
                                     beta_search_spend, beta_display_spend, beta_tv_spend,
                                     delayed.tv_spend, delayed.search_spend, delayed.display_spend))

pred.3 = predict(model.3, newdata = test.3)

pred.3.res = data.frame(
  cbind(test_set$sales, pred.3)
)

## Predictions with model 4 #############

test.4 = subset(test_set, select = c(tv_spend, search_spend, display_spend,
                                     beta_search_spend, beta_display_spend, beta_tv_spend,
                                     delayed.tv_spend, delayed.search_spend, delayed.display_spend,
                                     prior_trend, prior_level))

pred.4 = predict(model.4, newdata = test.4)

pred.4.res = data.frame(
  cbind(test_set$sales, pred.4)
)


# Assess the performance of each model using the Mean Squared Prediction error #######

MSPE.pred.1 = (1/nrow(pred.1.res)) * sum((pred.1.res$V1 - pred.1.res$pred.1)^2)
print(paste("MSPE of model1:", MSPE.pred.1))

MSPE.pred.2 = (1/nrow(pred.2.res)) * sum((pred.2.res$V1 - pred.2.res$pred.2)^2)
print(paste("MSPE of model2:", MSPE.pred.2))

MSPE.pred.3 = (1/nrow(pred.3.res)) * sum((pred.3.res$V1 - pred.3.res$pred.3)^2)
print(paste("MSPE of model3:", MSPE.pred.3))

MSPE.pred.4 = (1/nrow(pred.4.res)) * sum((pred.4.res$V1 - pred.4.res$pred.4)^2)
print(paste("MSPE of model4:", MSPE.pred.4))





# poor R squared indicating model poorly explains the variability in the data
# some coefficients are poor predictors


names(model.1)

res.model.2 = residuals(model.2)

plot(y=res.model.2, x=df$sales)
abline(h=0,col="red")

# there's a clear violation of linearity with the sales response variable

# constant variance and linearity assumptions hold with the predictors

par(mfrow=c(3,4))
# Constant Variance
plot(y=res.model.2, x=model.2$fitted.values)
abline(h=0,col="red")

# Linearity & Independence
plot(y=res.model.2, x=model_df$exp.tv_spend)
abline(h=0,col="red")

plot(y=res.model.2, x=model_df$exp.search_spend)
abline(h=0,col="red")

plot(y=res.model.2, x=model_df$delayed.tv_spend)
abline(h=0,col="red")

plot(y=res.model.2, x=model_df$delayed.search_spend)
abline(h=0,col="red")

plot(y=res.model.2, x=model_df$search_spend)
abline(h=0,col="red")

plot(y=res.model.2, x=model_df$display_spend)
abline(h=0,col="red")

plot(y=res.model.2, x=model_df$tv_spend)
abline(h=0,col="red")

# res vs. fitted values
# a bit of a violation of constant variance potentially
#plot(y=res.model.1, x=model.1$fitted.values)
#abline(h=0,col="red")


# checking assumptions of normality
# Assumptions of normality appear to be met
hist(res.model.2)

qqPlot(res.model.2, ylab="Residuals", main="")


