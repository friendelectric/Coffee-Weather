library(caTools)
library(ggplot2)
library(effects)
library(ISLR)
library(boot)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(tidyr)
library(DAAG)
library(DMwR)
library(bestglm)

setwd('C:/Coffee and Weather Code/data')

dataset <- read.csv("CafeHourly.csv")

# Correct types for Time parameters:

dataset$Day    <- as.Date(dataset$Day)
dataset$Hour   <- as.integer(dataset$Hour)
dataset$Season <- as.factor(dataset$Season)

# # 0 CORRELATIONS ------------------------------------------------

# Correlations (for all variables except for Time):

cors <- round(cor(dataset[,-c(1:3)]), 2)

# Among independent variables, Temperature and DewPoint are strongly
# correlated (0.93). Let's run a correlation test:

cor.test(dataset$Temperature, dataset$DewPoint)

# Significance is *** for this correlation, so will not use
# DewPoint variable in modeling.

# # 1 TRAIT : COLD ------------------------------------------------

# # # 1.1 Models -------------------------------------------------

coldModel1 <- lm(Trait.Cold ~ Temperature + Humidity + WindSpeed + 
                   Pressure + Clear + Fog + Rain + Snow, data=dataset)
summary(coldModel1)

# WindSpeed & ClearTRUE are insignificant. Dropping WindSpeed:

coldModel2 <- update(coldModel1, formula=drop.terms(coldModel1$terms, 3, keep.response = TRUE))
summary(coldModel2)

# ClearTRUE still insignificant. Dropping:

coldModel3 <- update(coldModel2, formula=drop.terms(coldModel2$terms, 4, keep.response = TRUE))
summary(coldModel3)

# All remaining predictors are significant.

# # # 1.2 Cross-validation -----------------------------------

# Leave-one-out cross-validation:

train(Trait.Cold ~ Temperature + Humidity + WindSpeed + 
        Pressure + Clear + Fog + Rain + Snow, method = "lm", 
      data = dataset, trControl = trainControl(method = "LOOCV"))
# RMSE  Rsquared  MAE
# 1.61  0.245     1.1

train(Trait.Cold ~ Temperature + Humidity + 
        Pressure + Clear + Fog + Rain + Snow, method = "lm", 
      data = dataset, trControl = trainControl(method = "LOOCV"))
# RMSE  Rsquared  MAE
# 1.61  0.245     1.1

train(Trait.Cold ~ Temperature + Humidity + 
        Pressure + Fog + Rain + Snow, method = "lm", 
      data = dataset, trControl = trainControl(method = "LOOCV"))
# RMSE  Rsquared  MAE
# 1.61  0.245     1.1

# K-fold cross-validation:

cv.lm(dataset, coldModel1, m=5, seed=50)
# ms 2.61 
cv.lm(dataset, coldModel2, m=5, seed=50)
# ms 2.6
cv.lm(dataset, coldModel3, m=5, seed=50)
cv.lm(dataset, coldModel1, m=10, seed=50)
cv.lm(dataset, coldModel2, m=10, seed=50)
cv.lm(dataset, coldModel3, m=10, seed=50)

# Mean square=2.6 for all but coldModel1 when K=5 (2.61 in that case).

# # # 1.3 Discussion -----------------------------------

# I am not sure how to interpret cross-validation results.
# Do both LOO and K-fold say that all our models are essentially the same
# and don't offer any improvement over each other? Should this influence
# choice of model, or should we only choose based on parsimony?
#
# It seems like, based on K-fold CV (K=5), the second model is the first one
# we could use (mean squares slightly smaller starting with 2nd model,
# and maintained in all following models), but the third one is most parsimonious. 
#
# Since I don't know how to answer this, here's an analysis based on 3rd model 
# (most parsimonious one):

summary(coldModel3)

# R^2 for the model is 0.246. This will likely be the model with most
# explanatory power among models in this project. It is commonsensical
# that nice (sunny, clear) weather means more iced beverages sold; however, our particular
# variables can show the intricacies of this relationship and can also
# deliver some insights (like "iced drinks aren't only consumed in hot weather
# to a level that would warrant looking into selling more iced drinks in the winter").
#
# Temperature has a positive relationship
# with cold drink sales, and humidity has a negative relationship.
# Therefore, we can definitely conclude that higher temperatures and less
# humid weather lead to more sales in cold drinks. Makes sense:
# when you're in hot dry weather, you'll want an iced beverage.
# (However, compared to the other weather variables we're examining, 
# the effect is quite small.)
# 
# Pressure has a negative relationship with Cold drinks sales.
# This is strange, because higher pressure means nicer (sunnier) weather; 
# it could mean that nicer weather isn't necessarily related to Cold drinks
# in a commonsensical way. Perhaps this is evidence that outside of summer
# hours, there isn't such a clear-cut relationship between cold drinks
# and nice weather, and people will drink iced drinks whenever?
# Could be a sign of the local culture (more on this later).
#
# Intuitively, Rain is a negative predictor for sales of cold drinks: 
# typically, we'll think of hot drinks to get comfortable in rainy weather.
#
# Fog and Snow as predictors for cold drink sales may prove challenging to explain,
# as they both seem to result in more cold drink sales. Foggy weather showing up as a predictor
# may indicate that cold drinks shouldn't necessarily be associated with being outside;
# when it's warm out, it makes sense to cool down even if it isn't sunny or clear.
#
# Snow appearing as (positive!) predictor may be a function of local culture.
# In Canada, people do drink iced beverages not only in the summer, but in the winter, too,
# albeit not as often. 
#
# Overall, it seems like there is a surprising finding: cold drinks aren't necessarily
# only associated with hot and dry weather (likely we're seeing this effect due to summer hours),
# and are also consumed throughout the year. This may be very useful for the client,
# who may not realize there's still a demand in the winter for cold drinks typically 
# sold in the summer, like cold brew coffee.


# # 2 CONTENT : ESPRESSO -----------------------------------------

# # # 2.1 Models -------------------------------------------------

# Running first LM on Content.Espresso:

espressoModel1 <- lm(Content.Espresso ~ Temperature + Humidity + WindSpeed + 
                       Pressure + Clear + Fog + Rain + Snow, data=dataset)
summary(espressoModel1)

# Humidity is the only predictor that's insignificant. Removing:

espressoModel2 <- update(espressoModel1, formula=drop.terms(espressoModel1$terms, 2, keep.response = TRUE))
summary(espressoModel2)

# All remaining predictors are significant.

# # # 2.2 Cross-validation ----------------------------------------

# Leave-one-out cross-validation:

train(Content.Espresso ~ Temperature + Humidity + WindSpeed + 
        Pressure + Clear + Fog + Rain + Snow, method = "lm", 
      data = dataset, trControl = trainControl(method = "LOOCV"))
# RMSE  Rsquared  MAE 
# 5.25  0.0158    4.22

train(Content.Espresso ~ Temperature + WindSpeed + 
        Pressure + Clear + Fog + Rain + Snow, method = "lm", 
      data = dataset, trControl = trainControl(method = "LOOCV"))
# RMSE  Rsquared  MAE 
# 5.25  0.016     4.22

# K-fold cross-validation:

cv.lm(dataset, espressoModel1, m=5, seed=50)
# ms 27.6
cv.lm(dataset, espressoModel2, m=5, seed=50)
# ms 27.6
cv.lm(dataset, espressoModel1, m=10, seed=50)
# ms 27.6
cv.lm(dataset, espressoModel2, m=10, seed=50)
# ms 27.6

# # # 2.3 Discussion -----------------------------------------

# LOO showed R^2 was a little bit higher for the 2nd model, 
# and it's the most parsimonious model, so let's try to explain its output:

summary(espressoModel2)

# R^2 is 0.0186; it makes sense, since espresso goes into
# most of the drinks offered at the cafe, so the impact of any weather variables will not be 
# too acutely felt on Espresso. Unlike with cold drinks, I can't think of a commonsensical idea 
# that could point to a relationship between weather and espresso sales. 
#
# In the absence of big data, explaining even 1% of variation in espresso sales via weather is good. 
# If we can figure out what weather conditions are favorable for selling espresso, 
# which is the base for most products the client sells,
# we can help the client adjust product selection or help with marketing 
# (for example, if foggy days are good for espresso sales, why not make a special sold on
# foggy days only, and advertise it on social media?).
#
# Temperature and WindSpeed have (compared to the other weather variables) 
# very low Estimates, meaning that they have the least impact on Espresso sales among 
# the variables we're looking at. Unsure for now how to interpret them. (More on this below.)
# 
# Positive relationship with Pressure is likely a manifestation of how atmospheric pressure works:
# pressure going up means good or fair weather moving in. This may be simply the positive effect of 
# better weather on overall consumption (since Espresso goes in most drinks, we can extrapolate).
# In this context, WindSpeed as predictor makes more sense.
# Wind blows from areas of high pressure toward areas of low pressure. 
# The closer the high and low pressure areas, the faster the wind. 
# So, it could be that there's a small positive effect on consumption of most drinks
# (those containing espresso) when the weather is about to change for worse.
#
# Clear days are a NEGATIVE predictor for espresso sales. Combined with Foggy days being POSITIVE
# predictors, it seems like espresso-based beverages are mostly enjoyed on days with less sunlight.
# (Espresso-based beverages in this case are a pick-me-up to replenish energy -- makes sense.)
#
# At the same time, Rain and Snow are NEGATIVE predictors for espresso sales. It may signify
# that precipitation introduces incovenience. So, a cloudy/foggy day is good for selling 
# espresso-based beverages, but when it starts raining, that effect is cancelled out by
# the inconvenience of walking outside.
#
# In this case, it makes sense that snow is also a predictor: in this climate, when it snows,
# it SNOWS, a lot. So, people tend to stay inside more when they get snowed in. Interestingly,
# if we run a model controlling for LowSalesPeriod (Winter AND Summer), 
# both Rain and Snow survive as predictors:

summary(lm(DrinksSold ~ Temperature + Humidity + WindSpeed + 
             Pressure + Clear + Fog + Rain + Snow, 
           dataset))

# So, precipitation definitely matters for espresso-based beverages.


# # 3 CONTENT : TEA ----------------------------------------------------

# # # 3.1 Models -------------------------------------------------

teaModel1 <- lm(Content.Tea ~ Temperature + Humidity + WindSpeed + 
                  Pressure + Clear + Fog + Rain + Snow, data=dataset)
summary(teaModel1)

# Temperature, WindSpeed, Pressure, SnowTRUE are insignificant.
# Eliminating in order of insignificance (starting with Temperature):

teaModel2 <- update(teaModel1, formula=drop.terms(teaModel1$terms, 1, keep.response = TRUE))
summary(teaModel2)

# WindSpeed, Pressure, SnowTRUE still insignificant. Removing WindSpeed:

teaModel3 <- update(teaModel2, formula=drop.terms(teaModel2$terms, 2, keep.response = TRUE))
summary(teaModel3)

# Pressure and SnowTRUE still insignificant. Removing Snow:

teaModel4 <- update(teaModel3, formula=drop.terms(teaModel3$terms, 6, keep.response = TRUE))
summary(teaModel4)

# Pressure still insignificant. Removing:

teaModel5 <- update(teaModel4, formula=drop.terms(teaModel4$terms, 2, keep.response = TRUE))
summary(teaModel5)

# All remaining predictors are significant.

# # # 3.2 Cross-validation -------------------------------------------------

# Leave-one-out cross-validation:

train(Content.Tea ~ Temperature + Humidity + WindSpeed + 
        Pressure + Clear + Fog + Rain + Snow, 
      method = "lm", data = dataset, trControl = trainControl(method = "LOOCV"))
# RMSE  Rsquared  MAE 
# 1.93  0.0259    1.53

train(Content.Tea ~ Humidity + WindSpeed + 
        Pressure + Clear + Fog + Rain + Snow, 
      method = "lm", data = dataset, trControl = trainControl(method = "LOOCV"))
# RMSE  Rsquared  MAE 
# 1.93  0.0262    1.53

train(Content.Tea ~ Humidity + Pressure + Clear + Fog + Rain + Snow, 
      method = "lm", data = dataset, trControl = trainControl(method = "LOOCV"))
# RMSE  Rsquared  MAE 
# 1.93  0.0265    1.53

train(Content.Tea ~ Humidity + Pressure + Clear + Fog + Rain, 
      method = "lm", data = dataset, trControl = trainControl(method = "LOOCV"))
# RMSE  Rsquared  MAE 
# 1.93  0.0267    1.53

train(Content.Tea ~ Humidity + Clear + Fog + Rain, 
      method = "lm", data = dataset, trControl = trainControl(method = "LOOCV"))
# RMSE  Rsquared  MAE 
# 1.93  0.0266    1.53

# K-fold cross-validation:

cv.lm(dataset, teaModel1, m=5, seed=50)
cv.lm(dataset, teaModel2, m=5, seed=50)
cv.lm(dataset, teaModel3, m=5, seed=50)
cv.lm(dataset, teaModel4, m=5, seed=50)
cv.lm(dataset, teaModel5, m=5, seed=50)

cv.lm(dataset, teaModel1, m=10, seed=50)
cv.lm(dataset, teaModel2, m=10, seed=50)
cv.lm(dataset, teaModel3, m=10, seed=50)
cv.lm(dataset, teaModel4, m=10, seed=50)
cv.lm(dataset, teaModel5, m=10, seed=50)

# mean square 3.72 for all.

# # # 3.3 Discussion -------------------------------------------------

# Based on LOOCV, 4th model had the largest R^2, so let's look at that one:

summary(teaModel4)

# R^2 for this model is 0.0286: like espresso, tea is also a part of
# many different types of drinks (for the purposes of this project,
# tea-based drinks include simple tea leaves+hot water, iced teas,
# chai lattes, and matcha lattes - the former two are made with tea concentrates).
#
# More humid weather means less teas sold, as does higher pressure.
# This makes sense intuitively: tea is less associated with nice weather, it's
# more of a comfort beverage.
#
# This is also confirmed by Clear weather being a negative predictor 
# of tea sales At the same time, Rain is a positive predictor, and Fog, too:
# so, rainy and foggy weather means more teas sold. 
#
# All this suggests that tea is a comfort beverage that sells best
# on days when there's less sunlight. Interestingly, Rain actually 
# leads to more teas sold, while it dampens Espresso sales.
# It appears that people will rather go for tea rather than coffee
# when it's raining.