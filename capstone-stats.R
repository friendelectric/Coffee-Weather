library(caTools)
library(ggplot2)
library(effects)
library(ISLR)
library(boot)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

setwd('C:/Coffee and Weather Code/data')

dataset <- read.csv("HourlyTable.csv")
# Remove date:
dataset <- dataset[,-1]

# # HISTOGRAMS/DISTRIBUTIONS -------------------------------------------------------

# As far as I understand, these are supposed to be Poisson distributions.
# Why are there two bulges in the density curve for Content.Espresso? 
# Is it still a Poisson distribution given this? What am I doing wrong?

ggplot(dataset, aes(x=Content.Espresso)) + 
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

# Why do the density curve look wavy/zigzaggy for Trait.Cold, Content.Tea, 
# and Trait.HighInCaffeine? Shouldn't it be a curve like here for lambda=1?
# https://upload.wikimedia.org/wikipedia/commons/thumb/1/16/Poisson_pmf.svg/325px-Poisson_pmf.svg.png

ggplot(dataset, aes(x=Trait.Cold)) + 
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

ggplot(dataset, aes(x=Content.Tea)) + 
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") 

ggplot(dataset, aes(x=Trait.HighInCaffeine)) + 
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

# # CORRELATIONS --------------------------------------------------------------------

correlations <- round(cor(dataset), 2)
correlations <- ifelse(correlations>=0.5, correlations, "NA")  

# DewPoint and Temperature are very highly correlated, so let's not use DewPoint.
# DewPoint is a measure of humidity+comfort; 
# we still have Humidity, so it's OK to drop DewPoint.

# # RUNNING MODELS ------------------------------------------------------------------

# # 1. ESPRESSO -----------------

EspressoLM1 <- lm(Content.Espresso ~ Temperature + Humidity + WindSpeed + Pressure + 
                   Clear + Fog + Rain + Snow,
                 data=dataset)
summary(EspressoLM)

# Linear Regression Model shows R2 of 0.018; it makes sense, since espresso goes into
# the overwhelming majority of drinks, so the impact of any variables will not be 
# too acutely felt on Espresso. However, explaining even 1% of variation via weather
# is a big deal: if we can figure out conditions that push the base of almost all drinks to
# higher sales, then we can drive all sales up. Very good for client.

# We clearly see that Humidity doesn't matter,
# and increases in Pressure lead to more Espressos sold - good, intuitive result.
# (Also the highest t-value is for Pressure - nice.)
# Days with lower visibility (the opposite of Clear is "cloudy") would be beneficial
# for sale sof Espresso, but not when there's precipitation. Also intuitive: coffee intuitively
# associated with more cloudy weather, a pick-me-up of energy on not-so-sunny days.

# If we remove Humidity, Snow is of small significance (makes sense), 
# the rest of the variables are very or quite significant (**, ***), R2 maintained:

EspressoLM2 <- lm(Content.Espresso ~ Temperature + WindSpeed + Pressure + 
     Clear + Fog + Rain + Snow,
   data=dataset)
summary(EspressoLM2)

# Same for GLM:

EspressoGLM1 <- glm(Content.Espresso ~ Temperature + Humidity + WindSpeed + Pressure + 
                  Clear + Fog + Rain + Snow,
                data=dataset,
                family=poisson(link = "log"))
summary(EspressoGLM1)

# Results are the same, removing Humidity:

EspressoGLM2 <- glm(Content.Espresso ~ Temperature + WindSpeed + Pressure + 
                      Clear + Fog + Rain + Snow,
                    data=dataset,
                    family=poisson(link = "log"))
summary(EspressoGLM2)

# Almost the same results as linear regression.

# How to understand AIC in this case?

# # 2. COLD -------------------------

ColdLM1 <- lm(Trait.Cold ~ Temperature + Humidity + WindSpeed + Pressure + 
                Clear + Fog + Rain + Snow,
              data=dataset)
summary(ColdLM1)

# Positive Temperature is a predictor w/ very high t-value, which is good.
# This is probably why we can explain 25% of variation with this model; 
# this is intuitive, that people want cold drinks in warmer weather is a no-brainer.
# The rest of the variables don't have t-values that high, removing the insignificant ones:

ColdLM2 <- lm(Trait.Cold ~ Temperature + Humidity + Pressure + Fog + Rain + Snow,
              data=dataset)
summary(ColdLM2)

# All variables still significant, everything makes sense more or less.
# Snow is suprisingly a positive predictor; then again, these are Canadians we're talking about.

# Now GLM:

ColdGLM1 <- glm(Trait.Cold ~ Temperature + Humidity + WindSpeed + Pressure + 
                  Clear + Fog + Rain + Snow,
                data=dataset,
                family=poisson(link = "log"))
summary(ColdGLM1)

ColdGLM2 <- glm(Trait.Cold ~ Temperature + Humidity + Pressure + 
                  Fog + Rain + Snow,
                data=dataset,
                family=poisson(link = "log"))
summary(ColdGLM2)

# # RUNNING TREES -----------------------------------------------------------------

# # 1. ESPRESSO ---------------------

EspressoTree <- rpart(Content.Espresso ~ Temperature + WindSpeed + Pressure + 
                        Clear + Fog + Rain + Snow,
                      data=dataset,
                      minbucket=50)
prp(EspressoTree)
plot(EspressoTree)
text(EspressoTree, cex=.5)

# The Espresso Tree is a root, so seems like CART isn't useful.
# (Or what does it mean?)

# # 2. COLD -------------------------

ColdTree <- rpart(Trait.Cold ~ Temperature + Humidity + WindSpeed + Pressure + 
                    Clear + Fog + Rain + Snow,
                  data=dataset,
                  minbucket=50)
prp(ColdTree)
plot(ColdTree)
text(ColdTree, cex=.5)

# The tree makes a lot of sense for Trait.Cold. It basically makes a distinction between
# summer weather at first (20.65 degrees). If it's colder, then we basically check if it's a warm 
# season: less than 14.55 degrees will result in way smaller sales. Same for the warm temperatures,
# the higher we get in temperature ranges the more cold drinks are sold.

# Is the tree better for Trait.Cold than linear models?
# Let's run the same with training and test set.

set.seed(2020)
split <- sample.split(dataset$Trait.Cold, SplitRatio = 0.7)
train <- subset(dataset, split==TRUE)
test  <- subset(dataset, split==FALSE)

# The model is different with a test set:

ColdLMTest <- lm(Trait.Cold ~ Temperature + Humidity + Pressure + 
               Clear + Fog + Snow,
             data=train)
summary(ColdLMTest)

ColdLM.Pred <- predict(ColdLMTest, newdata=test)
ColdLM.SSE <- sum((ColdLM.Pred-test$Trait.Cold)^2)
ColdLM.SSE 

ColdTreeTest <- rpart(Trait.Cold ~ Temperature + Humidity + Pressure + 
                        Clear + Fog + Snow,
                      data=train, 
                      minbucket=50)
prp(ColdTreeTest)
plot(ColdTreeTest)
text(ColdTreeTest, cex=.5)

# Almost the same result, now additional level for Humidity.

ColdTree.Pred <- predict(ColdTree, newdata=test)
ColdTree.SSE <- sum((ColdTree.Pred-test$Trait.Cold)^2)
ColdTree.SSE

# The tree does substantially better than a linear model 
# than presented with a test set!

# Let's check with GLM now (the model a bit different with test set):

ColdGLMTest <- glm(Trait.Cold ~ Temperature + Humidity + Pressure + 
                  Fog + Snow,
                data=train,
                family=poisson(link = "log"))
summary(ColdGLMTest)

ColdGLM.Pred <- predict(ColdGLMTest, newdata=test)
ColdGLM.SSE <- sum((ColdGLM.Pred-test$Trait.Cold)^2)
ColdGLM.SSE 

# GLM is doing about two times worse than both Linear Regression and CART.

# Let's do CV on tree:
tr.control <- trainControl(method="cv", number=10)
cp.grid <- expand.grid(.cp = (0:10)*0.001)
tr <- train(Trait.Cold ~ Temperature + Humidity + Pressure + 
              Clear + Fog + Snow,
            data=train, 
            method="rpart", 
            trControl = tr.control, 
            tuneGrid = cp.grid)
tr
best.tree <- tr$finalModel
prp(best.tree)

# The tree is VERY detailed, humidity plays a role different temperature ranges.

best.tree.pred <- predict(best.tree, newdata=test)

# Why does this give an error? These issues usually caused by spaces in column names,
# unclear why here:
#     Error in eval(predvars, data, env) : object 'ClearTRUE' not found

best.tree.sse <- sum((best.tree.pred - test$Trait.Cold)^2)
best.tree.sse