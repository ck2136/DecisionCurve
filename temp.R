# - - - - - - - - - - - - - - - - - - - - #
# File:             temp.R
# Objective:        Temporary file for running ui and server related code for the DecisionCurve shiny app
# Date created:     1/24/2019
# Modified by:      C.K.
# Date modified:    1/24/2019
# - - - - - - - - - - - - - - - - - - - - #

# - - - - - - - - - - - - - - - - - - - - #
# Setting up ----
# - - - - - - - - - - - - - - - - - - - - #
# load libraries
# shiny packages
library(shiny)
library(shinydashboard)

# for summary statistics
library(DT)
library("psych")
library(dplyr)
library(formattable)

# prediction models
library(rpart)
library(randomForest)
library(e1071)
library(xgboost)

# plotting
library(ggfortify)
library(plotly)
library(pROC)
library(rpart.plot)
library(caret)

# fit stats
library(stargazer)
library("MASS")

# - - - - - - - - - - - - - - - - - - - - #
# Plotting Confusion Matrix ----
# - - - - - - - - - - - - - - - - - - - - #
mod_glm <- glm(low ~ age + race + lwt + smoke, data = birthwt, family = binomial)
as.factor(mod_glm$y)


cm <- confusionMatrix(if_else(mod_glm$fitted.values >= 0.5, as.factor(names(table(birthwt[,"low"])))[1], as.factor(names(table(birthwt[,"low"])))[2]), as.factor(birthwt[,"low"]))
mod_glm$fitted.values
fourfoldplot(cm$table)

mod_rf <- randomForest(as.factor(low) ~ age + race, data=birthwt,
                       ntree = 1000, mtry = 2)
mod_svm <- svm(low ~ age + race, data = birthwt)
mod_glm$fitted.values
pred <- predict(mod_rf, birthwt, type = "prob" )[,2]
pred <- predict(mod_svm, birthwt)
cm <- confusionMatrix(if_else(pred >= 0.5, as.factor(names(table(birthwt[,"low"])))[1], as.factor(names(table(birthwt[,"low"])))[2]), birthwt %>% select_("low") %>% mutate_("low" = "as.factor(low)") %>% unlist)
fourfoldplot(confusionMatrix(xtab)$table)
cm <- confusionMatrix(if_else(pred >= 0.5, as.factor(names(table(birthwt[,"low"])))[1], as.factor(names(table(birthwt[,"low"])))[2]), as.factor(birthwt[,"low"]))
fourfoldplot(cm$table)
cm$overall
cm$byClass