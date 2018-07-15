rm(list=ls())

library(gdata)
library(dplyr)
library(woeBinning)
library(caTools)
library(ggplot2)
library(gridExtra)
library(DMwR)
library(reshape2)
library(MASS)
library(car)
library(caret)
library(rpart)
library(rattle)
library(ROCR)
library(DAAG)
library(randomForest)
# install.packages("DataExplorer")
library(DataExplorer)
library(knitr)

dem <- read.csv("Demographic data.csv", na.strings = c("", "NA"))
cred <- read.csv("Credit Bureau data.csv", na.strings = c("", "NA"))


#Let's look at the structure of the data

str(dem)
str(cred)


#== Checking Rate of Default

default_rate <- sum(dem$Performance.Tag == 1, na.rm = T)/ nrow(dem)
default_rate      

# Fraction of Defaulters = 0.04134932



#== Checking for Missing Values

plot_missing(dem)

knitr::kable(sort(sapply(dem, function(x) sum(is.na(x))), decreasing = T))

# Number of NA in Demographic Data

# |                                            |    x|
# |:-------------------------------------------|----:|
# |Performance.Tag                             | 1425|
# |Education                                   |  119|
# |Profession                                  |   14|
# |Type.of.residence                           |    8|
# |Marital.Status..at.the.time.of.application. |    6|
