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
# |No.of.dependents                            |    3|
# |Gender                                      |    2|

plot_missing(cred)

knitr::kable(sort(sapply(cred, function(x) sum(is.na(x))), decreasing = T))

# Number of NA in Credit Bureau Data

# |                                                                |    x|
# |:---------------------------------------------------------------|----:|
# |Performance.Tag                                                 | 1425|
# |Avgas.CC.Utilization.in.last.12.months                          | 1058|
# |Presence.of.open.home.loan                                      |  272|
# |Outstanding.Balance                                             |  272|
# |No.of.trades.opened.in.last.6.months                            |    1|




#== Checking for Duplicate Values

dem_dup <- dem[duplicated2(dem$Application.ID),]
cred_dup <- cred[duplicated2(cred$Application.ID),]

# Both Demographic and Credit bureau have duplicated rows

setdiff(dem_dup$Application.ID,cred_dup$Application.ID)
setdiff(cred_dup$Application.ID,dem_dup$Application.ID)

# Both datasets have the same Application ID duplicated, hence we will remove these rows

dem_clean <- dem[!duplicated2(dem$Application.ID),]
cred_clean <- cred[!duplicated2(cred$Application.ID),]



#== Merging datasets

# Checking if Applications ID are same in both datasets

setdiff(dem_clean$Application.ID,cred_clean$Application.ID)
setdiff(cred_clean$Application.ID,dem_clean$Application.ID)

# Both datasets contains exactly the same application ID, hence we will proceed with merging these 2 datasets

full_clean <- full_join(dem_clean,cred_clean,by=c("Application.ID","Performance.Tag"))

# Parallely we will be analysing demographic data to see how it can be used during application stage to reject/approve applications












#----------------------------------------------------------------------------------------
#                                   DATA CLEANING
#                                   =============
#________________________________________________________________________________________

# We will not be removing the Application ID column as it will be vital for future analysis

# NOTE : We will use the same cleaning process for demographic data as well


summary(full_clean)

knitr::kable(sort(sapply(full_clean, function(x) sum(is.na(x))), decreasing = T))

# NA values in the combined dataset
# |                                                                |    x|
# |:---------------------------------------------------------------|----:|
# |Performance.Tag                                                 | 1425|
# |Avgas.CC.Utilization.in.last.12.months                          | 1058|
# |Presence.of.open.home.loan                                      |  272|
# |Outstanding.Balance                                             |  272|
# |Education                                                       |  119|
# |Profession                                                      |   14|
# |Type.of.residence                                               |    8|
# |Marital.Status..at.the.time.of.application.                     |    6|
# |No.of.dependents                                                |    3|
