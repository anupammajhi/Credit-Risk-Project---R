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
# |Gender                                                          |    2|
# |No.of.trades.opened.in.last.6.months                            |    1|


# NOTE : FOR EVERY VARIABLE WITH LESS THAN 20 MISSING VALUES, WE WILL REMOVE THE MISSING VALUE ROWS. 
#        FOR >20 NA's, WE WILL IMPUTE USING WOE.

# Removing rows as mentioned above
full_clean <- full_clean[-which(is.na(full_clean$No.of.trades.opened.in.last.6.months)
                                | is.na(full_clean$Gender)
                                | is.na(full_clean$No.of.dependents)
                                | is.na(full_clean$Marital.Status..at.the.time.of.application.)
                                | is.na(full_clean$Type.of.residence)
                                | is.na(full_clean$Profession)),]


dem_clean <- dem_clean[-which(is.na(dem_clean$Gender)
                              | is.na(dem_clean$No.of.dependents)
                              | is.na(dem_clean$Marital.Status..at.the.time.of.application.)
                              | is.na(dem_clean$Type.of.residence)
                              | is.na(dem_clean$Profession)),]


summary(dem_clean)
summary(full_clean)

#==== AGE variable

boxplot(full_clean$Age)

# There are outliers towards lower ages
# We will remove rows with age lower than lower whisker

min_age <- quantile(full_clean$Age)[2] - 1.5 * IQR(full_clean$Age)
min_age 
# 13 years

full_clean <- full_clean[-which(full_clean$Age < min_age),]
dem_clean <- dem_clean[-which(dem_clean$Age < min_age),]

summary(full_clean$Age)
summary(dem_clean$Age)


#==== No. of dependents variable

summary(full_clean$No.of.dependents)

# Converting age to factor

full_clean$No.of.dependents <- as.factor(full_clean$No.of.dependents)
dem_clean$No.of.dependents <- as.factor(dem_clean$No.of.dependents)

summary(full_clean$No.of.dependents)


#==== INCOME variable

summary(full_clean$Income)

boxplot(full_clean$Income)

quantile(full_clean$Income,seq(0,1,0.01))

full_clean$Income[which(full_clean$Income <= 0)]

# There are negative values and 0, removing those rows

# As we can see, there are rows that have either 0 or -0.5 as Income. This has to be a data entry error.
# We will be removing these rows as they do not add value.

full_clean <- full_clean[-which(full_clean$Income <= 0),]
dem_clean <- dem_clean[-which(dem_clean$Income <= 0),]

summary(full_clean$Income)



#==== EDUCATION variable

summary(full_clean$Education)
# Variable EDUCATION has a significant number of Missing Values 
# which will be imputed later using WOE



#==== PROFESSION variable

summary(full_clean$Profession)


summary(full_clean)


#==== Variables with categorical values
# Converting to factor

# No.of.months.in.current.residence
# No.of.months.in.current.company
# No.of.times.90.DPD.or.worse.in.last.6.months
# No.of.times.60.DPD.or.worse.in.last.6.months
# No.of.times.30.DPD.or.worse.in.last.6.months
# No.of.times.90.DPD.or.worse.in.last.12.months
# No.of.times.60.DPD.or.worse.in.last.12.months
# No.of.times.30.DPD.or.worse.in.last.12.months
# No.of.trades.opened.in.last.6.months
# No.of.trades.opened.in.last.12.months
# No.of.PL.trades.opened.in.last.6.months
# No.of.PL.trades.opened.in.last.12.months
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
# Total.No.of.Trades

# Presence.of.open.auto.loan
full_clean$Presence.of.open.auto.loan <- as.factor(full_clean$Presence.of.open.auto.loan)

# Presence.of.open.home.loan
full_clean$Presence.of.open.home.loan <- as.factor(full_clean$Presence.of.open.home.loan)



#------------------------------------------------------------------

