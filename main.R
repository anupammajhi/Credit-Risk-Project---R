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


# Indices with Performance tag as NA
full_clean_Tag_NA_indices <- which(is.na(full_clean$Performance.Tag))
dem_clean_Tag_NA_indices <- which(is.na(dem_clean$Performance.Tag))

# creating copy of cleaned data
full_data <- full_clean
dem_data <- dem_clean

# converting Performance Tag to 1 where NA assuming they were rejected at the application stage of CC request and are potential defaulters
# This is done for WOE since WOE is done on Dichotomous dependent variable

full_data[full_clean_Tag_NA_indices,"Performance.Tag"] <- 1
dem_data[dem_clean_Tag_NA_indices,"Performance.Tag"] <- 1



full_data$Performance.Tag <- as.factor(full_data$Performance.Tag)
dem_data$Performance.Tag <- as.factor(dem_data$Performance.Tag)

#==========================================================================================

#-------------------------------------
#    USING WOE AND IV
#-------------------------------------


# Using WOE to impute missing values. This will take care of the remaining outlier behaviors also.

full_woe_data <- woe.binning(full_data, target.var = 'Performance.Tag', pred.var = full_data)

# Now lets look at the IV plot to determine the  variables to be included in the model.
tabulate.binning <- woe.binning.table(full_woe_data)
tabulate.binning[[20]]['IV']

tabulate.binning
capture.output(tabulate.binning, file = "Woe Tables Combined.txt")

IV_tables <- data.frame(unlist(lapply(tabulate.binning, function(x) tail(x$IV,1))))
colnames(IV_tables) <- 'IV'
IV_tables$Variable <- rownames(IV_tables)
rownames(IV_tables) <- NULL

IV_tables$Variable <- substring(IV_tables$Variable,14)


plotFrame1 <- IV_tables[order(-as.numeric(IV_tables$IV)), ]
plotFrame1$Variable <- factor(plotFrame1$Variable,levels = plotFrame1$Variable[order(-as.numeric(IV_tables$IV))])

demo_iv_plot<-ggplot(plotFrame1, aes(x = Variable, y = as.numeric(as.character(IV)))) +
  geom_bar(width = .35, stat = "identity", color = "lightblue", fill = "lightblue") +
  ggtitle("INFORMATION VALUE ") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = 'bold', color= 'darkgrey', hjust = 0.5)) +
  theme(axis.text.x = element_text( size = 10, angle = 90)) +
  geom_hline(yintercept=0.02, linetype="dashed", color = "red", size = 1) +
  labs(y="IV")
demo_iv_plot

write.csv(plotFrame1, "IV tables Combined.csv")



# We will remove variables with IV value lower than 0.02, except Application ID

full_data_low_IV <- as.character(full_woe_data[which(full_woe_data[,3] <= 0.02),1])
full_data <- full_data[,-which(colnames(full_data) %in% full_data_low_IV[!full_data_low_IV %in% c("Application.ID")])]

full_woe_data <- woe.binning(full_data, target.var = 'Performance.Tag', pred.var = full_data)


#woe.binning.plot(full_woe_data)

full_woe_data_deploy <- woe.binning.deploy(full_data,full_woe_data)

full_woe_data_deploy_imputed <- woe.binning.deploy(full_data, full_woe_data, add.woe.or.dum.var = 'woe')



# Using WOE on Demographic data

dem_woe_data <- woe.binning(dem_data, target.var = 'Performance.Tag', pred.var = dem_data)




#Now lets look at the IV plot to determine the  variables to be included in the model.
tabulate.binning_dem <- woe.binning.table(dem_woe_data)
tabulate.binning_dem[[3]]['IV']

tabulate.binning_dem
capture.output(tabulate.binning_dem, file = "Woe Tables Demographic.txt")

IV_tables <- data.frame(unlist(lapply(tabulate.binning_dem, function(x) tail(x$IV,1))))
colnames(IV_tables) <- 'IV'
IV_tables$Variable <- rownames(IV_tables)
rownames(IV_tables) <- NULL

IV_tables$Variable <- substring(IV_tables$Variable,14)


plotFrame1 <- IV_tables[order(-as.numeric(IV_tables$IV)), ]
plotFrame1$Variable <- factor(plotFrame1$Variable,levels = plotFrame1$Variable[order(-as.numeric(IV_tables$IV))])

demo_iv_plot<-ggplot(plotFrame1, aes(x = Variable, y = as.numeric(as.character(IV)))) +
  geom_bar(width = .35, stat = "identity", color = "lightblue", fill = "lightblue") +
  ggtitle("INFORMATION VALUE ") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = 'bold', color= 'darkgrey', hjust = 0.5)) +
  theme(axis.text.x = element_text( size = 10, angle = 90)) +
  geom_hline(yintercept=0.02, linetype="dashed", color = "red", size = 1) +
  labs(y="IV")
demo_iv_plot
write.csv(plotFrame1, "IV tables Demographic.csv")

# variables with low IV value (below 0.02) idealy have low or no predictive power and ideally can be gotten rid of



# We will remove variables with IV value lower than 0.02, except Application ID
dem_data_low_IV <- as.character(dem_woe_data[which(dem_woe_data[,3] <= 0.02),1])
dem_data <- dem_data[,-which(colnames(dem_data) %in% dem_data_low_IV[!dem_data_low_IV %in% c("Application.ID")])]

dem_woe_data <- woe.binning(dem_data, target.var = 'Performance.Tag', pred.var = dem_data)


# woe.binning.plot(dem_woe_data)

dem_woe_data_deploy <- woe.binning.deploy(dem_data,dem_woe_data)

dem_woe_data_deploy_imputed <- woe.binning.deploy(dem_data, dem_woe_data, add.woe.or.dum.var = 'woe')



# variables with low IV value (below 0.02) idealy have low or no predictive power and ideally can be gotten rid of

# Keeping columns Application ID, Performance Tag and Woe valued variables


colnames(full_woe_data_deploy_imputed)
colnames(dem_woe_data_deploy_imputed)


full_data_woe <- full_woe_data_deploy_imputed[,c(setdiff(colnames(full_woe_data_deploy_imputed),colnames(full_woe_data_deploy)),'Performance.Tag',"Application.ID")]
dem_data_woe <- dem_woe_data_deploy_imputed[,c(setdiff(colnames(dem_woe_data_deploy_imputed),colnames(dem_woe_data_deploy)),'Performance.Tag',"Application.ID")]


full_data_woe$woe.Application.ID.binned <- NULL
dem_data_woe$woe.Application.ID.binned <- NULL

# Separating rows which originally had Performance.Tag as NA (i.e. The rejected customers who were issued credit)
# So that those can be used in validation set to verify if the model can predict "rejection of candidates at form filling stage"

full_rejects_woe <- full_data_woe[full_clean_Tag_NA_indices,]
dem_rejects_woe <- dem_data_woe[dem_clean_Tag_NA_indices,]


# Data without the rows which originally has Performance tag NA i.e. The data of approved customers who were issued credit (those rows are further added in validation set) 
full_approves_woe <- full_data_woe[-full_clean_Tag_NA_indices,]
dem_approves_woe <- dem_data_woe[-dem_clean_Tag_NA_indices,]


#-----------------------------------------------------------------------------------------------------------

#                             -----------------------------------
#                                 EXPLORATORY DATA ANALYSIS
#                             -----------------------------------



#====  Univariate and Bivariate Analysis

# Let's look at the categorical variables in both datasets

plot_bar(full_data)
plot_bar(dem_data)

# Let's look at the  continuous variables in both datasets.

plot_histogram(full_data)
plot_histogram(dem_data)

# Lets's look at the Density plots for various variables in both datasets

plot_density(full_data,  fill = "pink", alpha = 0.5)
plot_density(dem_data, fill = "yellow", alpha = 0.5)



#====  Multivariate Analysis

cormat_full <- round(cor(sapply(full_approves_woe[!colnames(full_approves_woe) %in% c("Application.ID")],as.numeric)),2)
cormat_dem <- round(cor(sapply(dem_approves_woe[!colnames(dem_approves_woe) %in% c("Application.ID")],as.numeric)),2)

# Get upper and lower triangle of the correlation matrix

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}


reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


cormat_full <- reorder_cormat(cormat_full)
cormat_dem <- reorder_cormat(cormat_dem)

full_upper_tri <- get_upper_tri(cormat_full)
full_melted_cormat <- melt(full_upper_tri, na.rm = TRUE)

dem_upper_tri <- get_upper_tri(cormat_dem)
dem_melted_cormat <- melt(dem_upper_tri, na.rm = TRUE)


ggplot(full_melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_minimal()+ 
  theme(axis.text.x = element_blank())+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) + 
  scale_y_discrete(position = "right")

# For the full dataset we see that there is correlation between DPD's, which is expected
# We have similar correlations among PL Trades, Inquiries.


ggplot(dem_melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_minimal()+ 
  theme(axis.text.x = element_blank())+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) + 
  scale_y_discrete(position = "right")


# For Demographic Dataset we do not see any significant correlation among independent or dependent variables



# Number of months in Current Company
p1 <- dem_clean %>% filter(Performance.Tag == 1) %>%
  ggplot(aes(No.of.months.in.current.company, fill = as.factor(Performance.Tag) )) +
  geom_histogram(binwidth = 10) +
  xlab("Number of Months in Current Company") +
  scale_fill_discrete(name = "Performance") +
  theme(legend.position = "none")

# Current Income of the Applicant 
p2 <- dem_clean %>% filter(Performance.Tag == 1) %>%
  ggplot(aes(Income, fill = as.factor(Performance.Tag) )) +
  geom_histogram(binwidth = 5) +
  xlab("Income of the Applicant") +
  scale_fill_discrete(name = "Performance") +
  theme(legend.position = "none")

p3 <- dem_clean %>% filter(Performance.Tag == 1) %>%
  ggplot(aes(No.of.months.in.current.residence, fill = as.factor(Performance.Tag) )) +
  geom_histogram(binwidth = 25) +
  xlab("Number of Months in Current Residence") +
  scale_fill_discrete(name = "Performance") +
  theme(legend.position = "none")


p4 <- full_clean %>% filter(Performance.Tag == 1) %>%
  ggplot(aes(Avgas.CC.Utilization.in.last.12.months, fill = as.factor(Performance.Tag) )) +
  geom_histogram(binwidth = 10) +
  xlab("Average Credit Card Utilization in 12 Months") +
  scale_fill_discrete(name = "Performance") +
  theme(legend.position = "none")


p5 <- full_clean %>% filter(Performance.Tag == 1) %>%
  ggplot(aes(No.of.times.30.DPD.or.worse.in.last.12.months, fill = as.factor(Performance.Tag) )) +
  geom_histogram() +
  xlab("Number of 30 Days Past Dues in 12 months") +
  scale_fill_discrete(name = "Performance") +
  theme(legend.position = "none")
p5

grid.arrange(p1, p2, p3, p4)

# A person with higher number of months in current company - lower the chances of defaulting.
# People with higher income tends to have low default rates.
# People who are in same residence for very few months have high chances of defaulting.
# Frequent Credit Card users tends to default lesser.


#------------------------------------------------------------------------------------------------------------

#------------------------------------
#==== Preparing TRAIN and TEST Data
#------------------------------------



set.seed(421)


full_sample_70 <- sample(nrow(full_approves_woe),0.7*nrow(full_approves_woe),replace = F)
dem_sample_70 <- sample(nrow(dem_approves_woe),0.7*nrow(dem_approves_woe),replace = F)

# preparing training dataset from the customers who were approved for credit
full_train <- full_approves_woe[full_sample_70,]
dem_train <- dem_approves_woe[dem_sample_70,]

# Separating the Application IDs from Train Data
full_train_App_ID <- full_train$Application.ID
dem_train_App_ID <- dem_train$Application.ID

full_train$Application.ID <- NULL
dem_train$Application.ID <- NULL


# preparing test dataset from approved customers and also combined approved + rejected customers
full_test <- full_approves_woe[-full_sample_70,]
dem_test <- dem_approves_woe[-dem_sample_70,]

full_test_incl_rejects <- rbind(full_test,full_rejects_woe)
dem_test_incl_rejects <- rbind(dem_test,dem_rejects_woe)

# Shuffling test data by row after rbind
full_test_incl_rejects <- full_test_incl_rejects[sample(nrow(full_test_incl_rejects),replace = F),]
dem_test_incl_rejects <- dem_test_incl_rejects[sample(nrow(dem_test_incl_rejects),replace = F),]

summary(full_train$Performance.Tag)
summary(full_test$Performance.Tag)

summary(dem_train$Performance.Tag)
summary(dem_test$Performance.Tag)

summary(full_test_incl_rejects$Performance.Tag)
summary(dem_test_incl_rejects$Performance.Tag)







#----------------------------------------------------------------------------------------
#                                   DATA MODELING
#                                   =============
#________________________________________________________________________________________


# We have very less number of defaulters from analysis feasibility perspective
# Using SMOTE(Synthetic Minority Over-Sampling Technique) in training dataset for minority over-sampling 


# We tried running several models on default parameters to see which oversampling and undersampling values give the best results

# Note : Commenting Out the trials to get optimum SMOTE parameters
#        ---------------------------------------------------------


# #Since the train dataframe is unbalanced, will be using SMOTE to balance the training dataframe.
# #Running logisitc regression, decission trees and random forests with default parameters so that we can determine the 
# #optimal amount of over sampling required for the training dataframe.
# 
# #I.Trying to oversample the minonity class by two fold.
# 
# #Running to check if 10% of the minority class in the balanced training dataframe will be suitable.
# seed<-1234
# set.seed(seed)
# data_smote_2_10 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 200, perc.under = 1350)
# summary(data_smote_2_10$Performance.Tag)
# #    0     1 
# #55674  6186 
# nrow(data_smote_2_10) #61860
# #Hence the minority class percentage is 10%
# 
# #Running loagistic regression with default parameters.
# logistic_default_2_10 <- glm(Performance.Tag ~.,
#                              data = data_smote_2_10,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_2_10_probs <- predict(logistic_default_2_10, full_test, type = "response")
# prediction_logistic_2_10_probs_summary<-summary(prediction_logistic_2_10_probs)
# #Setting the cutoff at median.
# prediction_logistic_2_10 <- ifelse(prediction_logistic_2_10_probs >=prediction_logistic_2_10_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_2_10 <- confusionMatrix(factor(prediction_logistic_2_10), factor(test_results), positive = '1')
# test_conf_logistic_2_10
# #summary(test)
# 
# library(rpart)
# library(varhandle)
# tree_default_2_10<-rpart(Performance.Tag~.,data=data_smote_2_10, method= "class")
# plot(tree_default_2_10)
# tree_pred_2_10<-predict(tree_default_2_10, full_test, type = "class")
# tree_pred_2_10<-unfactor(tree_pred_2_10)
# tree_pred_2_10<-ifelse(tree_pred_2_10==1,"1","0")
# test_conf_tree_2_10<-confusionMatrix(factor(tree_pred_2_10),factor(test_results), positive  = '1')
# test_conf_tree_2_10
# 
# #Random Forest:
# library(randomForest)
# forest_default_2_10 <- randomForest(Performance.Tag ~., data = data_smote_2_10, proximity = F, do.trace = T)
# forest_pred_2_10<-predict(forest_default_2_10, full_test, type = "class")
# forest_pred_2_10<-unfactor(forest_pred_2_10)
# forest_pred_2_10<-ifelse(forest_pred_2_10==1,"1","0")
# summary(factor(forest_pred_2_10))
# forest_conf_tree_2_10<-confusionMatrix(factor(forest_pred_2_10),factor(test_results), positive  = '1')
# forest_conf_tree_2_10
# 
# #Running to check if 20% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_2_20 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 200, perc.under = 600)
# summary(data_smote_2_20$Performance.Tag)
# #0       1 
# #24744  6186 
# nrow(data_smote_2_20) #30930
# #Hence the minority class percentage is 20%
# #Running loagistic regression with default parameters.
# logistic_default_2_20 <- glm(Performance.Tag ~.,
#                              data = data_smote_2_20,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_2_20_probs <- predict(logistic_default_2_20, full_test, type = "response")
# prediction_logistic_2_20_probs_summary<-summary(prediction_logistic_2_20_probs)
# #Setting the cutoff at median.
# prediction_logistic_2_20 <- ifelse(prediction_logistic_2_20_probs >=prediction_logistic_2_20_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_2_20 <- confusionMatrix(factor(prediction_logistic_2_20), factor(test_results), positive = '1')
# test_conf_logistic_2_20
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_2_20<-rpart(Performance.Tag~.,data=data_smote_2_20, method= "class")
# plot(tree_default_2_20)
# tree_pred_2_20<-predict(tree_default_2_20, full_test, type = "class")
# tree_pred_2_20<-unfactor(tree_pred_2_20)
# tree_pred_2_20<-ifelse(tree_pred_2_20==1,"1","0")
# test_conf_tree_2_20<-confusionMatrix(factor(tree_pred_2_20),factor(test_results), positive  = '1')
# test_conf_tree_2_20
# 
# #Random Forest:
# library(randomForest)
# forest_default_2_20 <- randomForest(Performance.Tag ~., data = data_smote_2_20, proximity = F, do.trace = T)
# forest_pred_2_20<-predict(forest_default_2_20, full_test, type = "class")
# forest_pred_2_20<-unfactor(forest_pred_2_20)
# forest_pred_2_20<-ifelse(forest_pred_2_20==1,"1","0")
# summary(factor(forest_pred_2_20))
# forest_conf_tree_2_20<-confusionMatrix(factor(forest_pred_2_20),factor(test_results), positive  = '1')
# forest_conf_tree_2_20
# 
# 
# #Running to check if 30% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_2_30 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 200, perc.under = 350)
# summary(data_smote_2_30$Performance.Tag)
# #0     1 
# #14434  6186
# nrow(data_smote_2_30) #20620
# #Hence the minority class percentage is 30%
# #Running loagistic regression with default parameters.
# logistic_default_2_30 <- glm(Performance.Tag ~.,
#                              data = data_smote_2_30,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_2_30_probs <- predict(logistic_default_2_30, full_test, type = "response")
# prediction_logistic_2_30_probs_summary<-summary(prediction_logistic_2_30_probs)
# #Setting the cutoff at median.
# prediction_logistic_2_30 <- ifelse(prediction_logistic_2_30_probs >=prediction_logistic_2_30_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_2_30 <- confusionMatrix(factor(prediction_logistic_2_30), factor(test_results), positive = '1')
# test_conf_logistic_2_30
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_2_30<-rpart(Performance.Tag~.,data=data_smote_2_30, method= "class")
# plot(tree_default_2_30)
# tree_pred_2_30<-predict(tree_default_2_30, full_test, type = "class")
# tree_pred_2_30<-unfactor(tree_pred_2_30)
# tree_pred_2_30<-ifelse(tree_pred_2_30==1,"1","0")
# test_conf_tree_2_30<-confusionMatrix(factor(tree_pred_2_30),factor(test_results), positive  = '1')
# test_conf_tree_2_30
# 
# #Random Forest:
# library(randomForest)
# forest_default_2_30 <- randomForest(Performance.Tag ~., data = data_smote_2_30, proximity = F, do.trace = T)
# forest_pred_2_30<-predict(forest_default_2_30, full_test, type = "class")
# forest_pred_2_30<-unfactor(forest_pred_2_30)
# forest_pred_2_30<-ifelse(forest_pred_2_30==1,"1","0")
# summary(factor(forest_pred_2_30))
# forest_conf_tree_2_30<-confusionMatrix(factor(forest_pred_2_30),factor(test_results), positive  = '1')
# forest_conf_tree_2_30
# 
# 
# #Running to check if 40% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_2_40 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 200, perc.under = 220)
# summary(data_smote_2_40$Performance.Tag)
# #0    1 
# #9072 6186 
# nrow(data_smote_2_40) #15258
# #Hence the minority class percentage is 40%
# #Running loagistic regression with default parameters.
# logistic_default_2_40 <- glm(Performance.Tag ~.,
#                              data = data_smote_2_40,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_2_40_probs <- predict(logistic_default_2_40, full_test, type = "response")
# prediction_logistic_2_40_probs_summary<-summary(prediction_logistic_2_40_probs)
# #Setting the cutoff at median.
# prediction_logistic_2_40 <- ifelse(prediction_logistic_2_40_probs >=prediction_logistic_2_40_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_2_40 <- confusionMatrix(factor(prediction_logistic_2_40), factor(test_results), positive = '1')
# test_conf_logistic_2_40
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_2_40<-rpart(Performance.Tag~.,data=data_smote_2_40, method= "class")
# plot(tree_default_2_40)
# tree_pred_2_40<-predict(tree_default_2_40, full_test, type = "class")
# tree_pred_2_40<-unfactor(tree_pred_2_40)
# tree_pred_2_40<-ifelse(tree_pred_2_40==1,"1","0")
# test_conf_tree_2_40<-confusionMatrix(factor(tree_pred_2_40),factor(test_results), positive  = '1')
# test_conf_tree_2_40
# 
# 
# #Random Forest:
# library(randomForest)
# forest_default_2_40 <- randomForest(Performance.Tag ~., data = data_smote_2_40, proximity = F, do.trace = T)
# forest_pred_2_40<-predict(forest_default_2_40, full_test, type = "class")
# forest_pred_2_40<-unfactor(forest_pred_2_40)
# forest_pred_2_40<-ifelse(forest_pred_2_40==1,"1","0")
# summary(factor(forest_pred_2_40))
# forest_conf_tree_2_40<-confusionMatrix(factor(forest_pred_2_40),factor(test_results), positive  = '1')
# forest_conf_tree_2_40
# 
# 
# #Running to check if 50% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_2_50 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 200, perc.under = 150)
# summary(data_smote_2_50$Performance.Tag)
# #0    1 
# #6186 6186
# nrow(data_smote_2_50)   #12372
# #Hence the minority class percentage is 50%
# 
# #Running loagistic regression with default parameters.
# logistic_default_2_50 <- glm(Performance.Tag ~.,
#                              data = data_smote_2_50,
#                              family = 'binomial')
# 
# summary(logistic_default_2_50) 
# 
# prediction_logistic_2_50_probs <- predict(logistic_default_2_50, full_test, type = "response")
# prediction_logistic_2_50_probs_summary<-summary(prediction_logistic_2_50_probs)
# #Setting the cutoff at median.
# prediction_logistic_2_50 <- ifelse(prediction_logistic_2_50_probs >=prediction_logistic_2_50_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_2_50 <- confusionMatrix(factor(prediction_logistic_2_50), factor(test_results), positive = '1')
# test_conf_logistic_2_50
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_2_50<-rpart(Performance.Tag~.,data=data_smote_2_50, method= "class")
# plot(tree_default_2_50)
# tree_pred_2_50<-predict(tree_default_2_50, full_test, type = "class")
# tree_pred_2_50<-unfactor(tree_pred_2_50)
# tree_pred_2_50<-ifelse(tree_pred_2_50==1,"1","0")
# test_conf_tree_2_50<-confusionMatrix(factor(tree_pred_2_50),factor(test_results), positive  = '1')
# test_conf_tree_2_50
# 
# #Random Forest:
# library(randomForest)
# forest_default_2_50 <- randomForest(Performance.Tag ~., data = data_smote_2_50, proximity = F, do.trace = T)
# forest_pred_2_50<-predict(forest_default_2_50, full_test, type = "class")
# forest_pred_2_50<-unfactor(forest_pred_2_50)
# forest_pred_2_50<-ifelse(forest_pred_2_50==1,"1","0")
# summary(factor(forest_pred_2_50))
# forest_conf_tree_2_50<-confusionMatrix(factor(forest_pred_2_50),factor(test_results), positive  = '1')
# forest_conf_tree_2_50
# 
# 
# 
# 
# #Running to check if 60% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_2_60 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 200, perc.under = 100)
# summary(data_smote_2_60$Performance.Tag)
# #   0    1 
# #4124 6186
# nrow(data_smote_2_60)   #10310
# #Hence the minority class percentage is 60%
# 
# #Running loagistic regression with default parameters.
# logistic_default_2_60 <- glm(Performance.Tag ~.,
#                              data = data_smote_2_60,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_2_60_probs <- predict(logistic_default_2_60, full_test, type = "response")
# prediction_logistic_2_60_probs_summary<-summary(prediction_logistic_2_60_probs)
# #Setting the cutoff at median.
# prediction_logistic_2_60 <- ifelse(prediction_logistic_2_60_probs >=prediction_logistic_2_60_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_2_60 <- confusionMatrix(factor(prediction_logistic_2_60), factor(test_results), positive = '1')
# test_conf_logistic_2_60
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_2_60<-rpart(Performance.Tag~.,data=data_smote_2_60, method= "class")
# plot(tree_default_2_60)
# tree_pred_2_60<-predict(tree_default_2_60, full_test, type = "class")
# tree_pred_2_60<-unfactor(tree_pred_2_60)
# tree_pred_2_60<-ifelse(tree_pred_2_60==1,"1","0")
# test_conf_tree_2_60<-confusionMatrix(factor(tree_pred_2_60),factor(test_results), positive  = '1')
# test_conf_tree_2_60
# 
# 
# #Random Forest:
# library(randomForest)
# forest_default_2_60 <- randomForest(Performance.Tag ~., data = data_smote_2_60, proximity = F, do.trace = T)
# forest_pred_2_60<-predict(forest_default_2_60, full_test, type = "class")
# forest_pred_2_60<-unfactor(forest_pred_2_60)
# forest_pred_2_60<-ifelse(forest_pred_2_60==1,"1","0")
# summary(factor(forest_pred_2_60))
# forest_conf_tree_2_60<-confusionMatrix(factor(forest_pred_2_60),factor(test_results), positive  = '1')
# forest_conf_tree_2_60
# 
# #II. Trying to oversample the minonity class by three fold. Will be doing for three fold in detail, as the number of records
# #    having defaulters will increase so the sensitvity could increase and the number of records will be more so the overall
# #    accuracy could increase.
# 
# #Running to check if 10% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_3_10 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 300, perc.under = 1200)
# summary(data_smote_3_10$Performance.Tag)
# #0     1 
# #74232  8248 
# nrow(data_smote_3_10) #82480
# #Hence the minority class percentage is 10%
# 
# #Running loagistic regression with default parameters.
# logistic_default_3_10 <- glm(Performance.Tag ~.,
#                              data = data_smote_3_10,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_3_10_probs <- predict(logistic_default_3_10, full_test, type = "response")
# prediction_logistic_3_10_probs_summary<-summary(prediction_logistic_3_10_probs)
# #Setting the cutoff at median.
# prediction_logistic_3_10 <- ifelse(prediction_logistic_3_10_probs >=prediction_logistic_3_10_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_3_10 <- confusionMatrix(factor(prediction_logistic_3_10), factor(test_results), positive = '1')
# test_conf_logistic_3_10
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_3_10<-rpart(Performance.Tag~.,data=data_smote_3_10, method= "class")
# plot(tree_default_3_10)
# tree_pred_3_10<-predict(tree_default_3_10, full_test, type = "class")
# tree_pred_3_10<-unfactor(tree_pred_3_10)
# tree_pred_3_10<-ifelse(tree_pred_3_10==1,"1","0")
# test_conf_tree_3_10<-confusionMatrix(factor(tree_pred_3_10),factor(test_results), positive  = '1')
# test_conf_tree_3_10
# 
# #Random Forest:
# library(randomForest)
# forest_default_3_10 <- randomForest(Performance.Tag ~., data = data_smote_3_10, proximity = F, do.trace = T)
# forest_pred_3_10<-predict(forest_default_3_10, full_test, type = "class")
# forest_pred_3_10<-unfactor(forest_pred_3_10)
# forest_pred_3_10<-ifelse(forest_pred_3_10==1,"1","0")
# summary(factor(forest_pred_3_10))
# forest_conf_tree_3_10<-confusionMatrix(factor(forest_pred_3_10),factor(test_results), positive  = '1')
# forest_conf_tree_3_10
# 
# #Running to check if 20% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_3_20 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 300, perc.under = 532)
# summary(data_smote_3_20$Performance.Tag)
# #0     1 
# #32909  8248 
# nrow(data_smote_3_20) #41157
# #Hence the minority class percentage is 20%
# #Running loagistic regression with default parameters.
# logistic_default_3_20 <- glm(Performance.Tag ~.,
#                              data = data_smote_3_20,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_3_20_probs <- predict(logistic_default_3_20, full_test, type = "response")
# prediction_logistic_3_20_probs_summary<-summary(prediction_logistic_3_20_probs)
# #Setting the cutoff at median.
# prediction_logistic_3_20 <- ifelse(prediction_logistic_3_20_probs >=prediction_logistic_3_20_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_3_20 <- confusionMatrix(factor(prediction_logistic_3_20), factor(test_results), positive = '1')
# test_conf_logistic_3_20
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_3_20<-rpart(Performance.Tag~.,data=data_smote_3_20, method= "class")
# plot(tree_default_3_20)
# tree_pred_3_20<-predict(tree_default_3_20, full_test, type = "class")
# tree_pred_3_20<-unfactor(tree_pred_3_20)
# tree_pred_3_20<-ifelse(tree_pred_3_20==1,"1","0")
# test_conf_tree_3_20<-confusionMatrix(factor(tree_pred_3_20),factor(test_results), positive  = '1')
# test_conf_tree_3_20
# 
# #Random Forest:
# library(randomForest)
# forest_default_3_20 <- randomForest(Performance.Tag ~., data = data_smote_3_20, proximity = F, do.trace = T)
# forest_pred_3_20<-predict(forest_default_3_20, full_test, type = "class")
# forest_pred_3_20<-unfactor(forest_pred_3_20)
# forest_pred_3_20<-ifelse(forest_pred_3_20==1,"1","0")
# summary(factor(forest_pred_3_20))
# forest_conf_tree_3_20<-confusionMatrix(factor(forest_pred_3_20),factor(test_results), positive  = '1')
# forest_conf_tree_3_20
# 
# 
# #Running to check if 30% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_3_30 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 300, perc.under = 310)
# summary(data_smote_3_30$Performance.Tag)
# #    0     1 
# #19176  8248
# nrow(data_smote_3_30) #27424
# #Hence the minority class percentage is 30%
# #Running loagistic regression with default parameters.
# logistic_default_3_30 <- glm(Performance.Tag ~.,
#                              data = data_smote_3_30,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_3_30_probs <- predict(logistic_default_3_30, full_test, type = "response")
# prediction_logistic_3_30_probs_summary<-summary(prediction_logistic_3_30_probs)
# #Setting the cutoff at median.
# prediction_logistic_3_30 <- ifelse(prediction_logistic_3_30_probs >=prediction_logistic_3_30_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_3_30 <- confusionMatrix(factor(prediction_logistic_3_30), factor(test_results), positive = '1')
# test_conf_logistic_3_30
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_3_30<-rpart(Performance.Tag~.,data=data_smote_3_30, method= "class")
# plot(tree_default_3_30)
# tree_pred_3_30<-predict(tree_default_3_30, full_test, type = "class")
# tree_pred_3_30<-unfactor(tree_pred_3_30)
# tree_pred_3_30<-ifelse(tree_pred_3_30==1,"1","0")
# test_conf_tree_3_30<-confusionMatrix(factor(tree_pred_3_30),factor(test_results), positive  = '1')
# test_conf_tree_3_30
# 
# #Random Forest:
# library(randomForest)
# forest_default_3_30 <- randomForest(Performance.Tag ~., data = data_smote_3_30, proximity = F, do.trace = T)
# forest_pred_3_30<-predict(forest_default_3_30, full_test, type = "class")
# forest_pred_3_30<-unfactor(forest_pred_3_30)
# forest_pred_3_30<-ifelse(forest_pred_3_30==1,"1","0")
# summary(factor(forest_pred_3_30))
# forest_conf_tree_3_30<-confusionMatrix(factor(forest_pred_3_30),factor(test_results), positive  = '1')
# forest_conf_tree_3_30
# 
# 
# #Running to check if 40% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_3_40 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 300, perc.under = 200)
# summary(data_smote_3_40$Performance.Tag)
# #   0     1 
# #12372  8248
# nrow(data_smote_3_40) #20620
# #Hence the minority class percentage is 40%
# #Running loagistic regression with default parameters.
# logistic_default_3_40 <- glm(Performance.Tag ~.,
#                              data = data_smote_3_40,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_3_40_probs <- predict(logistic_default_3_40, full_test, type = "response")
# prediction_logistic_3_40_probs_summary<-summary(prediction_logistic_3_40_probs)
# #Setting the cutoff at median.
# prediction_logistic_3_40 <- ifelse(prediction_logistic_3_40_probs >=prediction_logistic_3_40_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_3_40 <- confusionMatrix(factor(prediction_logistic_3_40), factor(test_results), positive = '1')
# test_conf_logistic_3_40
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_3_40<-rpart(Performance.Tag~.,data=data_smote_3_40, method= "class")
# plot(tree_default_3_40)
# tree_pred_3_40<-predict(tree_default_3_40, full_test, type = "class")
# tree_pred_3_40<-unfactor(tree_pred_3_40)
# tree_pred_3_40<-ifelse(tree_pred_3_40==1,"1","0")
# test_conf_tree_3_40<-confusionMatrix(factor(tree_pred_3_40),factor(test_results), positive  = '1')
# test_conf_tree_3_40
# 
# 
# #Random Forest:
# library(randomForest)
# forest_default_3_40 <- randomForest(Performance.Tag ~., data = data_smote_3_40, proximity = F, do.trace = T)
# forest_pred_3_40<-predict(forest_default_3_40, full_test, type = "class")
# forest_pred_3_40<-unfactor(forest_pred_3_40)
# forest_pred_3_40<-ifelse(forest_pred_3_40==1,"1","0")
# summary(factor(forest_pred_3_40))
# forest_conf_tree_3_40<-confusionMatrix(factor(forest_pred_3_40),factor(test_results), positive  = '1')
# forest_conf_tree_3_40
# 
# 
# #Running to check if 50% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_3_50 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 300, perc.under = 130)
# summary(data_smote_3_50$Performance.Tag)
# #   0    1 
# #8041 8248
# nrow(data_smote_3_50) #16289
# #Hence the minority class percentage is 50%
# #Running loagistic regression with default parameters.
# logistic_default_3_50 <- glm(Performance.Tag ~.,
#                              data = data_smote_3_50,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_3_50_probs <- predict(logistic_default_3_50, full_test, type = "response")
# prediction_logistic_3_50_probs_summary<-summary(prediction_logistic_3_50_probs)
# #Setting the cutoff at median.
# prediction_logistic_3_50 <- ifelse(prediction_logistic_3_50_probs >=prediction_logistic_3_50_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_3_50 <- confusionMatrix(factor(prediction_logistic_3_50), factor(test_results), positive = '1')
# test_conf_logistic_3_50
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_3_50<-rpart(Performance.Tag~.,data=data_smote_3_50, method= "class")
# plot(tree_default_3_50)
# tree_pred_3_50<-predict(tree_default_3_50, full_test, type = "class")
# tree_pred_3_50<-unfactor(tree_pred_3_50)
# tree_pred_3_50<-ifelse(tree_pred_3_50==1,"1","0")
# test_conf_tree_3_50<-confusionMatrix(factor(tree_pred_3_50),factor(test_results), positive  = '1')
# test_conf_tree_3_50
# 
# 
# #Random Forest:
# library(randomForest)
# forest_default_3_50 <- randomForest(Performance.Tag ~., data = data_smote_3_50, proximity = F, do.trace = T)
# forest_pred_3_50<-predict(forest_default_3_50, full_test, type = "class")
# forest_pred_3_50<-unfactor(forest_pred_3_50)
# forest_pred_3_50<-ifelse(forest_pred_3_50==1,"1","0")
# summary(factor(forest_pred_3_50))
# forest_conf_tree_3_50<-confusionMatrix(factor(forest_pred_3_50),factor(test_results), positive  = '1')
# forest_conf_tree_3_50
# 
# #Running to check if 60% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_3_60 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 300, perc.under = 89)
# summary(data_smote_3_60$Performance.Tag)
# #0    1 
# #5505 8248 
# nrow(data_smote_3_60) #13753
# #Hence the minority class percentage is 50%
# #Running loagistic regression with default parameters.
# logistic_default_3_60 <- glm(Performance.Tag ~.,
#                              data = data_smote_3_60,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_3_60_probs <- predict(logistic_default_3_60, full_test, type = "response")
# prediction_logistic_3_60_probs_summary<-summary(prediction_logistic_3_60_probs)
# #Setting the cutoff at median.
# prediction_logistic_3_60 <- ifelse(prediction_logistic_3_60_probs >=prediction_logistic_3_60_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_3_60 <- confusionMatrix(factor(prediction_logistic_3_60), factor(test_results), positive = '1')
# test_conf_logistic_3_60
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_3_60<-rpart(Performance.Tag~.,data=data_smote_3_60, method= "class")
# plot(tree_default_3_60)
# tree_pred_3_60<-predict(tree_default_3_60, full_test, type = "class")
# tree_pred_3_60<-unfactor(tree_pred_3_60)
# tree_pred_3_60<-ifelse(tree_pred_3_60==1,"1","0")
# test_conf_tree_3_60<-confusionMatrix(factor(tree_pred_3_60),factor(test_results), positive  = '1')
# test_conf_tree_3_60
# 
# 
# #Random Forest:
# library(randomForest)
# forest_default_3_60 <- randomForest(Performance.Tag ~., data = data_smote_3_60, proximity = F, do.trace = T)
# forest_pred_3_60<-predict(forest_default_3_60, full_test, type = "class")
# forest_pred_3_60<-unfactor(forest_pred_3_60)
# forest_pred_3_60<-ifelse(forest_pred_3_60==1,"1","0")
# summary(factor(forest_pred_3_60))
# forest_conf_tree_3_60<-confusionMatrix(factor(forest_pred_3_60),factor(test_results), positive  = '1')
# forest_conf_tree_3_60
# 
# 
# #Running to check if 60% of the minority class in the balanced training dataframe will be suitable.
# set.seed(seed)
# data_smote_3_70 <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 300, perc.under = 58)
# summary(data_smote_3_70$Performance.Tag)
# #0    1 
# #5505 8248 
# nrow(data_smote_3_70) #11835
# #Hence the minority class percentage is 50%
# #Running loagistic regression with default parameters.
# logistic_default_3_70 <- glm(Performance.Tag ~.,
#                              data = data_smote_3_70,
#                              family = 'binomial')
# 
# 
# 
# prediction_logistic_3_70_probs <- predict(logistic_default_3_70, full_test, type = "response")
# prediction_logistic_3_70_probs_summary<-summary(prediction_logistic_3_70_probs)
# #Setting the cutoff at median.
# prediction_logistic_3_70 <- ifelse(prediction_logistic_3_70_probs >=prediction_logistic_3_70_probs_summary[3], '1', '0')
# test_results <- as.character(full_test$Performance.Tag)
# test_conf_logistic_3_70 <- confusionMatrix(factor(prediction_logistic_3_70), factor(test_results), positive = '1')
# test_conf_logistic_3_70
# 
# #Decision Trees
# library(rpart)
# library(varhandle)
# tree_default_3_70<-rpart(Performance.Tag~.,data=data_smote_3_70, method= "class")
# plot(tree_default_3_70)
# tree_pred_3_70<-predict(tree_default_3_70, full_test, type = "class")
# tree_pred_3_70<-unfactor(tree_pred_3_70)
# tree_pred_3_70<-ifelse(tree_pred_3_70==1,"1","0")
# test_conf_tree_3_70<-confusionMatrix(factor(tree_pred_3_70),factor(test_results), positive  = '1')
# test_conf_tree_3_70
# 
# 
# #Random Forest:
# library(randomForest)
# forest_default_3_70 <- randomForest(Performance.Tag ~., data = data_smote_3_70, proximity = F, do.trace = T)
# forest_pred_3_70<-predict(forest_default_3_70, full_test, type = "class")
# forest_pred_3_70<-unfactor(forest_pred_3_70)
# forest_pred_3_70<-ifelse(forest_pred_3_70==1,"1","0")
# summary(factor(forest_pred_3_70))
# forest_conf_tree_3_70<-confusionMatrix(factor(forest_pred_3_70),factor(test_results), positive  = '1')
# forest_conf_tree_3_70


#NOTE : We have chosen oversampling parameter 300 and undersampling parameter 130 as optimum values for SMOTE


full_train_smoted <- SMOTE(Performance.Tag ~. , data = full_train, perc.over = 300, perc.under = 130)
dem_train_smoted <- SMOTE(Performance.Tag ~. , data = dem_train, perc.over = 300, perc.under = 130)

summary(full_train_smoted$Performance.Tag)
summary(dem_train_smoted$Performance.Tag)

# We can see that the minority class percentage has increased and should be valuable for analysis purposes







# WE WILL FIRST CREATE AND EVALUATE THE MODEL FOR DEMOGRAPHIC DATA ONLY


#==============================================================================
#                ANALYSING ONLY DEMOGRAPHIC DATA
#==============================================================================



#==== LOGISTIC REGRESSION : DEMOGRAPHIC DATA ====

dem_logistic_model_1 <- glm(Performance.Tag ~.,
                            data = dem_train_smoted,
                            family = 'binomial')

summary(dem_logistic_model_1)



# we can see that there are no insignificant values. Hence this will be the Logistic Regression's Final model 

# Since all the variables are highly significant now, hence we will take this as final logistic model
dem_logistic_model_final <- dem_logistic_model_1





#==== MODEL EVALUATION : LOGISTIC REGRESSION : DEMOGRAPHIC DATA ====
#     ---------------------------------------------------------


# Running the model on test data to see performance

prediction_dem_logistic <- predict(dem_logistic_model_final, dem_test, type = "response")

# Finding out optimal cutoff
dem_logistic_perform_fn <- function(cutoff) 
{
  predicted_response <- ifelse(prediction_dem_logistic >= cutoff, "1", "0")
  test_results <- as.character(dem_test$Performance.Tag)
  conf <- confusionMatrix(factor(predicted_response), factor(dem_test$Performance.Tag), positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

dem_logistic_s = seq(.01,.99,length=100)

dem_logistic_OUT = matrix(0,100,3)


for(i in 1:100){
  dem_logistic_OUT[i,] = dem_logistic_perform_fn(dem_logistic_s[i])
} 


# plotting cutoffs 
par(mfrow=c(1,1), mar=c(3,2,2,2)) 
plot(dem_logistic_s, dem_logistic_OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1,cex.axis=1,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1)
lines(dem_logistic_s,dem_logistic_OUT[,2],col="darkgreen",lwd=2)
lines(dem_logistic_s,dem_logistic_OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


dem_logistic_cutoff <- dem_logistic_s[which(abs(dem_logistic_OUT[,1]-dem_logistic_OUT[,2])<0.1)]  
dem_logistic_cutoff <- dem_logistic_cutoff[length(dem_logistic_cutoff)]
dem_logistic_cutoff

#After trying the above values the best cut off value is stored in dem_logistic_cutoff
dem_logistic_predicted_response <- factor(ifelse(prediction_dem_logistic >= dem_logistic_cutoff, "1", "0"))

dem_logistic_conf_final <- confusionMatrix(factor(dem_logistic_predicted_response), factor(dem_test$Performance.Tag), positive = "1")
dem_logistic_conf_final

#Accuracy     0.5734
#Sensitivity  0.59708
#Specificity  0.57193



# KS Statistics

dem_logistic_pred_object_test <- prediction(as.numeric(as.character(dem_logistic_predicted_response)), as.numeric(as.character(dem_test$Performance.Tag)))
dem_logistic_performance_measures_test <- performance(dem_logistic_pred_object_test, "tpr", "fpr")  

dem_logistic_ks_table_test <- attr(dem_logistic_performance_measures_test, "y.values")[[1]] - 
  (attr(dem_logistic_performance_measures_test, "x.values")[[1]])

max(dem_logistic_ks_table_test)  # 0.166314

plot(dem_logistic_performance_measures_test,main=paste0(' KS=',round(max(dem_logistic_ks_table_test*100,1)),'%'), colorize = T)
lines(x=c(0,1),y=c(0,1))

# Area under the curve
dem_logistic_auc <- performance(dem_logistic_pred_object_test, "auc")
dem_logistic_auc@y.values[[1]] # 0.5831572


# Lift and Gain Chart

dem_logistic_performance_measures_test <- performance(dem_logistic_pred_object_test, "lift", "rpp")
plot(dem_logistic_performance_measures_test)

lift <- function(labels,predicted_prob,groups=10){
  if(is.factor(labels)){labels <- as.integer(as.character(labels))}
  if(is.factor(predicted_prob)){predicted_prob <- as.integer(as.character(predicted_prob))}
  helper = data.frame(cbind(labels,predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"],groups)
  gaintable = helper %>% group_by(bucket) %>%
    summarise_at(vars(labels ),funs(total = n(),totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),Gain=Cumresp/sum(totalresp)*100,Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

dem_logistic_default_decile = lift(as.numeric(as.character(dem_test$Performance.Tag)), as.numeric(as.character(dem_logistic_predicted_response)), groups = 10)
View(dem_logistic_default_decile)  

# K Fold - Cross Validation
cv.binary(dem_logistic_model_final, nfolds = 100)

# We see that the accuracy is consistent after 100 folds, hence we conclude that the model is quite stable







# For Test data including rejected applications
#------------------------------------------------


# Running the model on test data that includes rejected applications to see performance

prediction_dem_logistic_incl_rejects <- predict(dem_logistic_model_final, dem_test_incl_rejects, type = "response")

# Finding out optimal cutoff
dem_logistic_perform_fn_incl_rejects <- function(cutoff) 
{
  predicted_response <- ifelse(prediction_dem_logistic_incl_rejects >= cutoff, "1", "0")
  test_results <- as.character(dem_test_incl_rejects$Performance.Tag)
  conf <- confusionMatrix(factor(predicted_response), factor(dem_test_incl_rejects$Performance.Tag), positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

dem_logistic_s_incl_rejects = seq(.01,.99,length=100)

dem_logistic_OUT_incl_rejects = matrix(0,100,3)


for(i in 1:100){
  dem_logistic_OUT_incl_rejects[i,] = dem_logistic_perform_fn_incl_rejects(dem_logistic_s_incl_rejects[i])
}


# plotting cutoffs 
par(mfrow=c(1,1), mar=c(3,2,2,2)) 
plot(dem_logistic_s_incl_rejects, dem_logistic_OUT_incl_rejects[,1],xlab="Cutoff",ylab="Value",cex.lab=1,cex.axis=1,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1)
lines(dem_logistic_s_incl_rejects,dem_logistic_OUT_incl_rejects[,2],col="darkgreen",lwd=2)
lines(dem_logistic_s_incl_rejects,dem_logistic_OUT_incl_rejects[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


dem_logistic_cutoff_incl_rejects <- dem_logistic_s_incl_rejects[which(abs(dem_logistic_OUT_incl_rejects[,1]-dem_logistic_OUT_incl_rejects[,2])<0.1)]  
dem_logistic_cutoff_incl_rejects <- dem_logistic_cutoff_incl_rejects[length(dem_logistic_cutoff_incl_rejects)]
dem_logistic_cutoff_incl_rejects

#After trying the above values the best cut off value  is 0.5148485
dem_logistic_predicted_response_incl_rejects <- factor(ifelse(prediction_dem_logistic_incl_rejects >= dem_logistic_cutoff_incl_rejects, "1", "0"))

dem_logistic_conf_final_incl_rejects <- confusionMatrix(factor(dem_logistic_predicted_response_incl_rejects), factor(dem_test_incl_rejects$Performance.Tag), positive = "1")
dem_logistic_conf_final_incl_rejects

#Accuracy     68.34 %
#Sensitivity  62.38 %
#Specificity  69.03 %


# KS Statistics

dem_logistic_pred_object_test_incl_rejects <- prediction(as.numeric(as.character(dem_logistic_predicted_response_incl_rejects)), as.numeric(as.character(dem_test_incl_rejects$Performance.Tag)))
dem_logistic_performance_measures_test_incl_rejects <- performance(dem_logistic_pred_object_test_incl_rejects, "tpr", "fpr")  

dem_logistic_ks_table_test_incl_rejects <- attr(dem_logistic_performance_measures_test_incl_rejects, "y.values")[[1]] - 
  (attr(dem_logistic_performance_measures_test_incl_rejects, "x.values")[[1]])

max(dem_logistic_ks_table_test_incl_rejects)  # 0.3140488

plot(dem_logistic_performance_measures_test_incl_rejects,main=paste0(' KS=',round(max(dem_logistic_ks_table_test_incl_rejects*100,1)),'%'), colorize = T)
lines(x=c(0,1),y=c(0,1))

# Area under the curve
dem_auc_incl_rejects <- performance(dem_logistic_pred_object_test_incl_rejects, "auc")
dem_auc_incl_rejects@y.values[[1]] # 0.6570244

# Lift and Gain Chart

dem_logistic_performance_measures_test_incl_rejects <- performance(dem_logistic_pred_object_test_incl_rejects, "lift", "rpp")
plot(dem_logistic_performance_measures_test_incl_rejects)

lift <- function(labels,predicted_prob,groups=10){
  if(is.factor(labels)){labels <- as.integer(as.character(labels))}
  if(is.factor(predicted_prob)){predicted_prob <- as.integer(as.character(predicted_prob))}
  helper = data.frame(cbind(labels,predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"],groups)
  gaintable = helper %>% group_by(bucket) %>%
    summarise_at(vars(labels ),funs(total = n(),totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),Gain=Cumresp/sum(totalresp)*100,Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

dem_logistic_default_decile_incl_rejects = lift(as.numeric(as.character(dem_test_incl_rejects$Performance.Tag)), as.numeric(as.character(dem_logistic_predicted_response_incl_rejects)), groups = 10)
View(dem_logistic_default_decile_incl_rejects)  

# K Fold - Cross Validation
cv.binary(dem_logistic_model_final, nfolds = 100)
# Internal estimate of accuracy = 0.789
# Cross-validation estimate of accuracy = 0.789
# We see that the accuracy is quite high after 100 folds, hence we conclude that the model is quite stable




#==== DECISION TREE : DEMO DATA ====
#     -------------------------


dem_tree <- rpart(Performance.Tag ~ .,data = dem_train_smoted, method="class")
summary(dem_tree)

plot(dem_tree)
text(dem_tree, pretty=2)

dem_prediction_tree <- predict(dem_tree, dem_test_incl_rejects, type="class")


dem_conf_tree <- confusionMatrix(factor(dem_prediction_tree), factor(dem_test_incl_rejects$Performance.Tag), positive = "0")
dem_conf_tree

#Accuracy       61.48%
#Sensitivity    61.28%
#Specificity    63.20%

#Trying to find the optimal complexity parameter value.
printcp(dem_tree)
plotcp(dem_tree)

# Setting the CP value, with least error

bestcp <- dem_tree$cptable[which.min(dem_tree$cptable[,"xerror"]),"CP"]
bestcp


# Pruning the tree based on the CP value

dem_tree_pruned <- prune(dem_tree, cp= bestcp)

plot(dem_tree_pruned)
text(dem_tree_pruned, pretty=2)

dem_prediction_tree_pruned <- predict(dem_tree_pruned, dem_test_incl_rejects, type="class")

dem_conf_tree_pruned <- confusionMatrix(factor(dem_prediction_tree_pruned), factor(dem_test_incl_rejects$Performance.Tag), positive = "1")
dem_conf_tree_pruned

# We have improved the Sensitivity by Pruning.

#Accuracy       61.48%
#Sensitivity    63.20%
#Specificity    61.28%




#====Random Forest=====



rf <- randomForest(Performance.Tag ~. , data = dem_train_smoted,  ntree = 1000)


prediction_rf <- predict(rf, dem_test_incl_rejects, type = 'class')


confusionMatrix(prediction_rf, dem_test_incl_rejects$Performance.Tag, positive = "1")


#Accuracy       61.48%
#Sensitivity    63.20%
#Specificity    61.28%



# Tuning the RF
# -------------


# Searching for optimal mtry. 
# We will take ntree as 1000 in order to reduce system constraints



# Grid Search

control <- trainControl(method="repeatedcv", number=3, repeats=3, search="grid")
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=c(3:7))
dem_rf_gridsearch <- train(Performance.Tag~., data=dem_train_smoted, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(dem_rf_gridsearch)
plot(dem_rf_gridsearch)


# Random Forest 
# 
# 16226 samples
# 4 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Cross-Validated (3 fold, repeated 3 times) 
# Summary of sample sizes: 10818, 10817, 10817, 10817, 10818, 10817, ... 
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy   Kappa    
# 3     0.5840011  0.1680532
# 4     0.5830150  0.1660042
# 5     0.5832205  0.1664186
# 6     0.5834670  0.1669438
# 7     0.5830151  0.1660205
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 3.

# Creating our final RF based on this mtry value.

dem_rf_tuned <- randomForest(Performance.Tag ~. , data = dem_train_smoted, mtry = 3, ntree = 1000)



# The next thing to do is set the correct cutoff after predicting


# Predict response for test data

dem_rf_pred_tuned <- predict(dem_rf_tuned, dem_test_incl_rejects, type = 'prob')

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response_rf_tuned <- as.factor(ifelse(dem_rf_pred_tuned[, 2] >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response_rf_tuned, dem_test_incl_rejects$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100){
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

# Looking at the various cutoffs

list <- as.data.frame(OUT_rf)
colnames(list) <- c("Sensitivity", "Specificity", "Accuracy")


# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.15)]
cutoff_rf <- cutoff_rf[1]
cutoff_rf


predicted_response_rf_tuned <- factor(ifelse(dem_rf_pred_tuned[, 2] >= cutoff_rf, "1", "0"))

conf_forest_dem <- confusionMatrix(predicted_response_rf_tuned, dem_test_incl_rejects$Performance.Tag, positive = "1")

conf_forest_dem



#Accuracy       58%
#Sensitivity    70%
#Specificity    56%

# We have tremendously improved the Sensitivity


# Model Metrics across all models for Demographic data

models_dem <- as.data.frame(matrix(c(dem_logistic_conf_final_incl_rejects$byClass[1], dem_conf_tree_pruned$byClass[1], conf_forest_dem$byClass[1],
                                     dem_logistic_conf_final_incl_rejects$byClass[2],dem_conf_tree_pruned$byClass[2],conf_forest_dem$byClass[2],
                                     dem_logistic_conf_final_incl_rejects$overall[1],dem_conf_tree_pruned$overall[1], conf_forest_dem$overall[1]),nrow =3 ,ncol=3,byrow=TRUE))


colnames(models_dem) <-  c( 'Logistic Regression', 'Decision Trees ', 'Random Forest')


models_dem$Metrics <- c('Sensitivity','Specificity',  'Accuracy')

kable(models_dem[,c(4,1,2,3)])

# |Metrics     | Logistic Regression| Decision Trees | Random Forest|
# |:-----------|-------------------:|---------------:|-------------:|
# |Sensitivity |           0.6237581|       0.6319654|     0.6863931|
# |Specificity |           0.6902907|       0.6127759|     0.5413545|
# |Accuracy    |           0.6833953|       0.6147647|     0.5563863|


# We have taken the liberty of building a scorecard with just the demographic data. 
# The Results have been commented out
# The Modelling of the combined dataset appears after this commented portion






# #===========================================================
# #               Application Scorecard
# #============================================================

# FORMULAE
# --------

#            _____________________________________________________
#           | SCORE = OFFSET + FACTOR +LN(ODDS)                   |
#           |                                                     |
#           | SCORE + PDO = OFFSET + FACTOR + LN(2 * ODDS)        |
#           |_____________________________________________________|


#pdo<-20
#pdo = Factor * ln (2)
#Hence, Factor = pdo / ln (2)

# fact<-20/log(2)
# fact
# offset<-400 -(28.8539 * log(10))
# offset
# #Running the fianl logistic regression model on the test set which did not have Performance.Tag as NA that is test_target_without_na
# predictions_without_na<-predict(dem_logistic_model_final,dem_test, type = "response")
# scorecard<-data.frame(P_Good=1-predictions_without_na)
# library(dplyr)
# scorecard<-mutate(scorecard, Odds_good = P_Good /(1-P_Good))
# scorecard<-mutate(scorecard, ln_Odds = log(Odds_good))
# scorecard$Original_Response<-dem_test$Performance.Tag
# scorecard<-mutate(scorecard, Score = offset+(fact*ln_Odds))
# 
# summary(scorecard)
# write.csv(scorecard,"scorecard.csv")
# #After trial and error the optimal cut off was found to be at 330.
# predicted<- factor(ifelse(scorecard$Score>330, "0", "1"))
# scorecard$Predicted_Response<-predicted
# conf_without_na<- confusionMatrix(factor(scorecard$Predicted_Response), factor(scorecard$Original_Response), positive = "0")
# conf_without_na
# 
# #Accuracy :     68.78 %
# #Sensitivity :  69.75 %         
# #Specificity :  47.05 %
# 
# 
# #Now creating for the records that were rejected during the first stage of screening, these were the ones which had
# #Performance.Tag as na. So we shall see how well will the model perform with the cut off of 345 on the dataframe test_target_with_na.
# 
# predictions_final_only_na<-predict(dem_logistic_model_final,dem_rejects_woe, type = "response")
# scorecard_Performance.Tag_na<-data.frame(P_Good=1-predictions_final_only_na)
# scorecard_Performance.Tag_na<-mutate(scorecard_Performance.Tag_na, Odds_good = P_Good /(1-P_Good))
# scorecard_Performance.Tag_na<-mutate(scorecard_Performance.Tag_na, ln_Odds = log(Odds_good))
# scorecard_Performance.Tag_na$Original_Response <- 1
# scorecard_Performance.Tag_na$Original_Response <- factor(as.numeric(as.character(scorecard_Performance.Tag_na$Original_Response)),levels = c(0,1))
# scorecard_Performance.Tag_na<-mutate(scorecard_Performance.Tag_na, Score = offset+(fact*ln_Odds))
# predicted_response_only_na<- factor(ifelse(scorecard_Performance.Tag_na$Score>=330, "0", "1"))
# scorecard_Performance.Tag_na$Predicted_response<-predicted_response_only_na
# conf_only_na<- confusionMatrix(predicted_response_only_na, scorecard_Performance.Tag_na$Original_Response, positive = "1")
# conf_only_na
# 
# 
# #Accuracy : 68.12 %
# 
# 
# 
# 
# #==================================
# # Assessing Financial Benefit
# #==================================
# 
# 
# dem_test_incl_rejects$probs <- predict(dem_logistic_model_final, dem_test_incl_rejects, type = "response")
# 
# 
# dem_test_incl_rejects$P_Good<- 1- dem_test_incl_rejects$probs
# 
# 
# dem_test_incl_rejects <- mutate(dem_test_incl_rejects, Odds_good =  P_Good /(1-P_Good))
# 
# 
# dem_test_incl_rejects<-mutate(dem_test_incl_rejects, Score = offset+(fact*log(Odds_good)))
# 
# 
# # Using Our Score Cutoff of 330
# 
# dem_test_incl_rejects$pred <- factor(ifelse(dem_test_incl_rejects$Score > 330, "0", "1"))
# 
# confusionMatrix(dem_test_incl_rejects$pred, dem_test_incl_rejects$Performance.Tag, positive = "1")
# 
# # Accuracy :    68.74 %
# # Sensitivity : 59.96 %        
# # Specificity : 69.75 %
# 
# test_subset <- dem_test_incl_rejects[, c('Application.ID', 'pred')]
# 
# 
# main_subset <- full_data[,c('Application.ID', 'Outstanding.Balance', 'Performance.Tag')]
# 
# 
# score_check <- merge(test_subset, main_subset, by ="Application.ID")
# 
# 
# # Assessing Benefit
# 
# 
# # Revenue Gain 
# 
# #1 Correct Defaulters
# 
# correct_pred <- score_check[score_check$Performance.Tag == score_check$pred,]
# 
# correct_pred_def <- correct_pred[correct_pred$Performance.Tag == 1,]
# 
# R1 <- sum(correct_pred_def$Outstanding.Balance, na.rm =T)
# R1
# 
# # Revenue Gain = 1,517,114,061
# 
# 
# # 2 Correct Non Defaulters
# 
# correct_pred_non <- correct_pred[correct_pred$Performance.Tag == 0,]
# 
# R2 <- sum(correct_pred_non$Outstanding.Balance, na.rm =T)
# R2
# 
# # Revenue Gain = 17,382,609,578
# 
# 
# 
# Total_R <- R1+R2
# Total_R
# 
# # Total Revenue Gained = 18,899,723,639
# #                        --------------
# 
# 
# 
# # Credit Loss
# 
# #1 
# wrong_pred  <- score_check[score_check$Performance.Tag == 0 & score_check$pred == 1,]
# L1 <- sum(wrong_pred$Outstanding.Balance, na.rm =T)
# L1
# 
# # Loss = 7,536,346,714
# 
# 
# # 2 Loss as a result of Model not picking the defaulters
# 
# wrong_pred_notpicked  <- score_check[score_check$Performance.Tag == 1 & score_check$pred == 0,]
# L2 <- sum(wrong_pred_notpicked $Outstanding.Balance, na.rm =T)
# L2
# 
# # Loss  = 1,122,899,336
# 
# 
# Total_L <- L1+L2
# Total_L
# 
