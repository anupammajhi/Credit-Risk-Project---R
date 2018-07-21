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
