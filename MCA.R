##Load workspace and some libraries

setwd("C:/Users/Pol/Desktop/MVA_Project")
require("dplyr")        # Data manipulation
require("reshape2")     # Data reshaping for ggplot
require("ggplot2")      # Data visualization
require("RColorBrewer") # Colors on plots
require("readr")        # CSV file I/O, e.g. the read_csv function
require("dataQualityR") # DQR generation
require("randomForest") # Random Forest for variable importance
require("scales")       # Colour palette
require("fmsb")         # Radar plots
require("caret")        # For Machine Learning, TOP
require("randomForest") # Random Forest
require("RANN")
require("VIM")
require("chemometrics")
require("scales")       # Colour palette
require("fmsb")         # Radar plots
require("corrplot")     # To correlation plot of continuous variables
require("FactoMineR")   # To PCAs

## Read data and apply MCA

rm(list=ls())

data <- read.csv("phpkIxskf.csv", stringsAsFactors = F)

head(data)
colnames(data) <- c("age","job","marital", "education", "credit_in_default", 
                    "balance", "housing_loan", "personal_loan",
                    "contact_type", "last_contact_day",
                    "last_contact_month", "last_contact_duration",
                    "number_contacts_campaign", "last_day_contact_previous_campaign",
                    "number_contacts_before_campaign",
                    "outcome_previous_campaign","subscription")

head(data)
summary(data)

age <- which(colnames(data)=="age")
balance <- which(colnames(data)=="balance")
last_contact_day <- which(colnames(data)=="last_contact_day")
last_contact_duration <- which(colnames(data)=="last_contact_duration")
number_contacts_campaign <- which(colnames(data)=="number_contacts_campaign")
last_day_contact_previous_campaign <- which(colnames(data)=="last_day_contact_previous_campaign")
number_contacts_before_campaign  <- which(colnames(data)=="number_contacts_before_campaign")

subscription <- which(colnames(data)=="subscription")

data$age <- as.numeric(data$age)
data$balance <- as.numeric(data$balance)
data$last_contact_day <- as.numeric(data$last_contact_day)
data$last_contact_duration <- as.numeric(data$last_contact_duration)
data$number_contacts_campaign <- as.numeric(data$number_contacts_campaign)
data$last_day_contact_previous_campaign <- as.numeric(data$last_day_contact_previous_campaign)
data$number_contacts_before_campaign <- as.numeric(data$number_contacts_before_campaign)

data$subscription <- data$subscription - 1

data$subscription <- as.factor(data$subscription)
data$education <- as.factor(data$education)
data$housing_loan <- as.factor(data$housing_loan)
data$outcome_previous_campaign <- as.factor(data$outcome_previous_campaign)
data$job <- as.factor(data$job)
data$credit_in_default <- as.factor(data$credit_in_default)
data$personal_loan <- as.factor(data$personal_loan)
data$last_contact_month <- as.factor(data$last_contact_month)
data$marital <- as.factor(data$marital)
data$contact_type <- as.factor(data$contact_type)

sapply(data,class)

mca <- MCA(data, 
           quanti.sup = c(age,balance,last_contact_day,
                          last_contact_duration,number_contacts_campaign,
                          last_day_contact_previous_campaign,
                          number_contacts_before_campaign), 
           quali.sup = c(subscription))

# Interpret the first factorial components

dimdesc(mca)

## Decide the number of significant dimensions that we retain 
## (by subtracting the average eigenvalue and represent the 
## new obtained eigenvalues in a new screeplot).

plot(mca[["eig"]][,1],type="l", xlab = "Index", ylab = "Eigenvalue")

## Find average eigenvalue

avg <- mean(mca[["eig"]][,1])
abline(h = avg, col="red", lty = 2)

## Take all those dimensions > than average

lmb <- mca[["eig"]][,1][mca[["eig"]][,1]>avg] 

## New screeplot

plot(lmb,type="l",xlab = "Index",ylab = "Eigenvalue")

plot(cumsum(100*lmb/sum(lmb)), type = "o", xlab="Component Number", ylab="Contribution to total variance(%)", pch=16, ylim = c(0,100))
abline(h=80, col="red", lty = 2)

## We take dimensions that explains 80% of new dimension set

nd <- which.min(abs(cumsum(100*lmb/sum(lmb)) - 80))
L <- 1:nd

mca <- MCA(data, 
           quanti.sup = c(age,balance,last_contact_day,
                          last_contact_duration,number_contacts_campaign,
                          last_day_contact_previous_campaign,
                          number_contacts_before_campaign), 
           quali.sup = c(subscription),ncp = 11)

rm(list=setdiff(ls(), c("data","mca")))








