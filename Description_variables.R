setwd("C:/Users/Pol/Desktop/MVA_Project")
require("dplyr")        # data2 manipulation
require("reshape2")     # data2 reshaping for ggplot
require("ggplot2")      # data2 visualization
require("RColorBrewer") # Colors on plots
require("readr")        # CSV file I/O, e.g. the read_csv function
require("data2QualityR") # DQR generation
require("randomForest") # Random Forest for variable importance
require("scales")       # Colour palette
require("fmsb")         # Radar plots
require("beeswarm")     # To represent heavy tailored distributed variables (eg:balance)

## Librarys at your own way

data2 <- read.csv("phpkIxskf.csv", stringsAsFactors = F)
attach(data2)
summary(data2)
head(data2)
colnames(data2) <- c("age","job","marital", "education", "credit_in_default", 
                   "balance", "housing_loan", "personal_loan",
                   "contact_type", "last_contact_day",
                   "last_contact_month", "last_contact_duration",
                   "number_contacts_campaign", "last_day_contact_previous_campaign",
                   "number_contacts_before_campaign",
                   "outcome_previous_campaign","subscription")

head(data2)

## Top Studies distribution

studies <- tolower(data2$education)
table(studies)
studiesN <- as.data2.frame(table(studies))
studies.t <- studiesN[order(studiesN$Freq),]

studies.final <- data2.frame(
    name=c("Unknown","Primary","Tertiary","Secondary"),
    val=studies.t$Freq
)
meanStudies <- mean(studies.final$val)

studies.final %>%
    ggplot( aes(x=name, y=val)) +
    geom_bar(stat="identity",fill="#f9a65a") +
    coord_flip()+xlab("") + ylab("") +
    geom_hline(yintercept = meanStudies, color="blue",size=1) +
    geom_text(aes(label=studies.final$val), position=position_dodge(width=0.9), vjust=-0.25)

## Top Employments distribution
jobs <- tolower(data2$job)
table(jobs)
jobsN <- as.data2.frame(table(jobs))
jobs.t <- jobsN[order(jobsN$Freq),]

jobs.final <- data2.frame(
    name=c("Unknown","Student","Housemaid","Unemployed","Enterpreneur","Self-employed",
           "Retired","Services","Administration","Technician","Management","Blue-Collar"),
    val=jobs.t$Freq
)
meanjobs <- mean(jobs.final$val)

jobs.final %>%
    ggplot( aes(x=name, y=val)) +
    geom_bar(stat="identity",fill="#f9a65a") +
    coord_flip()+xlab("") + ylab("") +
    geom_hline(yintercept = meanjobs, color="blue",size=1) +
    geom_text(aes(label=jobs.final$val), position=position_dodge(width=0.9), vjust=-0.25)
## Top Employments distribution

## Distribution of ages by marital

ageDistribution<- data2[!is.na(data2$age),] %>%
    group_by(marital,age) %>% summarise(count = n())

r <- ggplot(ageDistribution, aes(x = age, y = count, fill = factor(marital))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_discrete(name = "") +
    xlab("Age") + ylab("Frequency")+ geom_line(stat = "identity", position = "dodge")+annotate(geom="text", x=75, y=800, label="Mean age = 41", color="black")
r
mean(data2$age)


## Balance variable

balance <- as.data2.frame((data2$balance))
tomean <- as.numeric(data2$balance)
mean(tomean,na.rm=TRUE)

beeswarm(balance)

t <- ggplot(data2 = balance, aes(x = "", y = balance)) + 
    geom_boxplot(fill="#f9a65a")+
    xlab("") + ylab("Balance (???)")
t

t + geom_jitter(position=position_jitter(0.2),alpha=0.18)+annotate(geom="text", x=1.3, y=75000, label="Mean balance = 1362,3 ???", color="black")

## Number of positive subscriptions

ggplot(data, aes(subscription)) + geom_bar(aes(position = "dodge",
                                               fill=subscription))





