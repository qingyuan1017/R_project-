# All code is the personal work of Qingyuan Zhang

# a) Read the Data
df <- read.csv(file="Assignment 1 Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# b) Create a new column called 'rDate' convert the 'date' column into the 'date' datatype. 
Date <-paste(df$date)
rDate <- as.Date(Date,'%m/%d/%Y')
df$rDate <- rDate

# c) Determine the date that started the treatment period    
Start_Date= min(df$rDate[which(df$isTreatmentPeriod==1)])

# d) Take a subset of all the data from the treatment group
df_treatmentgroup <- subset(df,isTreatmentGroup == 1)
# Run a regression that compares log(revenue) of treatment group and in pre-treatment period and in the treatment period
summary(lm(log(revenue) ~ isTreatmentPeriod, df_treatmentgroup))

# e) Take a subset of all the data from the pre-treatment period
df_pre_treatment <- subset(df,isTreatmentPeriod == 0)
# Run a regression that compares log(revenue) of in pre-treatment period for control and treatment groups
summary(lm(log(revenue) ~ isTreatmentGroup, data = df_pre_treatment))

# f) Take subset of all the data from post treatment period
df_post_treatment <- subset(df,isTreatmentPeriod == 1)
# Run a regression determine the effectiveness of eBay ads.
summary(lm(log(revenue) ~ isTreatmentGroup, data = df_post_treatment))

# g) Create a new variable called month
install.packages("lubridate")
library(lubridate)
df$month <- month(df$rDate)
df$month <-as.factor(df$month)

df_post_treatment <- subset(df,isTreatmentPeriod == 1)
# regression on the post treatment data, interact whether the DMA is in the treatment group with this month variable.
summary(lm(log(revenue) ~ isTreatmentGroup+month+isTreatmentGroup*month, data = df_post_treatment))


