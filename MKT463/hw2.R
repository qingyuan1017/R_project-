# Load the csv files
df_consumer <- read.csv(file="consumerDataFrame.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
df_item <- read.csv(file="ItemDataFrame.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# 1 b) Change the name of each column in each dataset to something readable
df_consumer$X <- NULL
df_item$X <- NULL

names(df_consumer)
names(df_consumer)[1]<-paste("Units")
names(df_consumer)[2]<-paste("Dollars")
names(df_consumer)[3]<-paste("Week")
names(df_consumer)[4]<-paste("StoreID")
names(df_consumer)[5]<-paste("CustomerID")
names(df_consumer)[6]<-paste("FlavorID")

names(df_item)
names(df_item)[1]<-paste("VolumePerUnit")
names(df_item)[2]<-paste("Ischicken")
names(df_item)[3]<-paste("PrivateLabel")
names(df_item)[4]<-paste("Flavor")
names(df_item)[5]<-paste("Istomato")
names(df_item)[6]<-paste("FlavorID")


# 1 c) Merge the datasets into one single large data frame.
# Create a empty df
df_loop <- NULL
# Run loop and iterate the whole item dataframe
for(i in 1: nrow(df_item)){
# get the subset in consumer dataframe which has the same flavor ID as item 
    df_loop_sub <- df_consumer[df_consumer$FlavorID == df_item[i,]$FlavorID,]
# if the subset dataframe is not empty
    if(nrow(df_loop_sub) != 0){
# column bind the consumer table with item table
      df_loop_part <- cbind(df_loop_sub,df_item[i,])
# row bind with previous result
      df_loop <- rbind(df_loop,df_loop_part) 
    }
}
# delete the repeat column
df_loop$FlavorID <- NULL

df_merge <- merge(df_consumer,df_item, by.x = "FlavorID", by.y = "FlavorID")

df <- df_merge

# 1 d) Create two new variables, one that contains the price per unit purchased, and another that
# contains the total volume of soup purchased. Use variable names ‘pricePerUnit’ and
#‘totalVolume’.
df$pricePerUnit <- df$Dollars/df$Units

df$totalVolume <- df$Units * df$VolumePerUnit

# Present some relevant descriptive statistics (i.e. mean, standard deviation) of the data.
# Include the market shares for each store, the market shares for each of the top ten flavors,
# and the market shares for the private label. Calculate market shares in terms of units sold.
# Answer in paragraph form in the pdf. (10 marks)

# 1 e) Relevant descriptive statistics
summary(df)
length(unique(df$FlavorID))
length(unique(df$CustomerID))
sapply(df, mean, na.rm=TRUE)
sapply(df, sd, na.rm=TRUE)


# 1 e) The market shares for each store
df_store <- aggregate(Units ~ StoreID, data = df, sum)
pct <- round(df_store$Units/sum(df_store$Units)*100,2)
df_store$marketshare <- paste(pct,"%",sep="")
df_store

# 1 e)The market shares for each of the top ten flavors
df_flavor <- aggregate(Units ~ FlavorID, data = df, sum)
pct <- round(df_flavor$Units/sum(df_flavor$Units)*100,2)
df_flavor$marketshare <- pct
df_flavor <- df_flavor[order(-df_flavor$marketshare),]
df_flavor$marketshare <- paste(df_flavor$marketshare, "%",sep="")
df_flavor <- df_flavor[1:10,]
df_top_flavor <- df[is.element(df$FlavorID, df_flavor$FlavorID),]
unique(df_top_flavor$Flavor)

# 1 e)The market shares for private label
df_label <- aggregate(Units ~ PrivateLabel, data = df, sum)
pct <- round(df_label$Units/sum(df_label$Units)*100,2)
df_label$marketshare <- pct
df_label

# 2 a)  The average purchase price for each consumer
df_each_consumer <- aggregate(cbind(Dollars,Units) ~ CustomerID, data = df, sum)
df_each_consumer$metric1 <- df_each_consumer$Dollars/df_each_consumer$Units

# 2 b) regression for each consumer where the dependent variable is the
# number of units purchased, and the independent variable is the price
length(unique(df$CustomerID))
df_each_consumer$coefficent <- NA
for(i in 1 : nrow(df_each_consumer)){
  df_regression <- df[df$CustomerID == df_each_consumer[i,]$CustomerID,]
  regression <- lm(Units ~ pricePerUnit, data = df_regression)
  df_each_consumer[i,]$coefficent <- coef(regression)[2]
}


# 2 c) Produce a single well formatted scatterplot with the metric from regression coeffcient on the y axis and
# the average purchase price by consumer on the x axis
library(ggplot2)
ggplot(df_each_consumer, aes(x= metric1, y=coefficent)) +
  geom_point(shape=1)+geom_smooth()+labs(title = "The correlation between the two metrics") +
  ylab("Regression coefficent") +
  xlab("Average Purchase Price")
cor(df_each_consumer$metric1, df_each_consumer$coefficent, use="complete")

# After review the result in scatterplot, some of the coeffcient outlier are to significant that can't be explain.
# I removed some outlier and did the plot again
df_each_consumer_new <- na.omit(df_each_consumer)
df_each_consumer_new <- subset(df_each_consumer_new,!(df_each_consumer_new$coefficent > quantile(df_each_consumer_new$coefficent, probs=c(.05, .95))[2] 
                           | df_each_consumer_new$coefficent < quantile(df_each_consumer_new$coefficent, probs=c(.05, .95))[1]))

#df_each_consumer_new <- subset(df_each_consumer_new,!(df_each_consumer_new$metric1 > quantile(df_each_consumer_new$metric1, probs=c(.01, .99))[2] 
 #                                                     | df_each_consumer_new$metric1 < quantile(df_each_consumer_new$metric1, probs=c(.01, .91))[1]))

ggplot(df_each_consumer_new, aes(x= metric1, y=coefficent)) +
  geom_point(shape=1)+geom_smooth()+labs(title = "The correlation between the two metrics (without outlier)") +
  ylab("Regression coefficent") +
  xlab("Average Purchase Price")
cor(df_each_consumer_new$metric1, df_each_consumer_new$coefficent, use="complete")

# 2 e) Use your metric of choice to identify the top 100 most price sensitive consumers.
df_each_consumer <- df_each_consumer[order(df_each_consumer$coefficent),]
df_top100 <- df_each_consumer[1:100,c("CustomerID", "coefficent")]
print(df_top100)

# top 100 consumer without outlier
df_each_consumer_new <- df_each_consumer_new[order(df_each_consumer_new$coefficent),]
df_top100_new <- df_each_consumer_new[1:100,c("CustomerID", "coefficent")]
print(df_top100_new)

# 3 a) Aggregate the sales data to the week level
df_weekly <- aggregate(Dollars ~ Week, data = df, sum)

# 3 b) Find the ARMA model that best fits this time series data using the AIC as the metric.
modelStat = data.frame(ar = rep(NA,25),ma = rep(NA,25),AIC = rep(NA,25))
rowNum = 1
for(arDegree in 0:3){
  for(maDegree in 0:3){
    currentFit = arima(df_weekly$Dollars,c(arDegree, 0, maDegree))
    modelStat[rowNum,] = c(arDegree,maDegree,AIC(currentFit))
    rowNum = rowNum + 1
  }
}

# Run ARMA model with best AR and MA degree
best_index <- modelStat[which.min(modelStat$AIC),]
optional_model = arima(df_weekly$Dollars,c(best_index$ar, 0, best_index$ma))
optional_model
summary(optional_model)

# 4 a)
# Find the top 100 price sensitive customer
top100 <- df_top100$CustomerID
top100
# Select all data about these 100 price sensitive consumer
df_100 <- df[is.element(df$CustomerID, top100),]
df_100_new <- df[is.element(df$CustomerID, df_top100_new$CustomerID),]
# Function to find mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

summary(df)
summary(df_100)
summary(df_100_new)
# What are the most popular flavor over price sensitive consumer
getmode(df_100$Flavor)
getmode(df_100_new$Flavor)
getmode(df$Flavor)



# 4 b) Soup sales over time
library(forecast)
fit <- Arima(df_weekly$Dollars,order=c(best_index$ar, 0, best_index$ma))
plot(fit$x,col="red", xlab = 'Weeks', ylab = 'Sales',main = 'Prediction of Soup Sales over Time')
lines(fitted(fit),col="blue")  
