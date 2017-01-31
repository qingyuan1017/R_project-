# Load the csv files
df_consumer <- read.csv(file="consumerDataFrame.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
df_item <- read.csv(file="ItemDataFrame.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# Change the name of each column in each dataset to something readable
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


# Merge the datasets into one single large data frame.
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

# Create two new variables, one that contains the price per unit purchased, and another that
# contains the total volume of soup purchased. Use variable names ‘pricePerUnit’ and
#‘totalVolume’.

#df_each_flavor <- aggregate(cbind(Dollars, Units) ~ FlavorID, data = df_merge, sum)
#df_each_flavor$price_per_unit <- df_each_flavor$Dollars/df_each_flavor$Units
#pricePerUnit <- df_each_flavor[,c("FlavorID","price_per_unit")]

df$pricePerUnit <- df_merge$Dollars/df_merge$Units


#df_each_customer <- aggregate(cbind(VolumePerUnit, Units, Dollars) ~ CustomerID, data = df_merge, sum)
#df_each_customer$totalVolume <- df_each_customer$Units*df_each_customer$VolumePerUnit 
#totalVolume <- df_each_customer[,c("CustomerID","totalVolume")]

df$totalVolume <- df$Units * df$VolumePerUnit

# Present some relevant descriptive statistics (i.e. mean, standard deviation) of the data.
# Include the market shares for each store, the market shares for each of the top ten flavors,
# and the market shares for the private label. Calculate market shares in terms of units sold.
# Answer in paragraph form in the pdf. (10 marks)

# Relevant descriptive statistics
summary(df)
sapply(df, mean, na.rm=TRUE)
sapply(df, sd, na.rm=TRUE)
sapply(df, var, na.rm=TRUE)
sapply(df, min, na.rm=TRUE)
sapply(df, max, na.rm=TRUE)
sapply(df, median, na.rm=TRUE)
sapply(df, range, na.rm=TRUE)


# The market shares for each store
df_store <- aggregate(Units ~ StoreID, data = df, sum)
pct <- round(df_store$Units/sum(df_store$Units)*100,2)
df_store$marketshare <- paste(pct,"%",sep="")
df_store

# The market shares for each of the top ten flavors
df_flavor <- aggregate(Units ~ FlavorID, data = df, sum)
pct <- round(df_flavor$Units/sum(df_flavor$Units)*100,2)
df_flavor$marketshare <- pct
df_flavor <- df_flavor[order(-df_flavor$marketshare),]
df_flavor$marketshare <- paste(df_flavor$marketshare, "%",sep="")
head(df_flavor,10)


# 2 a)  The average purchase price for each consumer
df_each_consumer <- aggregate(pricePerUnit ~ CustomerID, data = df, mean)

# 2 b) regression for each consumer where the dependent variable is the
# number of units purchased, and the independent variable is the price
length(unique(df$CustomerID))
df_each_consumer$coefficent <- NA
for(i in 1 : nrow(df_each_consumer)){
  df_regression <- df[df$CustomerID == df_each_consumer[i,]$CustomerID,]
  regression <- lm(Units ~ pricePerUnit, data = df_regression)
  df_each_consumer[i,]$coefficent <- coef(regression)[2]
}


# 2 c)
library(ggplot2)
ggplot(df_each_consumer, aes(x= pricePerUnit, y=coefficent)) +
  geom_point(shape=1)+geom_smooth()+labs(title = "The correlation between the two metrics") +
  ylab("Regression coefficent") +
  xlab("Average Purchase Price")
cor(df_each_consumer$Dollars, df_each_consumer$coefficent, use="complete")

# 2 e) 
df_each_consumer <- df_each_consumer[order(df_each_consumer$coefficent),]
df_top100 <- df_each_consumer[1:100,c("CustomerID", "coefficent")]
print(df_top100)


# 3 a) Aggregate the sales data to the week level
df_weekly <- aggregate(Dollars ~ Week, data = df, sum)
ts.plot(gtemp, pred$pred, lty = c(1,3), col=c(5,2))

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

best_index <- modelStat[which.min(modelStat$AIC),]
optional_model = arima(df_weekly$Dollars,c(best_index$ar, 0, best_index$ma))
optional_model

# 4
library(forecast)
fit <- Arima(df_weekly$Dollars,order=c(best_index$ar, 0, best_index$ma))
plot(fit$x,col="red")
lines(fitted(fit),col="blue")  
