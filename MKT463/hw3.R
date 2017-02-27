
# Load the csv files
df <- read.csv(file="recommendDB.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
summary(df)

# Define a data frame that has a row for each consumer, and 6 columns. 
columns <- c("consumerID","Rocky.1","Rocky.2","Rocky.3","Rocky.4","Rocky.5")
ratings <- matrix(nrow = length(unique(df$consumerID)),ncol = 6)
ratings <- data.frame(ratings)
colnames(ratings) <- columns

# Fill the data frame from part b using the rating data.
# fill in the consumerID
ratings$consumerID <- unique(df$consumerID)
# iterate through rating matrix
for (i in 1:nrow(ratings)){
# subseting the df to find about rating for a specific user
  rating <- df[df$consumerID == ratings$consumerID[i],] 
  for(j in 1:nrow(rating)){
#for each user find rating for the series.
    if(rating$rockyID[j] == 1){
      # Rocky1      
      ratings[i,2] <- rating$rating[j]
    }
    if(rating$rockyID[j] == 2){
      # Rocky2            
      ratings[i,3] <- rating$rating[j]
    }
    if(rating$rockyID[j] == 3){
      # Rocky3      
      ratings[i,4] <- rating$rating[j]
    }
    if(rating$rockyID[j] == 4){
      # Rocky4      
      ratings[i,5] <- rating$rating[j]
    }
    if(rating$rockyID[j] == 5){
      # Rocky5      
      ratings[i,6] <- rating$rating[j]
    }
  }
}

# PART 2
# Correlations between the ratings of each of the five movies
cov(ratings[c("Rocky.1","Rocky.2","Rocky.3","Rocky.4","Rocky.5")], use="pairwise.complete.obs")

# How to measure the similarity between two movie is really objective. In this case, we use the rating as the metric
# Normally, an user rate highly on a movie would also rate higher on the similar one. 
# According to the correlation matrix, Rocky4 and Rocky5 is most similar since they have the highest correlation.
# While Rocky1 and Rocky3 is least similar because they have the lowest correlation.


# Find the mean rating of each movie. 
colMeans(ratings[c("Rocky.1","Rocky.2","Rocky.3","Rocky.4","Rocky.5")], na.rm = TRUE,dims = 1)

# Create a subset of your data frame that only contains consumers who rated Rocky 4. 
ratings4 <- ratings[is.na(ratings$Rocky.4) == FALSE,]
colMeans(ratings4[c("Rocky.1","Rocky.2","Rocky.3","Rocky.4","Rocky.5")], na.rm = TRUE,dims = 1)


# PART 3
# install and import nnet package (neural network)
install.packages('nnet')
library('nnet')
# install and import rpart package (decision tree)
install.packages("rpart")
library('rpart')
# Analyze consumers with complete data
ratings.clean <- na.omit(ratings)
# Estimate and store a simple linear regression
summary(lm(Rocky.5 ~ Rocky.1*Rocky.2*Rocky.3*Rocky.4, data = ratings.clean))

# fomula generator
rocky1Vec = c('','+Rocky.1','+poly(Rocky.1,2)','+poly(Rocky.1,3)','+log(Rocky.1)')
rocky2Vec = c('','+Rocky.2','+poly(Rocky.2,2)','+poly(Rocky.2,3)','+log(Rocky.2)')
rocky3Vec = c('','+Rocky.3','+poly(Rocky.3,2)','+poly(Rocky.3,3)','+log(Rocky.3)')
rocky4Vec = c('','+Rocky.4','+poly(Rocky.4,2)','+poly(Rocky.4,3)','+log(Rocky.4)')
inVec1 = c('','+Rocky.1*Rocky.2*Rocky.3*Rocky.4')
formulaSet = paste('Rocky.5~1',apply(expand.grid(rocky1Vec,rocky2Vec,rocky3Vec,rocky4Vec,inVec1),1,paste,collapse=''))


# MSE calculate function
MSE = function(x){
  return(mean(x^2))
}


# load train and test dataset
df_train <- read.csv(file="rockyDB - trainingData.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
df_test <- read.csv(file="rockyDB - validationData.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# df for storing lm model result
performance.lm <- data.frame(formulaSet)
performance.lm$MSE <- NA

# df for storing NN model result
performance.NN <- performance.lm
size <- c(1,2,3)
size<- data.frame(size)
performance.NN <- merge(performance.NN,size)

# df for storing rpart model result
treeSet = paste('Rocky.5~ 1',
                apply(expand.grid(rocky1Vec,rocky2Vec,rocky3Vec,rocky4Vec),1,paste,collapse=''))
performance.rpart <- data.frame(treeSet)
performance.rpart$MSE <- NA

set.seed(90)

# Try different LM models 
for(i in 1:length(formulaSet)){
  model <- lm(as.formula(as.character(performance.lm[i,'formulaSet'])),data=df_train)
  performance.lm[i,'MSE'] <- MSE(df_test$Rocky.5 - predict(model,df_test))
}

# Try different NN models
for(i in 1:length(formulaSet)){
  for(j in 1:3){
    set.seed(90)
    model <- nnet(as.formula(as.character(performance.NN[i,'formulaSet'])),data=df_train,size = j, linout = 1,maxit = 1000)
    performance.NN[performance.NN$formulaSet == formulaSet[i] & performance.NN$size == j,"MSE"] <- MSE(df_test$Rocky.5 - predict(model,df_test))
  }
}

# Try different rpart models
for(i in 2:length(treeSet)){
  model <- rpart(as.formula(as.character(performance.rpart[i,'treeSet'])),data=df_train)
  performance.rpart[i,'MSE'] <- MSE(df_test$Rocky.5 - predict(model,df_test))
}

# find the best model in each case
best_index_lm <- performance.lm[which.min(performance.lm$MSE),]
best_index_NN <- performance.NN[which.min(performance.NN$MSE),]
best_index_rpart <- performance.rpart[which.min(performance.rpart$MSE),]

# Compare the best result between each estimate procedure
best_index_lm$MSE # MSE = 0.8992749
best_index_NN$MSE # MSE = 0.882356
best_index_rpart$MSE # MSE = 0.928041

# Store optional model
set.seed(90)
chosenModel <- nnet(as.formula(as.character(best_index_NN[1,'formulaSet'])),data=df_train,size = 3, linout = 1,maxit = 1000)
MSE(df_test$Rocky.5 - predict(chosenModel,df_test))
save(chosenModel, file = 'chosenModel.Rdata')

# Estimate and store the best model using the full dataset
randomOrder = order(runif(nrow(ratings.clean)))
trainData = subset(ratings.clean, randomOrder <= .6*nrow(ratings.clean))
validationData = subset(ratings.clean, randomOrder>.6*nrow(ratings.clean) & randomOrder < .8*nrow(ratings.clean))
testData = subset(ratings.clean, randomOrder >= .8*nrow(ratings.clean))

for(i in 1:length(formulaSet)){
  model <- lm(as.formula(as.character(performance.lm[i,'formulaSet'])),data=trainData)
  performance.lm[i,'MSE'] <- MSE(validationData$Rocky.5 - predict(model,validationData))
}

# Try different NN models
for(i in 1:length(formulaSet)){
  for(j in 1:3){
    set.seed(90)
    model <- nnet(as.formula(as.character(performance.NN[i,'formulaSet'])),data=trainData,size = j, linout = 1,maxit = 1000)
    performance.NN[performance.NN$formulaSet == formulaSet[i] & performance.NN$size == j,"MSE"] <- MSE(validationData$Rocky.5 - predict(model,validationData))
  }
}

# Try different rpart models
for(i in 2:length(treeSet)){
  model <- rpart(as.formula(as.character(performance.rpart[i,'treeSet'])),data=trainData)
  performance.rpart[i,'MSE'] <- MSE(validationData$Rocky.5 - predict(model,validationData))
}

# find the best model in each case
best_index_lm <- performance.lm[which.min(performance.lm$MSE),]
best_index_NN <- performance.NN[which.min(performance.NN$MSE),]
best_index_rpart <- performance.rpart[which.min(performance.rpart$MSE),]

# Compare the best result between each estimate procedure
best_index_lm$MSE 
best_index_NN$MSE
best_index_rpart$MSE 

# Store optional model
set.seed(90)
chosenModel_full <- nnet(as.formula(as.character(best_index_NN[1,'formulaSet'])),data=df_train,size = 3, linout = 1,maxit = 1000)
MSE(df_test$Rocky.5 - predict(chosenModel_full,df_test))
save(chosenModel_full, file = 'chosenModel_full.Rdata')
