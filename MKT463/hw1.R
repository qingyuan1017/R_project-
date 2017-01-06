# All code is the personal work of Qingyuan Zhang

# Read the Data
Data <- read.csv(file="Assignment 1 Data.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

# Create a new column called 'rDate' convert the 'date' column into the 'date' datatype. 
Date <-paste(Data$date)
rDate <- as.Date(Date,'%m/%d/%Y')
Data$rDate <- rDate

# Determine the date that started the treatment period    
Data <- Data[order(rDate),] 
i = 1
while(Data[i,"isTreatmentPeriod"] == 0){
  i = i+1
}
Data[i,"rDate"]
