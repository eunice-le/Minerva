###Multilateral Development Institution Data
foo <- read.csv ("https://tinyurl.com/yb4phxx8") # read the data
#Column names 
names(foo)
#Dimensions of the data set 
dim(foo)
#Quick look at the data structure 
head(foo)

#Take note of columns representing calendar dates 
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)
#There are missing values in the date columns. 
#The code below call the blank (empty) elements NA instead of leaving them blank.
#The last line also tells R those elements are Date objects. 
for(i in date.columns)
{
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  foo[which_values_are_missing, i] <- NA
  foo[, i] <- as.Date(as.character(foo[, i]))
}

#Testing our the new Date objects 
foo[3, 12]
foo[4, 12]
foo[3, 12]-foo[4,12]

#Eliminate rows with NA dates 
which.have.NAs <- which(is.na(foo$Rating == TRUE))
new_foo <- foo[-which.have.NAs, ]

#Choosing only objects with non-missing CirculationDate. 
noNA_foo <- new_foo[!is.na(new_foo$CirculationDate), ]
#New dataframe without non-missing Circulation.Date >= 2009-01-01
df <- noNA_foo[which(noNA_foo$CirculationDate >= as.Date("2009-01-01")), ]

#Checking the new 'df' data frame 
summary(df$CirculationDate)

###QUESTION 1
###Question 1 (a)
#Disregard some projects with no completion dates
nocd <- which(is.na(df$OriginalCompletionDate))
df_withDates <- df[-nocd, ]
expected_duration <- mean(df_withDates$OriginalCompletionDate)-mean(df_withDates$ApprovalDate)
expected_duration
#The expected duration in months - assuming each month lasts 30 days 
expected_duration/30

###Question 1 (b)
#Create a new column with Circulation year (extracted from circulation date) in the df_withDates data set
df_withDates$CirculationYear <- format(df_withDates$CirculationDate, "%Y")
#Checking for rows with NA dates in the RevisedCirculationDates column
sum(is.na(df_withDates$RevisedCompletionDate)) #There is none

#Add a delay column to the df_withDates data frame
df_withDates$Delay <- df_withDates$OriginalCompletionDate - df_withDates$RevisedCompletionDate
#Checking for rows with NA delay 
sum(is.na(df_withDates$Delay)) #There is none
#Calculating the mean, median, and quintile of Delay time for each year in the CirculationYear
library(dplyr)  
delayByYear <- df_withDates %>%  # %>% means "pass what is to the left to the function that follows"
  group_by(CirculationYear) %>%    
  summarise(mean.delay = mean(Delay), 
            median.delay = median(Delay),
            IQR.delay = quantile(Delay, 0.75) - quantile(Delay, 0.25))
delayByYear

###Question 1 (c)
#Adding an Actual duration column to the df_withDates data frame 
df_withDates$ActualDuration <- df_withDates$RevisedCompletionDate - df_withDates$ApprovalDate
#Statistics of the Actual Duration 
mean(df_withDates$ActualDuration) 
median(df_withDates$ActualDuration)
quantile(df_withDates$ActualDuration)
IQR(df_withDates$ActualDuration)

#Adding an Expected duration column to the df_withDates data frame 
df_withDates$ExpectedDuration <- df_withDates$OriginalCompletionDate - df_withDates$ApprovalDate
#Statistics of Expected Duration 
mean(df_withDates$ExpectedDuration)
median(df_withDates$ExpectedDuration) 
quantile(df_withDates$ExpectedDuration) 
IQR(df_withDates$ExpectedDuration)

###QUESTION 2 
#New data frame with only projects with non-missing RevisedCompletionDate 
foo2010 <- noNA_foo[which(noNA_foo$RevisedCompletionDate>= as.Date("2010-01-01")), ]
#Calculating the percentage of projects with each rating from 0 to 3
percent_rating_0 <- sum(foo2010$Rating == 0)/sum(foo2010$Rating != "NA")*100
percent_rating_0
percent_rating_1 <- sum(foo2010$Rating == 1)/sum(foo2010$Rating != "NA")*100
percent_rating_1
percent_rating_2 <- sum(foo2010$Rating == 2)/sum(foo2010$Rating != "NA")*100
percent_rating_2
percent_rating_3 <- sum(foo2010$Rating == 3)/sum(foo2010$Rating != "NA")*100
percent_rating_3

###QUESTION 3
#New data frame with only "PATA" projects from 2010 
foo2010_PATA <- foo2010[which(foo2010$Type == "PATA"), ]
##Calculating the percentage of projects with each rating from 0 to 3
pata_percent_rating_0 <- sum(foo2010_PATA$Rating == 0)/sum(foo2010_PATA$Rating != "NA")*100
pata_percent_rating_0
pata_percent_rating_1 <- sum(foo2010_PATA$Rating == 1)/sum(foo2010_PATA$Rating != "NA")*100
pata_percent_rating_1
pata_percent_rating_2 <- sum(foo2010_PATA$Rating == 2)/sum(foo2010_PATA$Rating != "NA")*100
pata_percent_rating_2
pata_percent_rating_3 <- sum(foo2010_PATA$Rating == 3)/sum(foo2010_PATA$Rating != "NA")*100
pata_percent_rating_3

###QUESTION 4
#Defining the top 10
df_Top10 <- df[which(df$RevisedAmount >= quantile(df$RevisedAmount, 0.9)),]
df_Bottom10 <- df[which(df$RevisedAmount <= quantile(df$RevisedAmount, 0.1)),]

#Comparing the ratings of the top 10% and bottom 10% 
(table(df_Top10$Rating) / length(df_Top10$Rating) * 100) - 
  (table(df_Bottom10$Rating) / length(df_Bottom10$Rating) * 100)

#Comparing other groupings 
#Dept 
(table(df_Top10$Dept) / length(df_Top10$Dept) * 100) - 
  (table(df_Bottom10$Dept) / length(df_Bottom10$Dept) * 100)
#Country
(table(df_Top10$Country) / length(df_Top10$Country) * 100) - 
  (table(df_Bottom10$Country) / length(df_Bottom10$Country) * 100)
#Division
(table(df_Top10$Division) / length(df_Top10$Division) * 100) - 
  (table(df_Bottom10$Division) / length(df_Bottom10$Division) * 100)
