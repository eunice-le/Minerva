#load libraries
install.packages("pastecs")
library(pastecs)

#load data set
foo <- read.csv("D:\\Hanh Le\\county_statistics.csv")

#count the number of missing value 
count.na <- data.frame(colSums(is.na(foo)))
percentage.na <- c()
for (i in (1:nrow(count.na))) {
  percentage.na[i] <- round(count.na$colSums.is.na.foo..[i]/nrow(foo)*100, digits = 0) 
}
count.na$percentage.na <- percentage.na

#eliminate rows with missing data
data <- na.omit(foo, c(4:8))

#determine the 2020 winner by vote percentage
#1 if Trump wins, 0 if Biden wins
winner <- c()
for (i in (1:nrow(data))) {
  if (data$percentage20_Donald_Trump[i]>data$percentage20_Joe_Biden[i]) {
    winner[i] <- 1
  } else {
    winner[i] <- 0
  }
}
data$winner <- winner 

#calculating the rate of COVID contraction in each county 
covid_rate <- c()
for (i in (1:nrow(data))) {
  covid_rate[i] <- round(data$cases[i]/data$TotalPop[i]*100, digits = 1)
}
data$covid_rate <- covid_rate

#calculating the rate of death from COVID in each county 
covid_death <- c()
for (i in (1:nrow(data))) {
  covid_death[i] <- round(data$deaths[i]/data$cases[i]*100, digits = 1)
}
data$covid_death <- covid_death

#subset counties where Trump/ Biden won
trump.county <- subset(data, winner == 1)
biden.county <- subset(data, winner == 0)

#descriptive stats for COVID rate in each county
#in Trump counties 
trump.desvar <- cbind(covid_rate, covid_death, Unemployment, Poverty)
options(digits=2)
stat.desc(trump.desvar, basic=F)

#in Biden counties
biden.desvar <- cbind(covid_rate, covid_death, Unemployment, Poverty)
options(digits=2)
stat.desc(biden.desvar, basic=F)

#simple regression of COVID rate on vote for Biden
lm1 <- lm(data$percentage20_Joe_Biden ~ data$covid_rate)
summary(lm1)
plot(x = data$covid_rate, y = data$percentage20_Joe_Biden, type = "p",
     main = "COVID rate and vote for Biden",
     xlab = "COVID contraction rate",
     ylab = "Ratio of vote for Biden")
abline(lm1)

lm2 <- lm(data$percentage20_Joe_Biden ~ data$covid_death)
summary(lm2)
plot(x = data$covid_death, y = data$percentage20_Joe_Biden, type = "p",
     main = "COVID death rate and vote for Biden",
     xlab = "COVID death rate",
     ylab = "Ratio of vote for Biden")
abline(lm2)
