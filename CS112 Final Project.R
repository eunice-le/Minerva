##### Installing and / or loading packages ####
rm(list=ls())

#from the dataverse code 
list.of.packages <- c("stargazer","visreg","spdep","maptools","rgdal",
                      "maptools","sandwich","lmtest","RCurl", "SnowballC",
                      "wordcloud","RColorBrewer","tm","foreign","dplyr",
                      "fuzzyjoin")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
                if(length(new.packages)){install.packages(new.packages)}

#for regression and matching 
library(arm)
library(foreign)
library(sp)
library(Matching)
library(rgenoud)

###### Load Datasets ######

#Load main Charnysh and Finkel dataset 
load("MainDataset.RData")
#This dataset contains 2 Large Spatial Polygons Data Frames 
#crd has data prior to 1989 
#pol has data after 1989
#Figure 3 represents data from 1945-1970, so we use the 'crd' set 

main.df <- data.frame(crd@data, stringsAsFactors = FALSE)

##################################
###### Recreate figure 3(a) ######
##################################
#Distance to Treblinka and Dwellings built in 1945-1970 (1988)

#y-axis = proportion of new dwellings built in 1945-1970
#create vector 'proportion' for y-axis data

proportion <- rep(0, nrow(main.df))
for(i in (1:nrow(main.df))){
  proportion[i] <- main.df$Dw4570[i]/main.df$Dwel88[i]
}

#add 'proportion' to the data frame 
main.df["proportion"] <- proportion

#create a new dataset that only has distance to Treblinka < 50km 
foo <- main.df[which(main.df$distTreb <= 50),]
#clean empty data 
foo <- foo[complete.cases(foo),]
#create a vector with distances from Treblinka of 1-50 kms
distances = seq(5, 50, 0.5)
len = length(distances)

#recreate the logit regression in Charnysh & Kinkl (2017) table 2
lm1 <- glm(foo$proportion ~ log(foo$distTreb))
summary(lm1)

#Simulating the logit regression above 
iterations = 10000
sim.lm1 <- sim(lm1,n.sims=iterations)

#Create an empty matrix to store the predicted values
ys <- matrix(rep(0,len*iterations), nrow = len, ncol = iterations)

#Fill the matrix:
i = 0
for (d in distances) {
  i = i + 1
  #At each distance, we calculate the log of the distance
  dist.log <- matrix(c(1,log(d)),nrow = 1,ncol = 2)
  #then multiply that with the coefficient from the regression model 
  ys[i,] <- dist.log %*% t(sim.lm1@coef)
}

#Calculate the 95% confidence intervals 
ci <- c()
mid <- c()
for (i in 1:len) {
  ci <- c(ci, quantile(ys[i,], probs = c(0.025, 0.975)))
  mid <- c(mid, quantile(ys[i,], probs = 0.5))
}

#Build a table with the 95% CIs and mean value of the proportion
#transpose the ci dataframe 
ci <- data.frame(t(data.frame(apply(ys, 1, quantile, probs=c(0.025, 0.975)))))
table <- cbind(ci,mid)
rownames(table) <- distances
table

#Plot the CIs and mean prediction of new dwelling proportion for each distance
#mean
plot(x = c(1:50),
     ylim = c(0.44,0.58), xlim = c(5,50),
     xlab = "Distance to Treblinka, km",
     ylab = "Proportion",
     main = "Dwellings built in 1945-1970 (1988)")
#draw the regression line 
lines(x = distances,y = mid,type="l",lwd=2)

#draw the confidence intervals
arrows(distances, table$X2.5., distances, table$X97.5.,
       length = 0, angle = 90, col = "blue")

########################
###### Extension #######
########################
#Use matching to improve balance and robustness

###prepare the data 
#clean emissing values 
main.df <- main.df[complete.cases(main.df),]
#the dataset main.df already has the 'proportion' column 
#'proportion' shows the amount of new dwellings built in 1945-1970 

#divide the data into treatment and control groups 
#create new variable 'treat'
trt <- c()
for(i in (1:nrow(main.df))) {
  if (main.df$distTreb[i] <= 50) {
    trt[i] <- 1
  } else {
    trt[i] <- 0
  }
}
#attach the vector 'treat' to main.df dataset 
main.df$treat <- trt 

### Propensity Score Matching ### 
#Estimate the propensity score 
glm1 <- glm(treat ~ distRail45KM + CityDistKm + Destr46 + Trade82, 
               data = main.df, family = "binomial")
summary(glm1)

X1 <- glm1$fitted
Y1 <- main.df$proportion
Tr1 <- main.df$treat

#Treatment effect
mout1 <-  Match(Tr = Tr1, X=X1)

#Balance check
mbout1 <- MatchBalance(treat ~ distRail45KM + CityDistKm + Destr46 + Trade82, 
                       data = main.df, match.out = mout1, nboots = 1000)
#p.value before matching: 0.044 
#p.value after matching: 2.22e-16 
#propensity score matching worsens the balance 

#re-run Match with Y included 
mout1.y <-  Match(Tr = Tr1, X = X1, Y = Y1)
summary(mout1.y)

### Genetic Matching ###

#set the covariances 
Tr <- main.df$treat
X <- cbind(main.df$distRail45KM, main.df$CityDistKm, 
           main.df$Destr46, main.df$Trade82)
Y <- main.df$proportion

genout <- GenMatch(Tr = Tr, X = X, pop.size = 20, nboots = 250)
mout <- Match(Tr = Tr, X = X, Weight.matrix = genout)
mbout <- MatchBalance(treat ~ distRail45KM + CityDistKm 
                      + Destr46 + Trade82, data = main.df,
                      match.out = mout)
#before matching min p.value: 0.044 
#after matching max p.value: 0.192
#genetic matching significantly improves balance

#re-run Match with Y included 
mout.y <- Match(Tr = Tr, X = X, Y = Y, Weight.matrix = genout)
summary(mout.y)


