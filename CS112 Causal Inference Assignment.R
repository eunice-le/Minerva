#Question 1: "Daughters"

# Read Section 5 (which is only about 1 page long!) of 
# Iacus, King, Porro (2011), Multivariate Matching Methods 
# That Are Monotonic Imbalance Bounding, JASA, V 106, N. 493,
# available here:
# https://gking.harvard.edu/files/gking/files/cem_jasa.pdf
# Don't worry about the "CEM" elements. Focus on the "daughters" case study.

# Data for this case study is available in "foo" below.
foo <- read.csv(url("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00089202-1711/daughters.csv"))

##PART (A)
## Before doing any matching, RUN A REGRESSION, with Y = nowtot, and the independent vars mentioned below:
# Dems, Repubs, Christian, age, srvlng, demvote
# Treatment  = hasgirls
## Show the regression specification.
lm1 <- lm(nowtot ~ hasgirls+Dems+Repubs+Christian+age+srvlng+demvote, data = foo)
summary(lm1)

## Use the regression to estimate a treatment effect and confidence interval. Check the balance of this not-matched data set.
lm1$coefficients
#the coefficient for treatment variable 'hasgirls' is -0.452, so treatment effect is -0.452
confint.lm(lm1,'hasgirls')
#confidence interval for treatment effect: 2.5%: -4.194; 97.5%: 3.29
MatchBalance(foo$hasgirls ~ foo$Dems + foo$Repubs + foo$Christian + foo$age + 
               foo$srvlng + foo$demvote, nboots=250)
#Balance before matching is bad with a small p.value pf 2.22e-16

## Then, do genetic matching, using the draft code below. 
## Use the same variables as in the regression above.

## Summarize (in 5-15 sentences) the genetic matching procedure and results,
## including what you matched on, what you balanced on, and what your balance results were.
## Provide a link to your code, and provide output for Match() and MatchBalance() in the body of your submission.

foo <- read.csv(url("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00089202-1711/daughters.csv"))

# Table 1 in Ebonya
# Indep. vars
# anygirls: Any female children
# ngirls: Number of female children
# totchi: Total number of children

# white: White=1, 0 otherwise
# female: Female=1, 0 otherwise
# age: age
# srvlng: Service length (years)

# reg1 - reg9 are regional binary variables
# binary religious variables: none, Protestant, Catholic, OtherC, OtherR
# binary party variables: Dems, Repubs, OthParty
# DEPENDENT VARIABLE: nowtot

## Set pop.size = 20, set nboots = 250... complete the code below...
library(Matching)
library(rgenoud)
set.seed(2324) 

Tr <- foo$hasgirls
X <- cbind(foo$Dems,foo$Repubs,foo$Christian,foo$age,foo$srvlng,foo$demvote)
Y <- foo$nowtot 

genout <- GenMatch(Tr = Tr, X = X, pop.size = 20, nboots = 250)

mout <- Match(Tr = Tr, X = X, Weight.matrix = genout)

mbout <- MatchBalance(hasgirls~ Dems+Repubs+Christian+
                        age+srvlng+demvote, data = foo, match.out = mout)

## If you obtain high balance, consider rerunning with M = 2 or 3...
## If/when you are satisfied by balance achieved, then rerun Match() with Y included 
## and obtain the treatment effect estimate, the standard error, and the confidence interval.

mout1 <- Match(Y = Y, Tr = Tr, X = X, Weight.matrix = genout)
summary(mout1)

#confidence interval for the treatment effect #I consulted Cyrus's code for this 
c(mout1$est-1.96*mout1$se, mout1$est+1.96*mout1$se)
#2.5%: -2.89; 97.5%: 6.255

# PART (B)
# Repeat everything  you've done for Part (A), including the regression, genetic algorithm, code, output, 
# and 5-15 sentences, EXCEPT
# this time change the definition of treatment to cover 2 girls, and change the definition of
# control to cover 2 boys. Exclude all observations that don't meet these requirements.
# Be sure to explain (in a sentence or two) what you're doing with
# your new treatment and control definitions. Do your new definitions change anything?

#Definition of new treatment and control 
#Treatment group: members with at least 2 girls and no boy
#Control group: members with at least 2 boys and no girl 
#Divide the dataset into new treatment and control groups 
Tr1 <- foo[which(foo$ngirls >= 2),]
Tr1 <- Tr1[which(Tr1$nboys == 0),]
Cr1 <- foo[which(foo$nboys >= 2),]
Cr1 <- Cr1[which(Cr1$ngirls == 0),]
#Combine Tr1 and Cr1 to form a new dataset 
foo1 <- rbind(Tr1, Cr1)

#New regression
lm2 <- lm(nowtot~ hasgirls + Dems + Repubs + Christian + age + srvlng + demvote,
            data= foo1)
summary(lm2)
lm2$coefficients
#The coefficient for treatment variable 'hasgirls' is 12.292, so the treatment effect is 12.292 
confint(lm2, 'hasgirls')
#Confidence interval for treatment effect: 2.5%: 5.331, 97.5%: 19.254
MatchBalance(foo1$hasgirls ~ foo1$Dems + foo1$Repubs + foo1$Christian + 
               foo1$age + foo1$srvlng + foo1$demvote, nboots=250)
#Before matching minimum p.value is 0.048 < 0.05. So the balance is still low. 

#Genetic matching
set.seed(2324) 
Tr2 <- foo1$hasgirls
X2 <- cbind(foo1$Dems,foo1$Repubs,foo1$Christian,foo1$age,foo1$srvlng,foo1$demvote)
Y2 <- foo1$nowtot 

genout2 <- GenMatch(Tr = Tr2, X = X2, pop.size = 20, nboots = 250)

mout2 <- Match(Tr = Tr2, X = X2, Weight.matrix = genout2)

mbout2 <- MatchBalance(hasgirls~ Dems+Repubs+Christian+
                        age+srvlng+demvote, data = foo1, match.out = mout2)

mout3 <- Match(Y= Y2, Tr = Tr2, X = X2, Weight.matrix = genout2)
summary(mout3)

#Confidence Interval for the treatment effect 
c(mout3$est-1.96*mout3$se, mout3$est+1.96*mout3$se)
#2.5%: 4.361; 97.5%: 20.32
