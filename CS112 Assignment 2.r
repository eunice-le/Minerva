###QUESTION 1
###Part (a)
sesame <- read.csv("https://tinyurl.com/wlgl63b")
head(sesame)
#Separate treatment group from control group
which.treat <- which(sesame$treatment== 1)
which.control <- which(sesame$treatment == 0)
sesame.treat <- sesame[-which.control,]
sesame.control <- sesame[-which.treat,]
#linear regression of pre.test and post.test for treatment group and control group
lm.treat <- lm(post.test~pre.test, sesame.treat)
lm.control <- lm(post.test~pre.test, sesame.control)
lm.treat
lm.control
#plotting the regression
#Treatment group: circle, control group: black dot
#Treatment group regression line: solid, Control group regression line: dash
plot(x = sesame$pre.test, y = sesame$post.test,
      pch = ifelse(sesame$treatment == 1, 1, 16),
      main = "Grade 4",
      xlab = "Pre-test", 
      ylab = "Post-test")
abline(lm.treat,
       col = "black")
abline(lm.control,
       col = "black",
       lty = 2)

###Part (b)
###Part (b)
#Set up a new data set for modification 
sesame.b <- read.csv("https://tinyurl.com/wlgl63b")
sesame.b$post.test[c(16)]<-85
#Separate treatment from control groups
which.treat.b <- which(sesame.b$treatment== 1)
which.control.b <- which(sesame.b$treatment == 0)
sesame.treat.b <- sesame.b[-which.control,]
sesame.control.b <- sesame.b[-which.treat,]
#Linear regression between pre.test and post.test 
lm.treat.b <- lm(post.test ~ pre.test, sesame.treat.b)
lm.control.b <- lm(post.test ~ pre.test, sesame.control.b)
lm.treat.b
lm.control.b
#Plotting the regression 
plot(x = sesame.b$pre.test, y = sesame.b$post.test,
     pch = ifelse(sesame.b$treatment == 1, 1, 16),
     col = ifelse(sesame.b$post.test == 85, "blue", "black"),
     main = "Grade 4",
     xlab = "Pre-test", 
     ylab = "Post-test")
abline(lm.treat.b,
       col = "black")
abline(lm.control.b,
       col = "black",
       lty = 2)











