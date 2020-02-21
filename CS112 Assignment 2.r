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










