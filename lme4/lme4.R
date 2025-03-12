# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# Meta-analysis of test accuracy studies Use lme4 With (In) R Software
install.packages("lme4")
library("lme4")
X = read.csv("https://raw.githubusercontent.com/timbulwidodostp/lme4/main/lme4/lme4.csv",sep = ";")
# Estimation Meta-analysis of test accuracy studies Use lme4 With (In) R Software
X$n1 <- X$tp+X$fn  
X$n0 <- X$fp+X$tn  
X$true1 <- X$tp  
X$true0 <- X$tn  
X$recordid <- 1:108
Y = reshape(X, direction = "long", varying = list(c("n1", "n0"),  
c("true1","true0")), timevar = "sens", times = c(1,0),  
v.names = c("n","true")) 
Y = Y[order(Y$id),]
Y$spec<- 1-Y$sens
Y.CT = Y[Y$test=="CT",] 
Y.MRI = Y[Y$test=="MRI",]
(MA_Y.CT = glmer(formula = cbind(true, n - true) ~ 0 + sens + spec + (0+sens + spec|Study_ID), data = Y.CT, family = binomial,  nAGQ = 1, verbose = 2 )) 
(ma_Y.CT = summary(MA_Y.CT))
glmer(formula = cbind(true, n - true) ~ 0 + sens + spec + (0+sens|Study_ID) + (0+spec|Study_ID), data = Y.CT, family = binomial,  nAGQ = 1, verbose = 2)
(summary(MA_Y.CT))$vcov
Result_1 <- (ma_Y.CT = summary(MA_Y.CT))
Result_2 <- (summary(MA_Y.CT))$vcov
Result_1
Result_2
# Meta-analysis of test accuracy studies Use lme4 With (In) R Software
# Olah Data Semarang
# WhatsApp : +6285227746673
# IG : @olahdatasemarang_
# Finished