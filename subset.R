sat <- read.csv("sat.txt",header=T)
source("Scripts/pairs.panels.r")

# Raw scatterplot pairs
# =====================
lab <- c("SAT","TAKERS","INCOME","YEARS","PUBLIC","EXPEND","RANK")
pairs.panels(sat[,2:8],labels=lab)

# Log scatterplot pairs
# =====================
lab[2] <- "LOGTAK"
sat2 <- data.frame(sat[,1:2],logtak=log(sat[,3]),sat[,4:8])
pairs.panels(sat2[,2:8],labels=lab)

# Sequence of backward elimination model fits
# ===========================================
n <- dim(sat2)[1]          # Computes sample size
library(MPV)               # Loads MPV library for PRESS function
# Install.packages("leaps")
library(leaps)             # Loads leaps library for leaps function

sat2 <- data.frame(sat[,1:2],logtak=log(sat[,3]),sat[,4:8])
X <- sat2[,c(3,4,5,6,7,8)] # Matrix of X-variables
leaps(X,sat2$sat)          # Computes Cp for all subsets


reg1 <- lm(sat~logtak+income+years+public+expend+rank,data=sat2)
summary(reg1)          # Regression summary
anova(reg1)            # ANOVA summary
step(reg1,k=2)         # Computes AIC statistic
step(reg1,k=log(n))    # Computes BIC statistic
PRESS(reg1)            # Computes PRESS statistic (in MPV library)

reg2 <- update(reg1,.~.-public) # Refits without the "public" term
summary(reg2)          # Regression summary
anova(reg2)            # ANOVA summary
step(reg2,k=2)         # Computes AIC statistic
step(reg2,k=log(n))    # Computes BIC statistic
PRESS(reg2)            # Computes PRESS statistic (in MPV library)

reg3 <- update(reg2,.~.-income)
summary(reg3)          # Regression summary
anova(reg3)            # ANOVA summary
step(reg3,k=2)         # Computes AIC statistic
step(reg3,k=log(n))    # Computes BIC statistic
PRESS(reg3)            # Computes PRESS statistic (in MPV library)

reg4 <- update(reg3,.~.-rank)
summary(reg4)          # Regression summary
anova(reg4)            # ANOVA summary
step(reg4,k=2)         # Computes AIC statistic
step(reg4,k=log(n))    # Computes BIC statistic
PRESS(reg4)            # Computes PRESS statistic (in MPV library)

reg5 <- update(reg4,.~.-years)
summary(reg5)          # Regression summary
anova(reg5)            # ANOVA summary
step(reg5,k=2)         # Computes AIC statistic
step(reg5,k=log(n))    # Computes BIC statistic
PRESS(reg5)            # Computes PRESS statistic (in MPV library)

reg6 <- update(reg5,.~.-expend)
summary(reg6)          # Regression summary
anova(reg6)            # ANOVA summary
step(reg6,k=2)         # Computes AIC statistic
step(reg6,k=log(n))    # Computes BIC statistic
PRESS(reg6)            # Computes PRESS statistic (in MPV library)


# Backward Elimination using step()
# =================================
null<-lm(sat~1,data=sat2)
full<-lm(sat~.-state,data=sat2)
bwd <- step(full,direction="backward")   # By default, use AIC metric





# All Possible Regression 
# =======================
subsets<-regsubsets(sat~logtak+income+years+public+expend+rank,data=sat2,
nbest=1)                  # Specify # of models of each size kept in the object
plot(subsets)             # Plot best models of each size using BIC
plot(subsets, scale="Cp") # Plot best models according to Cp
summary(subsets)$cp       # Return Cp for each model (can also be $bic, $adjr2) 


# Mallow's Cp Plot
# ================
mallow<-leaps(X,sat2$sat)          # Computes Cp for all subsets models
plot(mallow$size, mallow$Cp,       # Plots Cp v.s p
     main="Cp Plot",xlab="p", ylab="Cp", 
     xlim=c(2,7), ylim=c(0,15),
     pch=19, cex=1.4) 
abline(0, 1,lwd=2)                 # Add reference line y=x
