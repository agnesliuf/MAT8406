# =========== #
# Homework 11 #
# =========== #


# =================================== #
# Problem 1: Speed vs. Sex #
# =================================== #
maxspeed <- read.csv("Data/maxspeed.txt",header=T) # Reads in the maximum speed data.
summary(maxspeed)                                  # Summarizes the data set.

# Part (a)
# ========
boxplot(speed~sex,data=maxspeed,cex.axis=1.5,ylab= # Side-by-side boxplots of maximum speed for
  "Maximum Speed Driven (mph)",names=c("Female",   #   females and males
  "Male"),main="Boxplot of Maximum Speed by Sex",
  cex.main=1.8,cex.lab=1.6,mgp=c(1.5,1,0))

# Part (b)
# ========
sexnum <- as.numeric(maxspeed$sex=="F")            # Converts sex to numeric variable (1=F, 0=M)
logmod <- glm(sexnum~maxspeed$speed,family=        # Logistic model fit of sex on maximum speed
  binomial)
summary(logmod)                                    # Summary of logistic fit

# Part (c)
# ========
LD50 <- -coef(logmod)[1]/coef(logmod)[2]           # Computes the LD50 maximum speed

# Part (d)
# ========
sextest <- rep(0,length(sexnum))                   # Blank vector of length n
sextest <- as.numeric(logmod$fitted>0.5)           # Binary vector set to 1 if the prob. of being
                                                   #   domestic exceeds 0.50 and 0 otherwise
table(maxspeed$sex,sextest)                        # Contingency table of type outcomes

# ============================================= #
# Problem 2: O-ring Failure vs. Temp #
# ============================================= #
library(faraway)                                  # Loads faraway library
shuttle <- read.csv("Data/shuttle.txt",header=T)  # Reads in the O-ring/temp data.
summary(shuttle)                                  # Summarizes the data set.

# Part (a)
# ========
logmod <- glm(failure~temp,data=shuttle,family=   # Logistic model of O-ring on temp
  binomial)
summary(logmod)                                   # Summary of logistic fit

# Part (b)
# ========
1-pchisq(5.945,1)                                # Chi-square p-value for LRT test

# Part (c)
# ========
x0 <- c(1,31)                                     # Explanatory vector (x=31)
(pred <- ilogit(sum(coef(logmod)*x0)))            # Predicted O-ring failure
