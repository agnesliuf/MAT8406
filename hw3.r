# ========== #
# Homework 3 #
# ========== #

# ==================== #
# Problem 1 #
# ==================== #
recov <- read.csv("recovery.txt",       # Reads in the biological recovery data
                  header=T)
attach(recov)                                # Promotes the biological recovery data

# Part (a): 
# =========
logrec <- log(recov$recovery)                # Computes the log recovery percentages
reg.out <- lm(logrec~time)                   # Regression of log recovery on time
summary(reg.out)                             # Prints regression summary




# ==================== #
#     Problem 2        #
# ==================== #

big<-read.csv("bigbang.txt")
attach(big)

reg1<-lm(distance~velocity)
summary(reg1)

reg<-lm(distance~velocity-1)
summary(reg)

confint(reg, "velocity")   # Computes CI for slope of velocity
