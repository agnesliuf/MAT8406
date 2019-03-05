# =========== #
# Homework #7 #
# =========== #

# ==================== #
# Problem 1 #
# ==================== #
evol <- read.csv("evolution.txt",    # Reads in the evolution data
  header=T)
attach(evol)                              # Promotes the dataset

# Part (a)
# ========
plot(latitude,wingsize,cex.axis=1.5,      # Labeled scatterplot of wing size vs.
  cex=1.5,xlab="Latitude",ylab=           #   latitude for both NAM and EUR flies
  "Wing Size (thous.log mm)",cex.lab=1.6,
  pch=c(1,16)[unclass(as.factor(continent))],
  mgp=c(2.7,1,0))
reg1 <- lm(wingsize[continent=="NAM"]~    # Regression of wing size on latitude
  latitude[continent=="NAM"])             #   for NAM flies
reg2 <- lm(wingsize[continent=="EUR"]~    # Regression of wing size on latitude
  latitude[continent=="EUR"])             #   for EUR flies
abline(reg1,lwd=2)                        # Plots fitted line for surface
abline(reg2,lwd=2,lty=2)                  # Plots fitted line for deep
legend(36,850,c("NAM Flies","EUR Flies"), # Places a legend on the plot for
  lwd=c(2,2),lty=c(1,2),pch=c(16,1),      #   NAM and EUR flies at (36,850)
  cex=1.5)

# Parts (c),(d)
# =============
reg3 <- lm(wingsize~latitude*continent)   # Interaction model with latitude, continent
summary(reg3)
confint(reg3)                             # Confidence intervals for model parameters
