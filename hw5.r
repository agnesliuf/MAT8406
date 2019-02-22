# =========== #
# Homework #5 #
# =========== #

# ==================== #
# Problem 1 #
# ==================== #
drug <- read.csv("drug.txt",header=T)
attach(drug)

# Part (a)
# ========
plot(dose,response,xlab="Dose Level",pch=16,  # Plots response vs. dose with
  ylab="Response (drug strength)",cex=1.5,    #   axis labels and a closed
  cex.lab=1.6,cex.axis=1.5,mgp=c(2.7,1,0),    #   circle
  main="Response vs. Dose",cex.main=1.8)

# Part (b)
# ========
reg <- lm(response~dose)                      # Regression of response on dose
summary(reg)                                  # Regression summary

# Part (c)
# ========
plot(dose,reg$resid,xlab="Dose Level",pch=16, # Makes a residual plot of
  ylab="Residuals",cex=1.5,cex.lab=1.6,       #   the residuals vs. the
  cex.axis=1.5,mgp=c(2.7,1,0),main=           #   x-values (doses)
  "Residual Plot",cex.main=1.8)
abline(h=0,lwd=2,lty=2)                       # Plots the zero-line

# Part (d)
# ========
purelof(dose,response)              # Conducts a model lack of fit test

# Part (e)
# ========
ldose <- log(dose)                            # Log of the doses
plot(ldose,response,xlab="Log Dose",pch=16,   # Plots response vs. dose with
  ylab="Response (drug strength)",cex=1.5,    #   axis labels and a closed
  cex.lab=1.6,cex.axis=1.5,mgp=c(2.7,1,0),    #   circle
  main="Response vs. Log Dose",cex.main=1.8)
reg <- lm(response~ldose)                     # Regresses y on log dose
abline(reg,lwd=2)                             # Plots the regression line
summary(reg)                                  # Regression summary
plot(ldose,reg$resid,xlab="Log Dose",         # Makes a residual plot of
  ylab="Residuals",cex=1.5,cex.lab=1.6,       #   the residuals vs. the
  cex.axis=1.5,mgp=c(2.7,1,0),main=           #   log doses
  "Residual Plot",cex.main=1.8,pch=16)
abline(h=0,lwd=2,lty=2)                       # Plots the zero-line
purelof(log(dose),response)                   # Model LOF test


# ==================== #
# Problem 2 #
# ==================== #
natal <- read.csv("natal.txt",      # Reads in the natal data
  header=T)
attach(natal)                            # Promotes the natal dataset

# Part (a)
# ========
plot(bodymass,maxdist,xlab="Body Weight", # Scatterplot of maximum dispersal
  ylab="Dispersal Distance",cex.lab=1.6,  #   distance vs. body mass with
  cex.axis=1.5,pch=16,cex=1.5,main=       #   axis labels
  "Dispersal Distance vs. Body Weight",
  cex.main=1.6,mgp=c(2.7,1,0))

# Part (b)
# ========
logdist <- log(maxdist)                  # Log dispersal distance
logbody <- log(bodymass)                 # Log body mass
plot(logbody,logdist,cex.lab=1.6,pch=1,  # Scatterplot of log distance vs. log
  xlab="Log Body Weight",cex.axis=1.5,   #   body weight with axis labels
  ylab="Log Dispersal Distance",cex=1.5,
  main="Log Dispersal Distance vs. Log Body Weight",cex.main=1.6,
  mgp=c(2.7,1,0))
points(logbody[type=="Carnivore"],       # Plots log distance vs. log body mass
  logdist[type=="Carnivore"],pch=2,      #   for carnivores
  cex=1.5)
points(logbody[type=="Herbivore"],       # Plots log distance vs. log body mass
  logdist[type=="Herbivore"],pch=16,     #   for herbivores
  cex=1.5)
legend(-5.6,6,c("Omnivore","Carnivore",  # Legend for the three types at (-5.6,6)
  "Herbivore"),pch=c(1,2,16),cex=1.5)

# Part (c)
# ========
reg.out <- lm(logdist~logbody+type)      # Multiple linear regression model
summary(reg.out)                         # Regression summary

# Part (d)
# ========
studres <- rstudent(reg.out)             # Vector of Studentized deleted residuals

