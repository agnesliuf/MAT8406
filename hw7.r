# =========== #
# Homework #7 #
# =========== #

# ========================= #
# Problem 1 # evolution
# ========================= #
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


# ============================================= #
# Problem 2: Bat data #
# ============================================= #
bats <- read.csv("Data/bats.txt",header=T)     # Reads in the bat data
attach(bats)                                   # Promotes the data set
plot(logmass,logenergy,xlab="Log Mass",ylab=   # Scatterplot of log energy expend.
       "Log Energy Expenditure",cex.axis=1.5,       #   vs. log body mass with a separate
     pch=c(4,16,1)[unclass(type)],cex=1.5,        #   plotting symbol for the three
     cex.lab=1.6,mgp=c(2.7,1,0))                  #   types of species
reg1.out <- lm(logenergy[type=="Non-echolocating bats"]~
                 logmass[type=="Non-echolocating bats"])      # Regression for non-echo bats
abline(reg1.out,lwd=2,lty=3)                   # Plots the regression line
reg2.out <- lm(logenergy[type=="Non-echolocating birds"]~
                 logmass[type=="Non-echolocating birds"])     # Regression for non-echo birds
abline(reg2.out,lwd=2,lty=1)                   # Plots the regression line
reg3.out <- lm(logenergy[type=="Echolocating bats"]~
                 logmass[type=="Echolocating bats"])          # Regression for echo bats
abline(reg3.out,lwd=2,lty=2)                   # Plots the regression line
legend(1.8,0.5,c("Non-echo bats",              # Places a legend on the plot to
                 "Non-echo birds","Echo bats"),pch=c(4,16,1), #   distinguish the three species
       lwd=c(2,2,2),lty=c(3,1,2),cex=1.3)           #   types, at (1.8,0.5)

reg4.out <- lm(logenergy~logmass*type)         # Interaction model
summary(reg4.out)                              # Summary of interaction model
anova(reg4.out)                                # ANOVA for interaction model



# ============================ #
# Problem 3: Sludge #
# ============================ #
sludge <- read.csv("Data/sludge.txt",header=T) # Reads in the sludge data
summary(sludge)                                # Data summary
attach(sludge)                                 # Attaches the sludge data

# Part (a)
# ========
plot(soilconc,plantconc,cex.axis=1.5,cex=1.5,  # Plot of plant content vs.
     pch=c(1,2,16)[unclass(type)],cex.lab=1.6,    #   soil content
     xlab="Soil Mercury Content",ylab="Plant Mercury Content",
     mgp=c(2.7,1,0))
legend(1.5,150,c("Barley","Corn","Wheat"),     # Places a legend on the plot
       pch=c(1,2,16),cex=1.5)

# Part (c)
# ========
x2 <- as.numeric(type=="Corn")                 # Indicator variable for corn
x3 <- as.numeric(type=="Wheat")                # Indicator variable for wheat
reg1 <- lm(plantconc~soilconc+x2+x3+           # Fits a quadratic model for
             I(soilconc*x2)+I(soilconc*x3)+I(soilconc^2)+ #   the three crop types
             I(soilconc^2*x2)+I(soilconc^2*x3))
summary(reg1)                                  # Regression summary
anova(reg1)                                    # Regression ANOVA
