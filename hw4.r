# ========== #
# Homework 4 #
# ========== #


# ==================== #
#     Problem 1        #
# ==================== #
horse <- read.csv("horsepower.txt",header=T)
attach(horse)

# Part (a): Scatterplot of price vs. horsepower
# =============================================
plot(horsepower,price,cex.axis=1.5,cex=1.5,     # Plots price vs. horsepower
     xlab="Horsepower",pch=16,cex.lab=1.6,ylab=    #   with axis labels, and a
       "Price (thousands of $)",cex.main=1.8,main=   #   title.
       "Price vs. Horsepower",mgp=c(2.7,1,0))

# Part (b): Regression of price and horsepower with plotted line
# ==============================================================
reg <- lm(price~horsepower)                     # Regresses price on horsepower
abline(reg,lwd=2)                               # Plots regression line

# Part (c): Prediction interval for the average price 
# ==============================================================
xval <- data.frame(horsepower=280)              # Specify an x value
pred <- predict(reg,xval,interval=              # Computes prediction interval for y at x
                  "prediction")


# Part (e): Residual plot
# =======================
plot(horsepower,reg$resid,cex.axis=1.5,cex=1.5, # Plots residuals vs. horsepower
     xlab="Horsepower",pch=16,cex.lab=1.6,ylab=    #   with axis labels, and a
       "Residuals",cex.main=1.8,main=                #   title.
       "Residual Plot",mgp=c(2.7,1,0))



# ==================== #
# Problem 2 #
# ==================== #
wheatears <- read.csv("wheatears.txt",      # Reads in the wheatears data
  header=T)
attach(wheatears)                           # Promotes the wheatears dataset


plot(mass,tcell,xlab="Mean Stone Mass (g)", # Scatterplot of T-cell Response
  ylab="T-cell Response (mm)",cex.lab=1.6,  #    vs. Stone Mass
  cex.axis=1.5,pch=16,cex=1.5,
  main="T-cell Response vs. Stone Mass",
  cex.main=1.6,mgp=c(2.7,1,0))


exptcell=exp(tcell)
expmass=exp(mass)
reg <- lm(exptcell~expmass,data=wheatears)      # Regression of T-cell on stone mass
summary(reg)      
abline(reg,lwd=2)                         # Plots regression line


# Residual Analysis 
plot(reg$fitted,reg$resid,ylab="Residuals", 	# Residual plot
     xlab="Predicted Values",cex.lab=1.6,cex=1.5,
     pch=16,main="Residuals vs. Predicted Values",
     cex.main=1.8,cex.axis=1.5,mgp=c(2.7,1,0))
abline(h=0,lty=2,lwd=2)                       	# Plots the y=0 line

qqnorm(reg$resid,pch=16,ylab="Residuals",	# Normal quantile plot of model residuals
       xlab="Standard Normal Quantiles",       
       cex.lab=1.6,cex.axis=1.5,cex.main=1.8,
       main="Normal Quantile Plot",cex=1.5,mgp=c(2.7,1,0))

studres <- rstudent(reg)             # Vector of Studentized deleted residuals
wheatears[abs(studres)>2.1,]         # Display outlying obs





