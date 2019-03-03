# =========== #
# Homework #6 #
# =========== #

# ==========#
# Problem 1 #
# ==========#

install.packages("faraway")    # Installs the farway package
	                             # Select local mirror like USA(PA 1) 
library(faraway)               # Opens the faraway library
data(seatpos)                  # Loads the seat position data
attach(seatpos)                # Allow to quote var names without
					                     # need of dataset name 
# Part (a)
# ========
reg1 <- lm(hipcenter~.,data=seatpos)      # Multiple linear regression fit
summary(reg1)                             # Regression summary

# Part (b)
# ========
round(cor(seatpos[,1:8]),digits=2)        # Correlation matrix for x-variables

# Part (c)
# ========
vif(reg1)                                 # Variance inflation factors

# Part (d)
# ========
reg2 <- lm(hipcenter~Age+Weight+Ht+       # Fits model for hipcenter on age,
             I(Age*Weight),data=seatpos)  #   weight, ht, and age*weight
summary(reg2)                             # Regression summary
