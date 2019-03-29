##### Partial Regression Plot #########
mod1<-lm(y~x1)     # Existing model
mod2<-lm(x2~x1)    # Regress new predicator on existing ones
plot(mod2$resid, mod1$resid, main='y|x1 ~ x2|x1')  # Partial regression plot



reg<-lm(y~x1+x2,data=dat)          # Regression of y on x1, x2

##### Residual plot against x1 shows fan shape  ########
reg1<-lm(reg$resid^2~x1,data=dat)  # Regress squared residuals on x1
wlsreg<-lm(y~x1+x2,data=dat, weights=(1/reg1$fitted))  # WLS: estimate weights with a function of x1


bcox<-boxcox(y~x1+x2,data=dat,
       lambda = seq(-2, 2, length = 20)) # Compute and plot log-likelihoods for lambda in Box-Cox 
lambda<-bcox$x[bcox$y==max(bcox$y)]      # Return maximizing lambda for Box-Cox tr.