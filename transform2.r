# Scatterplot of heart disease mortality vs. wine consumption
# ===========================================================
hwdat <- read.csv("Data/heartwine.txt",  # Reads in the heart disease and
  header=T)                              #   mortality data
plot(hwdat$wine,hwdat$mortality,pch=16,  # Plots mortality vs. wine
  xlab="Wine Consumption (l/person/yr)", #   consumption with axis labels
  cex.lab=1.6,cex.axis=1.5,cex=1.5,      #   and a title
  ylab="Heart Disease Mortality (deaths/1000)",
  main="Heart Disease vs. Wine Consumption",
  cex.main=1.8,mgp=c(2.7,1,0))

# Scatterplot of heart disease mortality vs. wine consumption
# ===========================================================
par(mfrow=c(1,1),mar=c(5,4,4,2)+.1)      # Creates a 1x1 graphics window
logwine <- log10(hwdat$wine)             # Log wine consumptions
plot(logwine,hwdat$mortality,pch=16,xlab=# Plots mortality vs. log(wine)
  "Log Wine Consumption (l/person/yr)",  #   consumption with axis labels
  cex.lab=1.6,cex.axis=1.5,cex=1.5,      #   and a title
  ylab="Heart Disease Mortality (deaths/1000)",
  main="Heart Disease vs. Log Wine Consumption",
  cex.main=1.8,mgp=c(2.7,1,0))
abline(lm(hwdat$mortality~logwine),lwd=2)# Plots regression line

