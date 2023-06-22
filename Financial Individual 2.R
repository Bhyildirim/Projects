## MSc Business Analitics- Finance Individual Assignment by Beyza Yildirim Crypto

## Crypto Currency Lists; Dogecoin (DOGE), Ethereum (ETH),SOC,AVAX, The Sandbox, 

#setup
library(tidyverse)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(quantmod)
install.packages("date, intersect, setdiff, union")
install.packages("PerformanceAnalytics")
install.packages("as.zoo.data.frame zoo")
install.packages(c("tidyverse", "zoo", "xts", "ggplot2", "vars", "lmtest"))
install.packages("as.Date, as.Date.numeric, zoo, first, last, xts,strucchange, sandwich, urca, lmtest, as.zoo.data.frame zoo")
install.packages("strucchange, sandwich")

##1st Bitcoin; Dogecoin (DOGE)
DOGE_USD <- read_csv("Downloads/DOGE-USD.csv")
View(DOGE_USD)

# View the first few rows of the data
head(DOGE_USD)

# Clean The Data
# Remove any rows with using na.omit()
DOGE_USD <- na.omit(DOGE_USD)

# Remove any non-numeric or missing values
DOGE_USD <- na.omit(as.numeric(DOGE_USD)
                   
# Checking any tied values
DOGE_USD <- unique(DOGE_USD)
                   
# Sort the data by date 
DOGE_USD<- setorder(DOGE_USD)



##Calculate correlations
library(tidyverse)
library(zoo)
library(xts)
library(ggplot2)
library(vars)
library(lmtest)
library(quantmod)

##CONVERT IT TIME SERIES
DOGE_USD <- DOGE_USD %>% 
  mutate(Date = as.Date(Date, "%Y-%m-%d"))
DOGE_USD <- xts(DOGE_USD[, -1], order.by = DOGE_USD$Date)

##Calculation of the log returns of DOGE-USD
DOGE_USD_returns <- diff(log(DOGE_USD))

# Get S&P 500 data and calculate returns
sp500 <- getSymbols("^GSPC", auto.assign = FALSE)
sp500_returns <- diff(log(Cl(sp500)))

# Merge DOGE-USD and S&P 500 data
merged_data <- merge(DOGE_USD, sp500_returns)

# Calculate the correlation between DOGE-USD and S&P 500 returns
correlation <- cor(merged_data[,1], merged_data[,2])

##Last Step for Calculating correlations
sp500_returns <- diff(log(as.xts(sp500)))


# Calculate Variance
# Remove rows with missing values
merged_data <- na.omit(merged_data)

# Fit VAR model
var_model <- VAR(merged_data, p = 2)
Summary(variance)

# Calculate causality
library(lmtest)

# Generate two time series
x <- rnorm(100)
y <- rnorm(100)

# Perform Granger causality test
grangertest(y ~ x, order = 2)


# Calculate Volatility
DOGE_USD_volatility <- volatility(DOGE_USD, calc="garman.klass", N=365)
summary(volatility)

#RUN THE NECESSARILY TESTS

# Linear regression
x <- c(1,2,3,4,5)
y <- c(2,4,6,8,10)
# perform linear regression
model <- lm(y ~ x)
# view the summary of the model
summary(model)

# p values less than 0.05. we reject the null hypothesis that the data are not from the normal distribution


#Regressions' code necessarily to add it before the calculation

# a function to create CSAD and Rm
exchange.herd = function(return) 
{
  n=ncol(return)
  Rm = rowMeans(return)
  temp_dif =abs(return-Rm)
  temp_sum = rowSums(temp_dif)
  CSAD = temp_sum / ncol(return)
  CSAD = cbind (CSAD, Rm)
  return (CSAD)
}


head (f) # show the first 6 rows

CSAD.df = fortify.zoo(f) # converting f into a dataframe (to simplify further calculations)
CSAD.df$Rm2 = CSAD.df$Rm^2 # calculating Rm^2
CSAD.df = CSAD.df[-c(1),] # removing the first row with NAs
head (CSAD.df) # show the first 6 rows
tail (CSAD.df) # show the last 6 rows


#reassign my columns as Y and Xs to look better in the regression model
y = CSAD.df$CSAD  # reassign my columns as Y and Xs to look better in the regression model
x1 = abs (CSAD.df$Rm)
x2 = CSAD.df$Rm2


#Bayesian-regression:
merged_data <- as.data.frame(merged_data)
length(DOGE_USD)
length(sp500_returns)

install.packages("rstanarm")
install.packages("Rcpp")
install.packages("ar, brms, Rcpp")
install.packages("dirichlet, exponential, get_y, lasso, ngrps")

library (brms)
hourly = cbind(y, x1, x2)
model = brm(formula = y ~ x1+x2, 
            data    = hourly,
            seed    = 123)
summary(model)

# Markow regime-switching model
library (MSwM)
nstates <- 2 
msEuro = msmFit(linearMod, k = nstates, sw = rep(TRUE, 4)) 
summary(msEuro) 
plotProb(msEuro ,which=1)


#Two sample t-test for the difference in sample means
t.test(DOGE_USD$High,DOGE_USD$Low,alternative = "two.sided", var.equal=TRUE)
# p value is less than 0.05 thus, we reject the null hypothesis that the data are not from the normal distribution

set.seed("119933")
par(mfrow=c(1,2)) # to build one  figures in one row and two columns  
x1 = rlnorm(20,meanlog=1,sdlog=2) # generate random values from the Log Normal Distribution, mean of the distribution on the log scale
hist(x1) # build the histograms 

x2 = rlnorm(20,meanlog=3,sdlog=2)
hist(x2)

#Welch Two Sample t-test
boxplot(list(x1,x2),main="(a) no transformation")
boxplot(list(log(x1),log(x2)),main="(b) log transformation")
t.test(x1,x2,equal.var=F)
t.test(log(x1),log(x2))


# skewness
library(e1071)


# D'AGOSTINO TEST OF SKEWNESS
library (moments)
agostino.test(return, alternative = "two.sided")

# Anscombe-Glynn test of kurtosis
anscombe.test (return, alternative = "two.sided" )

#Bonett-seier test of kurtosis
bonett.test (return, alternative = "two.sided" )

# Remove missing values
DOGE_USD <- na.omit(DOGE_USD)

# Shapiro-Wilk test
shapiro.test (DOGE_USD$Close)

# Kolmogorov-Smirnov
library (fBasics)
# extract daily closing prices
DOGE_USD <- DOGE_USD$Close
# One-sample Kolmogorov-Smirnov test
ks.test(DOGE_USD$Close, "pnorm")


# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(DOGE_USD$Close)

#Anderson-Darling goodness of fit test
library (ADGofTest)
library(fBasics)
ad.test (DOGE_USD$Close, plnorm)


## pearson Correlation TEST
cor.test (DOGE_USD$High, DOGE_USD$Low, method="pearson",alternative="two.sided",conf.level = 0.95)

#spearman rank correlation test
cor.test(DOGE_USD$High, DOGE_USD$Low, method="spearman",alternative="two.sided")
#The Spearman rank correlation between x and y is 0.978

#KENDALL’S TAU CORRELATION COEFFICIENT TEST
cor.test(DOGE_USD$High, DOGE_USD$Low, method="kendal",alternative="two.sided")

# Essential of the calculation of the Plots
# in order to calculate it i need to make some measurements
library(robust)
library(faraway)
set.seed(99) # to insure replicability
x = 1:11 # create a variable x with values from 1 to 11
x[11] = 50 # replace the 11th element with 50 (anomaly)
y=1+x+rnorm(11) # generate y that depends on x
y2 = y 
y2[11] = y[11]-45
x2 = x
x2[11] = 5.5
cexx = c(rep(21,10),19) # create cexx with 10 values of 21, and the last element is 19
install.packages("fit.models")

# Leverage Plots
par(mfrow=c(2,2),lwd=1,pch=19)
plot(hatvalues(lm(y~x)),ylab="leverage",main="(a)",ylim=c(0,1))
plot(hatvalues(lm(y2~x)),ylab="leverage",main="(b)",ylim=c(0,1))
plot(hatvalues(lm(y~x2)),ylab="leverage",main="(c)",ylim=c(0,1))
plot(x2,hatvalues(lm(y~x2)),xlab="x",ylab="leverage", main="(d)",ylim=c(0,1))



# Residual Plots -  rstudent()
par(mfrow=c(2,3),lwd=1,pch=19)
plot(rstudent(lm(y~x)),ylab="studentized residual",main="Dataset (a)")
plot(rstudent(lm(y2~x)),ylab="studentized residual",main="Dataset (b)")
plot(rstudent(lm(y~x2)),ylab="studentized residual",main="Dataset (c)")
plot(residuals(lm(y~x)),ylab="residual",main="Dataset (a)")
plot(residuals(lm(y2~x)),ylab="residual",main="Dataset (b)")
plot(residuals(lm(y~x2)),ylab="residual",main="Dataset (c)")

# Cook's distance
par(mfrow=c(2,3),cex.axis=1,cex.lab=1,lwd=1,pch=19)
plot(sqrt(cooks.distance(lm(y~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (a)",ylim=c(0,11))
plot(sqrt(cooks.distance(lm(y2~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (b)",ylim=c(0,11))
plot(sqrt(cooks.distance(lm(y~x2))),ylab=("square root Cook's D"),cex=1,main="Dataset (c)",ylim=c(0,11))
halfnorm( sqrt(cooks.distance(lm(y~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (a)",xlim=c(0,1.85))
halfnorm(sqrt(cooks.distance(lm(y2~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (b)", xlim=c(0,1.85))
halfnorm(sqrt(cooks.distance(lm(y~x2))),ylab=("square root Cook's D"),cex=1,main="Dataset (c)", xlim=c(0,1.85))

# Estimate a statistical factor model. What is the pattern of factor influence? Interpret.

# calculate asset returns
DOGE_USD_returns <- DOGE_USD %>%
  mutate(Close_lag = lag(Close)) %>%
  filter(!is.na(Close_lag)) %>%
  mutate(DOGE_USD_return = Close / Close_lag - 1) %>%
  select(Date, DOGE_USD_return)

# merge with the S&P 500 return
merged_data <- na.omit(merged_data)

# calculate correlation matrix
cor_mat <- cor(merged_data[, -1])

# perform principal component analysis
library(psych)

# Compute the correlation matrix
corr_matrix <- cor(DOGE_USD[, 2:6])

# Perform PCA on the correlation matrix
pca <- principal(corr_matrix, nfactors = 2, rotate = "varimax")

# View the factor loadings
print(pca$loadings)

# Economic Factors
library(tidyverse)
library(psych)
library(GPArotation)


# Calculate weekly returns
DOGE_USD <- DOGE_USD %>% 
mutate(weekly_return = (Close - lag(Close))/lag(Close))


DOGE_USD_sub <- na.omit(DOGE_USD_sub)


# Subset the data
DOGE_USD_sub <- DOGE_USD[, c("Open", "High", "Low", "Close", "Volume", "weekly_return")]

# Factor analysis without rotation
fa_none <- factanal(DOGE_USD_sub, 2, rotation = "none")
print(fa_none, cutoff = 0.1)


# Varimax rotation
fa_vari <- factanal(DOGE_USD_sub, 2, rotation = "varimax")
print(fa_vari, cutoff = 0.1, sort = TRUE)
print(fa_vari, cutoff = 0.1)

# Factor loadings
B <- fa_vari$loadings[,]

# Assessing the factorability of the data

# Bartlett's Test of Sphericity
cortest.bartlett(DOGE_USD_sub)

# KMO
KMO(DOGE_USD_sub)

# Determining the Number of Factors to Extract

# Scree plot
scree(DOGE_USD_sub)

# Parallel Analysis
fa.parallel(DOGE_USD_sub)

# Estimation of factor model
factor_model <- fa(DOGE_USD_sub, nfactors = 3, fm = "ml", max.iter = 100, rotate = "oblimin")

# Display factor diagram
fa.diagram(factor_model)

# Interpretation of the factor loadings
print(factor_model$loadings, cutoff = 0.3, digits = 3)

# what are the variances of the unique risks?
# extract factor loadings and uniquenesses from PCA object
factor_loadings <- pca$loadings[,1]
unique_variances <- pca$uniquenesses

# print variances of unique risks
print(unique_variances)

#OTHER TYPE OF TV MODELS
#Generalised additive models
plot(model, se=TRUE, col="blue", xlab="Date", ylab="Close Price (log)", main="Generalised additive models (GAM)")

# =Kalman filter
install.packages("KFAS")
library(KFAS)
# Define state-space model
SSM <- SSModel(DOGE_USD$Close ~ SSMtrend(1,Q=0.001), H=0.01)

# Apply Kalman filter
kf <- KFS(SSM)

# Extract filtered states and variances
filtered_states <- kf$states
filtered_variances <- kf$var
summary(kf)

















                           
















