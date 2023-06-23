## MSc Finance 

install.packages("as.zoo.data.frame zoo ")
install.packages("date, intersect, setdiff, union")
install.packages("dplyr")
install.packages("first, last")
install.packages("xts")
install.packages("MASS, strucchange, sandwich")
install.packages("lubridate")
install.packages("tidyquant")
install.packages("IRR, NPV, PMT")
install.packages("kurtosis, skewness")

#setup
library(tidyverse)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(quantmod)


library(readr)
SYK <- read_csv("Downloads/SYK.csv")

View(SYK)

## Cleaning Data

# Remove any rows with using na.omit()
SYK <- na.omit(SYK)

# Remove any tied values
SYK <- unique(SYK)

# Remove any rows with using na.omit()
SYK <- na.omit(SYK)

# Remove any non-numeric or missing values
SYK <- na.omit(as.numeric(SYK)
               
# Sort the data by date 
SYK<- setorder(SYK)
               
# Remove missing values
SYK <- na.omit(SYK)
                                
# Print the cleaned data
print(SYK)

summary(SYK)

boxplot(SYK$Close)

# Calculate the variance 
library(vars)
var(SYK)

# TEST THE DATA

# Log return calculation
library(tidyquant)
data.xts = SYK$Close
return = diff (log(data.xts))
return = return [-1]
summary (return)


plot(return, main = "SYK Daily Returns", xlab = "Year", type = "l", ylab = "Log Return")



## Estimates of Variability

# Mean absolute deviation
# Tools for Descriptive Statistics
library (DescTools) 
MeanAD(return)

# Variance
var(return)

# Standard deviation
sd(return)

# Median Absolute Deviation
mad (return)

# Communalities
data(mtcars)

# Perform factor analysis
fit <- factanal(mtcars, factors = 3)

# Extract communalities
communalities <- fit$communalities

# Print communalities
print(communalities)

# Estimates Based on Percentiles
quantile (return)
# values in the 5th and 95th quantiles
quantile (return, probs=c(0.05, 0.95)) 
# generate sequesnce of numbers that will be used as quantiles
tauseq = seq(.1,.95,.1)
quantile (return, tauseq)

# IQRx = Qx(0,75) - Qx(0.25)
IQR (return)


## Tests for normality

#Two sample t-test for the difference in sample means
t.test(SYK$High,SYK$Low,alternative = "two.sided", var.equal=TRUE)
set.seed("119933")
par(mfrow=c(1,2)) # to build one  figures in one row and two columns  
x1 = rlnorm(20,meanlog=1,sdlog=2) 
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
skewness (SYK$Close)
kurtosis(SYK$Close)

# D'AGOSTINO TEST OF SKEWNESS
library (moments)
agostino.test(return, alternative = "two.sided")

# Anscombe-Glynn test of kurtosis

anscombe.test (return, alternative = "two.sided" )
# since p-value is lower than 0.05 i can reject H0.

#Bonett-seier test of kurtosis
bonett.test (return, alternative = "two.sided" )

# since p vallue is lover than 0.05, therefore we reject the null hypothesis, the data exhibits excess Geary's measure of kurtosis relative to the normal distribution.

# Shapiro-Wilk test
shapiro.test (SYK$Close)
# since p value is lower than 0.05 we reject the null hypothesis that the data are not from the normal distribution

# Kolmogorov-Smirnov
library (fBasics)


# extract daily closing prices
SYK_close <- SYK$Close

# One-sample Kolmogorov-Smirnov test
ks.test(SYK_close, "pnorm")

# p-value is lower than 0.05, therefore reject the null hypothesis that the data are from the normal distribution

# JARQUE-BERA TEST
library (fBasics)
jarqueberaTest(SYK$Close)

# since p value is lower than 0.05, we reject the null hypothesis that the data are from the normal distribution.

#Anderson-Darling goodness of fit test
library (ADGofTest)
library(fBasics)
ad.test (SYK$Close, plnorm)

## pearson Correlation TEST
cor.test (SYK$High, SYK$Low, method="pearson",alternative="two.sided",conf.level = 0.95)

#spearman rank correlation test
cor.test(SYK$High, SYK$Low, method="spearman",alternative="two.sided")

#KENDALL’S TAU CORRELATION COEFFICIENT TEST
cor.test(SYK$High, SYK$Low, method="kendal",alternative="two.sided")


#Variance
install.packages("quantmod")
library(quantmod)
symbol <- "SYK"
start_date <- "2017-05-03"
end_date <- "2023-05-03"
cat("Variance of the adjusted opening price:")
print(var)


# Days
SYK <- SYK %>%
group_by(Date %>%
mutate(Daily_Return = (Close - lag(Close))/lag(Close)) %>%
summarise(CSAD = mean(abs(Daily_Return - mean(Daily_Return)))) %>%


# Estimate Herdings
library (xts)
library (zoo)
library(sandwich)
library (lmtest)

# Take logarithm of positive values
log_syk <- log(SYK)
return = Return.calculate(SYK,method = "log")

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
# Herding Result
f = exchange.herd(return) 
head (f) 

CSAD.df = fortify.zoo(f) 
CSAD.df$Rm2 = CSAD.df$Rm^2 
CSAD.df = CSAD.df[-c(1),] 

# reassign my columns as Y and Xs to look better in the regression model
# reassign my columns as Y and Xs to look better in the regression model
y = CSAD.df$CSAD  
x1 = abs (CSAD.df$Rm)
x2 = CSAD.df$Rm2


#Linear model
# build linear regression model on full data
linearMod <- lm(y~x1+x2)  
print(linearMod)


summary(linearMod)

# Calculate herding for up/down days, high/low volatility days
  
# Estimate herding
library(dplyr)
library(xts)
library(zoo)
install.packages(" first, last, filter, lag, intersect, setdiff, setequal, union")
  
# Define herding function
# Calculate rolling standard deviation
SYK_volatility <- rollapply(SYK_returns, width = 52, FUN = sd, fill=NA)

# Calculate herding measures
up_days <- sum(SYK_returns > 0) / length(SYK_returns)
down_days <- sum(SYK_returns < 0) / length(SYK_returns)
high_volatility_days <- sum(SYK_volatility > quantile(SYK_volatility, 0.9), na.rm=TRUE) / length(SYK_volatility)
low_volatility_days <- sum(SYK_volatility < quantile(SYK_volatility, 0.1), na.rm=TRUE) / length(SYK_volatility)

# Print results Calculation of the Up/Down-High & Low Days
cat("Up days: ", up_days, "\n")
cat("Down days: ", down_days, "\n")
cat("High volatility days: ", high_volatility_days, "\n")
cat("Low volatility days: ", low_volatility_days, "\n")


# Aggregate data by week and calculate up/down days- Summarize
SYK_weekly <- SYK %>% 
  group_by(week = week(date)) %>% 
  summarise(up_days = sum(Close > Open), down_days = sum(Close < Open))


herding <- function(data) {
  data %>%
    group_by(Date, UpDown) %>%
    summarise(CSAD = mean(abs(Daily_Return - mean(Daily_Return)))) %>%
    summarise(Herding = 1 - (min(CSAD) / max(CSAD)))
}

print(herding)

library(TTR)


## Testing models


# Perform required analysis
cm10_dif <- diff(SYK$Open)
aaa_dif <- diff(SYK$High)
cm30_dif <- diff(SYK$Low)
ff_dif <- diff(SYK$Close)
aaa_dif <- diff(SYK$`Adj Close`)
adj_close_diff <- diff(SYK$`Adj Close`)
par(mfrow=c(1,1))
pdf("cm10aaa.pdf",width=6,height=5)
plot(cm10_dif,aaa_dif,xlab="change in 10YR T rate", ylab="change in AAA rate")
graphics.off()
options(digits = 3)
summary(lm(aaa_dif ~ cm10_dif))


##Multiple linear regression with interest rates
summary(lm(aaa_dif ~ cm10_dif + cm30_dif + ff_dif))
plot(as.data.frame(cbind(aaa_dif,cm10_dif,cm30_dif,ff_dif)))

## Anova
anova(lm(aaa_dif ~ cm10_dif + cm30_dif + ff_dif))
anova(lm(aaa_dif~ff_dif+cm30_dif+cm10_dif))

## Testing models
fit1 = lm(aaa_dif ~ cm10_dif)
fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
fit3 = lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)
anova(fit1, fit3)
anova(fit2, fit3)

##Weekly interest rates-Model selection by AIC and BIC
library(leaps)
subsets = regsubsets(aaa_dif~., data=as.data.frame(cbind(cm10_dif,cm30_dif,ff_dif)),nbest=1) # calculate 3 indexes: BIC, CP and R2(adj) 
b = summary(subsets)
b

# ploting the indexes
par(mfrow=c(1,3),lab=c(2,5,3),pch=19)
plot(1:3,b$bic,type="b",xlab="number of variables", ylab="BIC",cex=2.5)
plot(1:3,b$cp,type="b",xlab="number of variables",  ylab="Cp",cex=2.5)
plot(1:3,b$adjr2,type="b",xlab="number of variables", ylab="adjusted R2")

# AIC and BIC
# regression with 1,2 and 3 variables
sat.lm1 <- lm(aaa_dif ~ cm10_dif) # estimate the regression with 1 varible
sat.lm2 <- lm(aaa_dif~cm10_dif+cm30_dif) # estimate the regression with 2 varibles
sat.lm3 <- lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)# estimate the regression with 3 varibles

# AIC and BIC for one variable regression
sat.n <- length(aaa_dif) # number of observations
sat.sse1 <- sum(resid(sat.lm1) ^2) # the sum of squared residuals
AIC.selfmade <- sat.n + sat.n*log(2*pi) + sat.n * log(sat.sse1 / sat.n) + 2 * (2+1)
AIC.selfmade
#AIC functin
AIC(sat.lm1, k=2)

BIC.selfmade <- sat.n + sat.n * log(2*pi) + sat.n*log(sat.sse1/sat.n) + log(sat.n) * (2+1)
BIC.selfmade
#BIC functions
AIC (sat.lm1, k=log(sat.n))
BIC (sat.lm1)


#AIC and BIC for 2 variable regression
AIC(sat.lm2, k=2)
BIC (sat.lm2)

#AIC and BIC for  3 variable model
AIC(sat.lm3, k=2)
BIC (sat.lm3)


#Checking Model Assumptions: Nonlinearity======================================
#generate data
n = 80
set.seed("2020")
e = matrix(runif(12*n),nrow=n) %*% rep(1,12)
e = abs(e)^4
e= e/mean(e) 
x1 = runif(n)
x1 = sort(x1) 
x2 = rbeta(n,6,.5)

y =( 8*x2 + x1 + 5*x1^3) + ( 4* x2 + x1 + 7*x1^3) * e 
par(mfrow=c(1,2))
plot(x1,y,xlab=expression(x[1]))
plot(x2,y,xlab=expression(x[2]))
fit = lm(y~x1+x2)
rstudent = rstudent(fit)
par(mfrow=c(1,2))
qqnorm(rstudent,datax=T,main="Normal QQ Plot")
hist(rstudent,12)

# Set parameters
n <- 252    # number of days
r <- 0.05   # risk-free interest rate
vol <- 0.1  # volatility
dt <- 1/n   # time step

# Simulate bond prices
price <- rep(100, n+1)  # initial price is 100
for (i in 2:(n+1)) {
  price[i] <- price[i-1] * exp((r - vol^2/2)*dt + vol*sqrt(dt)*rnorm(1))
}

# Plot simulated bond prices
plot(price, type = "l", xlab = "Days", ylab = "Price",
     main = "Simulated Bond Prices")

install.packages("strucchange")

par(mfrow=c(1,1)) # set or query graphical parameters - here one graph; for details help (par)
plot(SYK,price,pch="*",cex = 2) # building the the graph using observations

grid = seq(0, 20, length=201)
price_grid = 1000*exp(-0.0585*grid)
lines(grid,price_grid, lwd = 2, col = "red")
legend("topright",c("price","predicted price"),pch=c("*",NA), col = c("black","red"), lty=c(NA,1),pt.cex=c(2,1))


## Troubleshooting ========
# Leverages' measurements
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


##Estimating default probabilities

# Historical default rates by credit rating
rating_default_rates <- data.frame(
  rating = c("AAA", "AA", "A", "BBB", "BB", "B", "CCC"),
  default_rate = c(0.07, 0.26, 0.51, 2.06, 6.07, 17.01, 39.86)
)

# Function to estimate default probabilities based on credit rating
estimate_default_prob <- function(rating) {
  default_rate <- rating_default_rates$default_rate[rating_default_rates$rating == rating]
  if (length(default_rate) == 0) {
    warning("Rating not found: ", rating)
    default_prob <- NA
  } else {
    default_prob <- default_rate / 100
  }
  return(default_prob)
}
estimate_default_prob("AA")
estimate_default_prob("AAA")
estimate_default_prob("BBB")
estimate_default_prob("BB")
estimate_default_prob("B")
estimate_default_prob("CCC")

## transformation

plot(`Adj Close` ~ Date, data = SYK, col = "grey", pch = 45, cex = 2, 
     main = "SYK Stock Prices Over Time") # Date on the horizontal 

plot(Date ~ `Adj Close`, data = SYK, col = "grey", pch = 20, cex = 1.5, 
     main = "SYK Stock Prices Over Time") # Date on the vertical


install.packages("as.Date, as.Date.numeric, xts,zoo")


SYK <- getSymbols("SYK", src = "yahoo", from = "2017-05-03", to = "2023-05-03", auto.assign = FALSE)

SYK_df <- data.frame(date = index(SYK), SYK$SYK.Close)

names(SYK_df) <- c("date", "close_price")

SYK_df$log_return <- log(lag(SYK_df$close_price)) - log(SYK_df$close_price)

plot(close_price ~ date, data = SYK_df, col = "grey", pch = 20, cex = 1.5, main = "SYK Close Price Over Time")
abline(lm(close_price ~ date, data = SYK_df), col = "darkorange", lwd = 2)

par(mfrow = c(1, 2))

plot(fitted(lm(close_price ~ date, data = SYK_df)), resid(lm(close_price ~ date, data = SYK_df)), col = "grey", pch = 20, xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(lm(close_price ~ date, data = SYK_df)), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(lm(close_price ~ date, data = SYK_df)), col = "dodgerblue", lwd = 2)

plot(log(close_price) ~ date, data = SYK_df, col = "grey", pch = 20, cex = 1.5, main = "SYK Close Price Over Time")
abline(lm(log(close_price) ~ date, data = SYK_df), col = "darkorange", lwd = 2)

plot(close_price ~ date, data = SYK_df, col = "grey", pch = 20, cex = 1.5, main = "SYK Close Price Over Time")

par(mfrow = c(1, 2))

plot(fitted(lm(log(close_price) ~ date, data = SYK_df)), resid(lm(log(close_price) ~ date, data = SYK_df)), col = "grey", pch = 20, xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(lm(log(close_price) ~ date, data = SYK_df)), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(lm(log(close_price) ~ date, data = SYK_df)), col = "dodgerblue", lwd = 2)

# RMSE
# Calculate RMSE for the linear model
library(quantmod)
sqrt(mean(resid(syk_fit) ^ 2))

# Convert SYK data to a data frame
syk_df <- data.frame(Date = index(SYK), coredata(SYK))

# Rename columns for convenience
names(syk_df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

# Fit a linear regression model
syk_fit <- lm(Close ~ Open + High + Low, data = syk_df)

# Get summary of the regression model
summary(syk_fit)

# Get residuals of the regression model
residuals <- resid(syk_fit)

# Calculate RMSE
rmse <- sqrt(mean(residuals ^ 2))
rmse

#studentized Breusch-Pagan test
library(lmtest)
bptest(savings_model)

#Shapiro-Wilk normality test
shapiro.test(resid(savings_model))


# Box-Cox transformation
library(MASS)
library(faraway)
savings # show the data

savings_model = lm(sr ~ ., data = savings) # estimate a general regression, sr is y, all the rest variables are Xs
boxcox(savings_model, plotit = TRUE) # determine the value of lambda
boxcox(savings_model, plotit = TRUE, lambda = seq(0.5, 1.5, by = 0.1)) # close up look at the best selected values from the previous steps.

plot(fitted(savings_model), resid(savings_model), col = "dodgerblue", pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)



## Box-cox transformation - the second dataset
gala_model = lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
plot(fitted(gala_model), resid(gala_model), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

boxcox(gala_model, lambda = seq(-0.25, 0.75, by = 0.05), plotit = TRUE)

# transformation
gala_model_cox = lm((((Species ^ 0.3) - 1) / 0.3) ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
plot(fitted(gala_model_cox), resid(gala_model_cox), col = "dodgerblue", pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

boxcox(syk_fit)

## use trafo
install.packages("Ecfun, sign, Orange, SP500")
library (trafo)
library(Ecdat) # for data
data(University) # dataset University

linMod <- lm(nassets ~ stfees, data = University)
summary (linMod)

plot(fitted(linMod), resid(linMod), col = "dodgerblue",     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

plot(nassets ~ stfees, data = University, col = "grey", pch = 20, cex = 1.5,
     main = "how study fees (stfees) raise the universities net assets (nassets)")
abline(linMod, col = "darkorange", lwd = 2)

assumptions(linMod)

linMod_trafo <- trafo_lm(linMod)
diagnostics(linMod_trafo)
plot (linMod_trafo)

linMod_trafo2 <- trafo_lm(object = linMod, trafo = "logshiftopt", method = "skew")

### Binary Regression ===================================
install.packages("carData")
library("AER")
library("faraway")
library(MASS)

# Load the data
data("CreditCard")
CreditCard_clean <- CreditCard[CreditCard$age > 18,] 

# Fit the logistic regression model
logit_model <- glm(card ~ log(reports+1) + income + log(share) + age + owner + dependents + months,
                   family = binomial(link = "logit"), data = CreditCard_clean)

# View the model summary
summary(logit_model)

# Perform variable selection using AIC
step_model <- stepAIC(logit_model, direction = "both")

# View the final selected model
summary(step_model)


summary(fit1)
stepAIC(fit1) # Choose a model by AIC in a Stepwise Algorithm

# RMSE
sqrt(mean(resid(fit1) ^ 2))

# Load necessary packages
library(tidyverse)
library(AER)
library(markovswitch)
install.packages("markovswitch")

# Linear regression
x <- c(1,2,3,4,5)
y <- c(2,4,6,8,10)
# perform linear regression
model <- lm(y ~ x)
# view the summary of the model
summary(model)

# Install and load required packages
install.packages(c("tvReg", "mATR"))
library(tvReg)

# Markow regime-switching model
library (MSwM)
install.packages("parallel, MSwM")

nstates <- 2 # a number of states
msEuro = msmFit(linearMod, k = nstates, sw = rep(TRUE, 4)) # estimation; linearMod is an object from a linear estimation
summary(msEuro) 
plotProb(msEuro ,which=1)

#Quantile regression
install.packages("SparseM, backsolve")
library (quantreg)
taus<-seq(from = .1, to = .9, by = .1) 
coef0 <- rq( y ~ x1+x2, tau=taus)
summary (coef0)

# Bayesian models
install.packages("ar, brms, Rcpp")
library (brms)
hourly = cbind(y, x1, x2)
model = brm(formula = y ~ x1+x2, 
            data    = hourly,
            seed    = 123)
summary(model)


# TV-REGRESSION

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


f = exchange.herd(return) # calling the function "exchange.herd" that calculates CSAD and Rm
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


# tv-regression's formula
library (tvReg)
tvlm.fit = tvLM(y~x1+x2, bw = NULL  ) #bw=0.149 
head (tvlm.fit$coefficients)
plot(tvlm.fit$coefficients[,1], type="l")
plot(tvlm.fit$coefficients[,2], type="l")
plot(tvlm.fit$coefficients[,3], type="l")


# Calculation of the up/down days and high/low volatility days; 
library(dplyr)
library(quantmod)
# Days
SYK <- SYK %>%
  group_by(Date %>%
mutate(Daily_Return = (Close - lag(Close))/lag(Close)) %>%
summarise(CSAD = mean(abs(Daily_Return - mean(Daily_Return)))) %>%
# Set date column as the index
SYK <- xts(SYK[,-1], order.by = SYK$Date)
           
# Calculate daily returns
SYK$Daily_Return <- dailyReturn(SYK$Close)
           
# Calculate the average absolute deviation from the mean return for each day
SYK_df <- SYK_df %>%
group_by(Date) %>%
summarise(CSAD = mean(abs(Daily_Return - mean(Daily_Return))))
# Convert to data.frame
SYK_df <- data.frame(Date = index(SYK), coredata(SYK))
           
 # Fill any missing values with the latest observation carried forward
SYK <- na.locf(SYK)
           
# Calculate up/down days
SYK_up_days <- ifelse(ROC(Cl(SYK$Close)) > 0, 1, 0)
SYK_down_days <- ifelse(ROC(Cl(SYK$Close)) < 0, 1, 0)
           
# Calculation of high/low volatility days
SYK_high_vol_days <- ifelse(abs(ROC(Cl(SYK$Close))) > mean(abs(ROC(Cl(SYK$Close)))), 1, 0)
SYK_low_vol_days <- ifelse(abs(ROC(Cl(SYK$Close))) < mean(abs(ROC(Cl(SYK$Close)))), 1, 0)
           



































