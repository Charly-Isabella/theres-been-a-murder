# Loading & Cleaning Data -------------------------------------------------
df = data.frame(homicides)

names(df)[names(df)=="V1"] <- "FH"
names(df)[names(df)=="V2"] <- "NFH"
names(df)[names(df)=="V3"] <- "FS"
names(df)[names(df)=="V4"] <- "NFS"
names(df)[names(df)=="V5"] <- "LIH"
names(df)

class(df)

#update to time series
homicides_ts=ts(df,start=c(1986,01,1),end=c(1991,12,31) ,frequency =52)


# Exploring Data ----------------------------------------------------------

na.remove(homicides_ts)
class(homicides_ts)
summary(homicides_ts)
plot(homicides_ts)

#From the plots we can see peaks of Firearm Homicides and Non-firearm homicides correlating with Legal Intervention Homicides
#Suicides seems non-correlated and no obvious pattern.


# VAR Analysis ------------------------------------------------------------
library(vars)
library(fpp2)
library(urca)
library(tseries)


#Keeping it simple; only looking at interplay between FH and LIH [Firearm Homicides and Legal Intervention Homicides]
#Looking at AIC to select number of lags. Use type="none" because there is a non-zero mean and no trend
VARselect(homicides_ts[,c(1,5)],type="none") 
#VARselect indicates that using p=6 lags has the lowest AIC

#Checking that the time series is stationary:
#Visually: the data does not appear to have a trend, suggesting that the data is stationary
#Augmented Dickey-Fuller Unit Root Test. Do not have a trend but does have a non-zero mean, therefore use type="None" to test for stationarity

adf1 <- summary(ur.df(homicides_ts[,"FH"], type = "none", lags = 6)) 
adf1 #computed p-value is <0.05 therefore we can accept that the series is stationary
adf2 <- summary(ur.df(homicides_ts[,"LIH"], type = "none", lags = 6))
adf2 

#Selecting the VAR model based on best number of lags via AIC
var.aic <- VAR(homicides_ts[,c(1,5)], type = "none", lag.max = 6, ic = "AIC")
summary(var.aic)


# Extract coefficients, standard errors etc. from the object
# produced by the VAR function
est_coefs <- coef(var.aic)

# Extract only the coefficients for both dependend variables
# and combine them to a single matrix
est_coefs <- rbind(est_coefs[[1]][, 1], est_coefs[[2]][, 1]) 

# Print the rounded estimates in the console
round(est_coefs, 2)


# Impulse Response --------------------------------------------------------


#Looking at the Impulse Response variable of an increase in FH on LIH
# Calculate the IRF
ir.1 <- irf(var.aic, n.ahead = 10, ortho = TRUE)
plot(ir.1)

#Look at the culmulative effect
ir.2 <- irf(var.aic, n.ahead = 10, ortho = TRUE, cumulative = TRUE)
plot(ir.2)
