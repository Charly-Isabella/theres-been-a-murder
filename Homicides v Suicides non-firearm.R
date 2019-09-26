VARselect(homicides_ts[,c(2,4)],type="none") 
#VARselect indicates that using p=6 lags has the lowest AIC

#Checking that the time series is stationary:
#Visually: the data does not appear to have a trend, suggesting that the data is stationary
#Augmented Dickey-Fuller Unit Root Test. Do not have a trend but does have a non-zero mean, therefore use type="None" to test for stationarity

adf1 <- summary(ur.df(homicides_ts[,"NFH"], type = "none", lags = 6)) 
adf1 #computed p-value is <0.05 therefore we can accept that the series is stationary
adf2 <- summary(ur.df(homicides_ts[,"NFS"], type = "none", lags = 6))
adf2 #computed p-value is <0.05 therefore we can accept that the series is stationary

#Selecting the VAR model based on best number of lags via AIC
var.aic <- VAR(homicides_ts[,c(2,4)], type = "none", lag.max = 6, ic = "AIC")
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
