#### DATA WRANGALING LIBRARY ########################################################################
if(!require(caret)) install.packages("caret")
if(!require(rpart.plot)) install.packages("rpar.plot")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(fields)) install.packages("fields")
if(!require(tidy)) install.packages("tidy")
if(!require(VIM)) install.packages("VIM")
if(!require(aTSA)) install.packages("aTSA")

#### DATA WRANGALING ########################################################################
setwd("C:/Users/home/Desktop/Data_USA")

data <- read.csv("Data_us.csv", strip.white =  T, stringsAsFactors = T, sep = ";", header = F, na.strings=c("","NA"))
pop <- read.csv("Total_Population.csv", strip.white =  T, stringsAsFactors = T, sep = ",")
                
head(data)
head(pop)

# remove the first 2 rows
data <- data[-c(1,2),]; head(data)

#change the names
colnames(data) <- c("class", "year", "share", "AVG")

#creating the population dataset 
US_pop <- filter(pop, Location == "United States of America")
US_pop <- filter(US_pop, Time >= "1950" & Time <= "2018")
US_pop <- US_pop[,-c(1:4, 6:8, 10)]
colnames(US_pop) <- c("year", "total pop 10^3")
US_pop$year <- as.factor(US_pop$year)

# creating the class 
top1 <- filter(data, class == "p99p100")
bottom50 <- filter(data, class == "p0p50" )
middle40 <- filter(data, class == "p50p90" )

# aerage gdp
AVG <- filter(data, class == "pall" )
class(US_pop$year)

# only the year and data are needed
top1 <- top1[-c(1,4)]
bottom50 <- bottom50[-c(1,4)]
middle40 <- middle40[-c(1,4)]
AVG <- AVG[-c(1,3)]

#Creating the dataset
income <- left_join(AVG, top1, by = "year")
income <- left_join(income, middle40, by = "year")
income <- left_join(income, bottom50, by = "year")
income <-left_join(income, US_pop, by = "year")
colnames(income) <- c("year", "AVG", "share_top_1", "share_middle_40", "share_bottom_50", "total_pop")
income <- mutate_all(income, function(x) as.numeric(as.character(x)))

#somme missing values 
income

#k-Nearest Neighbour Imputation 
  # Fill the NA's by clustering 
income <- kNN(income, imp_var = FALSE)
income 

#finding out how many people we have in the classes + an estimate of the GDP per year
incomeTotal <- income %>% mutate("total_pop" = total_pop *1000,
                            "tot1" = total_pop * 0.01,
                            "tot40" = total_pop * 0.40,
                            "tot50" = total_pop * 0.50,
                            "app_GDP" = total_pop * as.numeric(AVG))
head(incomeTotal)

#calculating the income by year base on the share and population in the classes 
incomeIndividual <- incomeTotal %>% mutate("top1" = (as.numeric(share_top_1) * app_GDP) / tot1,
                            "middle40" = (as.numeric(share_middle_40) * app_GDP) / tot40,
                            "bottom50" = (as.numeric(share_bottom_50) * app_GDP) / tot50)
head(incomeIndividual)

# keep only the years, top1% middle40% and bottom50%
incomeIndividual <- incomeIndividual[,c(1,11:13)]
head(incomeIndividual)

rm(AVG, bottom50, data, income, incomeTotal, middle40, pop, top1, US_pop)
#### TIME SERIES LIBRARY ########################################################################
if(!require(tidymodels)) install.packages("tidymodels")
if(!require(data.table)) install.packages("data.table")
if(!require(tidyposterior)) install.packages("tidyposterior")
if(!require(tsibble)) install.packages("tsibble")
if(!require(fable)) install.packages("fable")
if(!require(ggfortify)) install.packages("ggfortify")
if(!require(forecast)) install.packages("forecast")
if(!require(tseries)) install.packages("tseries")
if(!require(chron)) install.packages("chron")
if(!require(lubridate)) install.packages("lubridate")
if(!require(directlabels)) install.packages("directlabels")
if(!require(lmtest)) install.packages("lmtest")
if(!require(TTR)) install.packages("TTR")
if(!require(MTS)) install.packages("MTS")
if(!require(vars)) install.packages("vars")
if(!require(fUnitRoots)) install.packages("fUnitRoots")
if(!require(lattice)) install.packages("lattice")
if(!require(grid)) install.packages("grid")
#### CAUSALITY FORECASTS #########################################################################

#Forecasting 
#Creating a time series data 
mts <- ts(incomeIndividual[,2:3], frequency = 1, start = 1950, end = 2010)

# Standard exploratory tools
plot(mts)

theme_set(theme_bw())
autoplot(mts) + 
  ggtitle("Time Series Plot") + 
  theme(plot.title = element_text(hjust = 0.5))

head(mts)
class(mts)

# Testing for stationarity

#Augmented Dickey-Fuller test
apply(mts, 2, adf.test, k = 8) 

# Diff the mts
dmts <- diffM(mts)

# Dickey-Fuller test on the diffrence
apply(dmts, 2, adf.test)

# VAR modeling 
plot.ts(dmts)

autoplot(ts(dmts,
            start = 1950)) +
ggtitle("Time Series of the stationary income")

#Lag order identification 
VARselect(dmts, 
          type = "none", # since it was made stationary suing diff 
          lag.max = 8)

var.a <- vars::VAR(dmts,
                   lag.max = 8,
                   ic = "FPE", #AIC HQ SC FPE
                   type = "none")
summary(var.a)

# Residual Diagnostics 
serial.test(var.a)

#selecting the vairables 
causality(var.a, 
          cause = c("middle40"))

#Forecasting VAR models 
fcast <- predict(var.a, n.ahead = 8)
plot(fcast)


# Forecasting the income 
top1 <- fcast$fcst[1]; top1
middle40 <- fcast$fcst[2]; middle40
bottom50 <- fcast$fcst[3]; bottom50

#Extracting the forecast column 
x1 <- top1$top1[,1]; x1
x2 <- middle40$middle40[,1]; x2
x3 <- bottom50$bottom50[,1]; x3
tail(mts)

#Inverting the differencing 
x1 <- cumsum(x1) + 1090439
plot.ts(x1)

x2 <- cumsum(x2) + 56290.90
plot.ts(x2)

x3 <- cumsum(x3) + 14469.71
plot.ts(x3)

# Adding data and forecast to one ts 
top1 <- ts(c(mts[,1], x1),
          start = 1950, 
          frequency = 1)
plot(top1)

middle40 <- ts(c(mts[,2], x2),
          start = 1950,
          frequency = 1)
plot(middle40)

bottom50 <- ts(c(mts[,3], x3),
              start = 1950,
              frequency = 1)
plot(bottom50)

class(top1)
plot.ts(top1[50:69])
plot.ts(middle40[50:69])
plot.ts(bottom50[50:69])

incomeF <- data.frame("year" = 1950:2018,
                         "top1" = top1,
                         "middle40" = middle40,
                         "bottom50" = bottom50)

# Plot forecast of the yearly income
ggplot() +
  geom_line(data = incomeIndividual[1:61,], 
            aes(y = get("top1"),
                x = seq(1, 61)), color = "black") +
  geom_line(data = incomeF[61:69,], 
            aes(y = get("top1"),
                x = seq(61, 69)), color = "red") +
  geom_line(data = incomeIndividual[61:69,], 
            aes(y = get("top1"),
                x = seq(61, 69)), color = "green") +
  geom_line(data = incomeIndividual[1:61,], 
            aes(y = get("middle40"),
                x = seq(1, 61)), color = "black") +
  geom_line(data = incomeF[61:69,], 
            aes(y = get("middle40"),
                x = seq(61, 69)), color = "red") +
  geom_line(data = incomeIndividual[61:69,], 
            aes(y = get("middle40"),
                x = seq(61, 69)), color = "green") +
  geom_line(data = incomeIndividual[1:61,], 
            aes(y = get("bottom50"),
                x = seq(1, 61)), color = "black") +
  geom_line(data = incomeF[61:69,], 
            aes(y = get("bottom50"),
                x = seq(61, 69)), color = "red") +
  geom_line(data = incomeIndividual[61:69,], 
            aes(y = get("bottom50"),
                x = seq(61, 69)), color = "green") +
  ggtitle("Plot of forecast of the income inequality") +
  xlab("Time") + ylab("Value")
  
# Plot forecast of the yearly income of the 90%
ggplot() +
  geom_line(data = incomeIndividual[1:61,], 
            aes(y = get("middle40"),
                x = seq(1, 61)), color = "black") +
  geom_line(data = incomeF[61:69,], 
            aes(y = get("middle40"),
                x = seq(61, 69)), color = "red") +
  geom_line(data = incomeIndividual[61:69,], 
            aes(y = get("middle40"),
                x = seq(61, 69)), color = "green") +
  geom_line(data = incomeIndividual[1:61,], 
            aes(y = get("bottom50"),
                x = seq(1, 61)), color = "black") +
  geom_line(data = incomeF[61:69,], 
            aes(y = get("bottom50"),
                x = seq(61, 69)), color = "red") +
  geom_line(data = incomeIndividual[61:69,], 
            aes(y = get("bottom50"),
                x = seq(61, 69)), color = "green") +
  ggtitle("Plot of forecast of the income inequality in the 90%") +
  xlab("Time") + ylab("Value")

rm( var.a, bottom50, middle40, top1, dmts, fcast)

#### MEAN, NAIVE AND DRIFT FORECASTS ########################################################################
#  FORECAST ERRORS # SCALE-DEPENDENT ERRORS # SCALE ERRORS

# Creating time series training sets 
in1 <- window(ts(incomeIndividual[,2], start = 1950),start = 1950, end = 2010); plot(in1)
in2 <- window(ts(incomeIndividual[,3], start = 1950),start = 1950, end = 2010); plot(in2)
in3 <- window(ts(incomeIndividual[,4], start = 1950),start = 1950, end = 2010); plot(in3)

# apply to the top 1%
infit1 <- meanf(in1, h = 8)
infit2 <- rwf(in1, h = 8)
infit3 <- rwf(in1, drift = T, h = 8)

# plot the forecasts for the top 1%
autoplot(window(ts(incomeIndividual[,2], start = 1950),start = 1950)) +
  autolayer(infit1, series = "mean", PI = FALSE) +
  autolayer(infit2, series = "Naive", PI = FALSE) +
  autolayer(infit3, series = "drift", PI = FALSE) +
  xlab("year") + ylab("income") +
  ggtitle("forecasts comparasion top 1%") + 
  guides(color = guide_legend(title = "forecast"))

# test the accuracy 
int1.2 <- window(ts(incomeIndividual[,2], start = 2010),start = 2010)
accuracy(infit1, int1.2)
accuracy(infit2, int1.2)
accuracy(infit3, int1.2)

# apply to the middle 40%
infit1 <- meanf(in2, h = 8)
infit2 <- rwf(in2, h = 8)
infit3 <- rwf(in2, drift = T, h = 8)

# plot the forecasts for the middle 40%%
autoplot(window(ts(incomeIndividual[,3], start = 1950),start = 1950)) +
  autolayer(infit1, series = "mean", PI = FALSE) +
  autolayer(infit2, series = "Naive", PI = FALSE) +
  autolayer(infit3, series = "drift", PI = FALSE) +
  xlab("year") + ylab("income") +
  ggtitle("forecasts comparasion middle 40%") + 
  guides(color = guide_legend(title = "forecast"))

# test the accuracy 
int1.2 <- window(ts(incomeIndividual[,3], start = 2010),start = 2010)
accuracy(infit1, int1.2)
accuracy(infit2, int1.2)
accuracy(infit3, int1.2)

# apply to the bottom 50%
infit1 <- meanf(in3, h = 8)
infit2 <- rwf(in3, h = 8)
infit3 <- rwf(in3, drift = T, h = 8)

# plot the forecasts for the bottom 50%
autoplot(window(ts(incomeIndividual[,4], start = 1950),start = 1950)) +
  autolayer(infit1, series = "mean", PI = FALSE) +
  autolayer(infit2, series = "Naive", PI = FALSE) +
  autolayer(infit3, series = "drift", PI = FALSE) +
  xlab("year") + ylab("income") +
  ggtitle("forecasts comparasion bottom 50%") + 
  guides(color = guide_legend(title = "forecast"))

# test the accuracy 
int1.2 <- window(ts(incomeIndividual[,4], start = 2010),start = 2010)
accuracy(infit1, int1.2)
accuracy(infit2, int1.2)
accuracy(infit3, int1.2)

rm(in1, in2, in3, int1.2, infit1, infit2, infit3, x1, x2, x3)
#TIME SERIES CROSS-VALIDATION 
# compare the time series corss-validation RMSE with the residual RMSE

e <- tsCV(in1, rwf, drift = TRUE, h = 1); e
sqrt(mean(e^2, na.rm = TRUE))

#As expected, the RMSE from the residuals is smaller
sqrt(mean(residuals(rwf(in1, drift = TRUE)) ^2, na.rm = TRUE))

e <- tsCV(in1, rwf, drift = TRUE, h = 8); e

# compute the MSE values and remove missing values 
mse <- colMeans(e^2, na.rm = T); mse

# Plot the MSE values against the forecast Horinzon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

# evaluate the forecasting performance of 1 to 8 step ahead
# drift forecasts using MSE as the forecast error measure.
# the plot shows that the forecast error increases 
# as the forecast horizon increases, as expected 

#### ARIMA MODELING ##################################################

#find the right parameters to be used in the ARIMA
#ACF Plots

# ACF plots display correlation between a series and its lags
  #ACF chart is very slow, which means that the income is not stationary

acf(mts)
  # The autocorrelations are significant for a large number of lags--but perhaps
  # the autocorrelations at lags 2 and above are merely due to the propagation of 
  # the autocorrelation at lag 1. This is confirmed by the PACF plot:
lapply(mts, pacf)
  
acf(log(mts))

#ACF chart is very slow, which means that the income is not stationary

# Let's see how ACF and PACF curve come out after regressing on the difference.
acf(diff(log(mts)))
pacf(diff(log(mts)))

 # ACF plots display correlation between a series and its lags that explained by previous lags.
lapply(mts, function(x){
  pacf(diff(x))
})

#Build the ARIMA model
  #select a model automaticaly 
fit1 <- auto.arima(mts[,1], seasonal = F); fit1
fit2 <- auto.arima(mts[,2], seasonal = F); fit2
fit3 <- auto.arima(mts[,3], seasonal = F); fit3

for1 <- forecast(fit1, h = 8); plot(for1)
for2 <- forecast(fit2, h = 8); plot(for2)
for3 <- forecast(fit3, h = 8); plot(for3)

# adding the forecast to the data
df <- incomeIndividual[1:61,]
incomeF <- data.frame(year = 2011:2018,
                      top1 = for1$mean,
                      middle40 = for2$mean,
                      bottom50 = for3$mean)
incomeF <- rbind(df, incomeF)

# Plot the graph 
ggplot() +
  geom_line(data = incomeIndividual[1:61,], 
            aes(y = get("top1"),
                x = seq(1, 61)), color = "black") +
  geom_line(data = incomeF[61:69,], 
            aes(y = get("top1"),
                x = seq(61, 69)), color = "red") +
  geom_line(data = incomeIndividual[61:69,], 
            aes(y = get("top1"),
                x = seq(61, 69)), color = "green") +
  geom_line(data = incomeIndividual[1:61,], 
            aes(y = get("middle40"),
                x = seq(1, 61)), color = "black") +
  geom_line(data = incomeF[61:69,], 
            aes(y = get("middle40"),
                x = seq(61, 69)), color = "red") +
  geom_line(data = incomeIndividual[61:69,], 
            aes(y = get("middle40"),
                x = seq(61, 69)), color = "green") +
  geom_line(data = incomeIndividual[1:61,], 
            aes(y = get("bottom50"),
                x = seq(1, 61)), color = "black") +
  geom_line(data = incomeF[61:69,], 
            aes(y = get("bottom50"),
                x = seq(61, 69)), color = "red") +
  geom_line(data = incomeIndividual[61:69,], 
            aes(y = get("bottom50"),
                x = seq(61, 69)), color = "green") +
  ggtitle("Plot ARIMA forecast of the income inequality") +
  xlab("Time") + ylab("Value")

#plot the income inequality of the 90%
ggplot() +
  geom_line(data = incomeIndividual[1:61,], 
            aes(y = get("middle40"),
                x = seq(1, 61)), color = "black") +
  geom_line(data = incomeF[61:69,], 
            aes(y = get("middle40"),
                x = seq(61, 69)), color = "red") +
  geom_line(data = incomeIndividual[61:69,], 
            aes(y = get("middle40"),
                x = seq(61, 69)), color = "green") +
  geom_line(data = incomeIndividual[1:61,], 
            aes(y = get("bottom50"),
                x = seq(1, 61)), color = "black") +
  geom_line(data = incomeF[61:69,], 
            aes(y = get("bottom50"),
                x = seq(61, 69)), color = "red") +
  geom_line(data = incomeIndividual[61:69,], 
            aes(y = get("bottom50"),
                x = seq(61, 69)), color = "green") +
  ggtitle("Plot ARIMA forecast of the income inequality in the 90%") +
  xlab("Time") + ylab("Value")

# test the accuracy 
int1.2 <- window(ts(incomeIndividual[,4], start = 2013),start = 2013)
accuracy(for1, int1.2)
accuracy(for2, int1.2)
accuracy(for3, int1.2)

    #personal not: YYYYYYYYYYYYYYYYYAAAAAAAAAAAAAAAAAAAASSSSSSSSSSSS
