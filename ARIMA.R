# PACKAGES ----------------------------------------------------------------

pacotes <- c("plotly","tidyverse","ggrepel","sjPlot","reshape2","FactoMineR",
             "cabootcrs","knitr","kableExtra","gifski","gganimate","factoextra",
             "plot3D", "readr", "dslabs", "ggplot2", "readxl", "janitor", "dplyr",
             "data.table", "dplyr", "formattable", "tidyr", "gt", "glue", 
             "devtools", "gridExtra", "remotes", "htmlTable", "xtable", "expss",
             "kableExtra", "lubridate", "stringr", "colorRamps", "grDevices", 
             "latticeExtra", "randomcoloR", "scales", "lattice", "rattle", "jtools",
             "forecast","tidyverse","fpp2","TSA","tseries")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



# LOADING FILES -----------------------------------------------------------

#Listing the files of the project
list.files()

#Loading the Dataset
dataset <- read_excel("WorldEnergyBalancesHighlights2021.xlsx", 
                      sheet = "TimeSeries_1971-2020", 
                      skip = 1) %>% select(-c(4, 5, 6))


energy_prod <- dataset %>% filter(Country == "World" & Product == "Total" &
                                    Flow == "Production (PJ)") %>% 
  select(-c(1, 2, 3))

energy_cons <- dataset %>% filter(Country == "World" & Product == "Total" &
                                    Flow == "Total final consumption (PJ)") %>% 
  select(-c(1, 2, 3))

# PRE PROCESSING ----------------------------------------------------------

energy_cons <- t(energy_cons) # Transposing the data frame

energy_cons <- data.frame(energy_cons) # Converting to a data frame

energy_cons <- tibble::rownames_to_column(energy_cons, "VALUE")

colnames(energy_cons) <- c("Year", "Consumption") # Adding heads

energy_cons$Consumption <- as.numeric(energy_cons$Consumption)

energy_cons$Year <- as.numeric(energy_cons$Year)


#energy_cons$Year <- as.integer(energy_cons$Year)

# Checking the variable types and the presence of NA´s

ener_chart <- energy_cons %>% ggplot(aes(x = Year, y = Consumption)) +
  geom_line() + 
  scale_colour_manual("Legend", values = "blue") +
  labs(title = "ENERGY CONSUMPTION ALONG THE YEARS",
       y = "Consumption [PJ]") +
  theme_bw()

ener_chart # Chart

# The time plot shows a trend suggesting a Movel Average (MA) model. There is
# a sudden drop in 2009 due the global recession, the first decline since 1982.
# The model is not seasonal

summary(energy_cons)

energy_cons <- na.omit(energy_cons) # Removing NA´s

ener_cons <- energy_cons # Creating a step database to manipulate the tables

energy_cons <- energy_cons %>% remove_rownames %>% column_to_rownames(var="Year")


# DEFINING HOLT WINTERS PARAMETERS ----------------------------------------

## Separating a train and test dataset to define Holt Winters 
## parameters (alpha and beta)

train <- energy_cons[1:44,]

test <- energy_cons[45:49,]

energy_train <- ts(train, start = 1971, end = 2014, frequency = 1)

energy_test <- ts(test, start = 2015, end = 2019, frequency = 1)

beta = seq(.01, 0.99, by = .01) # Define the sequence of trained values

accuracy = c()

# Finding beta parameter

for(i in seq_along(beta)) {
  fit = holt(energy_train,
             beta = beta[i])
  accuracy[i] = accuracy(fit, 
                         energy_test)[2,5]
}

length(accuracy)

length(beta)

# Convert to a dataframe and identity the best beta

beta.fit = cbind(beta, accuracy)
beta.fit<-as.data.frame(beta.fit)

beta.min = subset(beta.fit, 
                  accuracy == min(accuracy))

# Finding alpha parameter


alpha = seq(.01, 0.9, by = .01) # Define a sequência de valores treinados
accuracy = c()

for(i in seq_along(alpha)) {
  fit = holt(energy_train,
             alpha = alpha[i])
  accuracy[i] = accuracy(fit, 
                         energy_test)[2,5]
}

# Convert to a dataframe and identity the best alpha

alpha.fit = cbind(alpha, accuracy)


alpha.min = subset(alpha.fit, 
                   accuracy == min(accuracy))


holt_model_calibrated = holt(energy_train, alpha =alpha.min[1,1], 
                             beta = beta.min[1,1])
plot(holt_model_calibrated)


forecast_calibrated = predict(holt_model_calibrated, h = 10)

# Function to calc the mean

calc_mape<-function(x,y){
  
  dataf = cbind(x, y)
  dataf = as.data.frame(dataf)
  dataf$difabs = abs(dataf[,1] - dataf[,2])
  dataf$mape_perc = dataf$difabs/dataf[,1]*100
  dataf$mape_medio = mean(dataf$mape_perc)
  
  return(dataf)
  
}


calc_mape(energy_test, forecast_calibrated$mean)


# Saving the alpha and beta in variables

alpha.min <- data.frame(alpha.min)

alpha = alpha.min[1,1]

beta = beta.min[1,1]

# Converting the data to time series

energy_cons <- ts(energy_cons, frequency = 1, start = 1971)

# Checking the Autocorrelation (ACF, PACF)

# ACF: Relation existing between the observations throughout the time.
# Direct and Indirect Correlation

# PACF: It gives a partial correlation of a time series stationary 
# with its own lagged values regressing the time series values in more short lags.
# Only Direct Correlation

ener2 <- energy_cons %>% diff() %>% ggtsdisplay(main="")

# OBS: The ACF plot of the residuals shows that all auto correlations are within 
# the threshold limits, indicating that the residuals are behaving like white 
# noise

# ARIMA - FORECASTS -------------------------------------------------------

# Using ARIMA to prediction

mod.arima <- auto.arima(energy_cons, seasonal = FALSE)

summary(mod.arima)

# The ARIMA model is (0, 1, 0) having only Moving Average (MA)

# Evaluating the ACF, PACF and Ljung-Box test (much far from 0), one can 
# conclude that the residuals are independent
# There is no correlation between the values

arima_forecast <- mod.arima %>% forecast(h = 11)

arima_forecast %>% autoplot(ylab = "Energy Concumption [PJ]")

table_arima <- ener_cons %>%  mutate("Fitted" = arima_forecast$fitted)


pred_values_arima <- data.frame(seq(2020, 2030, 1)) %>% 
  mutate("Mean" = arima_forecast$mean) %>% mutate("Upper" = arima_forecast$upper[,2]) %>% 
  mutate("Lower" = arima_forecast$lower[,2])

colnames(pred_values_arima)[1] <- "Year"

# Visualizing the predicted values
pred_values_arima %>% kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, font_size = 12)

# pred_values_arima <- pred_values_arima %>% remove_rownames %>% column_to_rownames(var="Year")

chart_arima <- merge(table_arima, pred_values_arima, by = "Year", all = TRUE)

colors <- c("Consumption" = "black", "Fitted" = "blue", 
            "Mean" = "red", "Upper" = "gray", "Lower" = "gray")

chart_arima %>% ggplot(aes(x = Year)) +
  geom_line(aes(y = Consumption, color = "Consumption"), lwd=0.8) +
  geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", lwd=0.8) +
  geom_line(aes(y = Mean, color = "Mean"), lwd=0.8) +
  geom_line(aes(y = Upper, color = "Upper"), lwd=0.8) +
  geom_line(aes(y = Lower, color = "Lower"), lwd=0.8) +
  labs(title = "ENERGY FORECAST - ARIMA MODEL",
       y = "Consumption [PJ]", color = "Legend") +
  scale_color_manual(values = colors) +
  theme_bw()

# OBS: The ACF plot of the residuals shows that all auto correlations are within 
# the threshold limits, indicating that the residuals are behaving like white 
# noise. The Ljung-Box test returns a large p-value which enhances the suggested
# behavior


# Using Holt Winters library ----------------------------------------------

# alpha: the “base value”. Higher alpha puts more weight on the 
# most recent observations.

# beta: the “trend value”. Higher beta means the trend slope is 
# more dependent on recent trend slopes.

# gamma: the “seasonal component”. Higher gamma puts more weighting 
# on the most recent seasonal cycles.


hw_energy <- HoltWinters(energy_cons, alpha = alpha, beta = beta, gamma = FALSE)


plot(energy_cons, ylab = "Energy Concumption", xlim = c(1971,2019),lwd=2.0)
lines(hw_energy$fitted[,1], lty = 2, col = 'blue', lwd=2.0)
title("Model Prediction Using HoltWinters Library")

# Creating a table to visualize the data
table_hw <- ener_cons[3:49,] %>%  mutate("Fitted" = hw_energy$fitted[,1])

# Predicting values
energy_prediction <- predict(hw_energy, 11, prediction.interval = TRUE, 
                             level = 0.95)

pred_values_hw <- data.frame(seq(2020, 2030, 1)) %>% mutate("Mean" = energy_prediction[,1]) %>%
  mutate("Upper" = energy_prediction[,2]) %>% mutate("Lower" = energy_prediction[,3])

colnames(pred_values_hw)[1] <- "Year"

data_chart_hw <- merge(table_hw, pred_values_hw, by = "Year", all = TRUE)

colors <- c("Consumption" = "black", "Fitted" = "blue", 
            "Mean" = "red", "Upper" = "gray", "Lower" = "gray")

data_chart_hw %>% ggplot(aes(x = Year)) +
  geom_line(aes(y = Consumption, color = "Consumption"), lwd=0.8) +
  geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", lwd=0.8) +
  geom_line(aes(y = Mean, color = "Mean"), lwd=0.8) +
  geom_line(aes(y = Upper, color = "Upper"), lwd=0.8) +
  geom_line(aes(y = Lower, color = "Lower"), lwd=0.8) +
  labs(title = "ENERGY FORECAST - HOLT WINTERS MODEL",
       y = "Consumption [PJ]", color = "Legend") +
  scale_color_manual(values = colors) +
  theme_bw()

# Visualizing the predicted values
pred_values_hw %>% kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                full_width = F, font_size = 12)


plot(energy_cons, ylab = "Energy Concumption", xlim = c(1971,2031), 
     ylim = c(200000, 600000), lwd=2.0)
lines(hw_energy$fitted[,1], lty = 2, col = 'blue',lwd=2.0)
lines(energy_prediction[,1], col = "red", lwd=2.0)
lines(energy_prediction[,2], col = "gray", lwd=2.0)
lines(energy_prediction[,3], col = "gray", lwd=2.0)
title("Energy Forecast - Holt Winter Library")
#legend("topleft", legend = c("Original", "Model", "Predicted", "Confidence"),
#       lwd = 0.5, col = c("black", "blue", "red", "gray"))

#Creating a table

# Using Forecast library

energy_forecast <- forecast(hw_energy, h = 10, level = 95)

plot(energy_forecast,ylab = "Energy Concumption", xlim = c(1971,2030), lwd=2.0)
lines(energy_forecast$fitted, lty = 2, col = "purple", lwd=2)

# Forecast Evaluation

acf(energy_forecast$residuals, lag.max=20, na.action=na.pass)
Box.test(energy_forecast$residuals, lag=20, type="Ljung-Box")
hist(energy_forecast$residuals)

# Evaluating the ACF, PACF and Ljung-Box test, one can conclude that the residuals are independent
# There is no correlation between the values

