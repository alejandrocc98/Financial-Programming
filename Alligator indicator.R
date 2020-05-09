library(ggplot2)
library(quantmod)
library(tidyquant)
library(dplyr)
library(PerformanceAnalytics)
library(IntroCompFinR)
library(XLConnect)

# Setting the timeframe for the information from today to one year ago
from<- Sys.Date()-366
to <- Sys.Date()

# Downloading the data from Tesla and USD/MXN
TSLA <- na.omit(tq_get("TSLA", from = from, to = to))
USDMXN <- na.omit(tq_get("MXN=X", from = from, to = to))

# Creating a column for the mean of the price
TSLA$mean <- (TSLA$high + TSLA$low)/2 
USDMXN$mean <- (USDMXN$high + USDMXN$low)/2


# This is used to save the data into an excel file
wb <- loadWorkbook("TSLA.xlsx")
writeWorksheet(object = wb, data = TSLA, sheet = getSheets(wb)[1])
renameSheet(wb,getSheets(wb)[1],"TSLA")
createSheet(wb, "USDMXN")
writeWorksheet(object = wb, data = USDMXN, sheet = "USDMXN")
saveWorkbook(wb, "alligator.xlsx")


   
# TSLA Alligator Indicator    
# Jaw line

# Creating the jaw column and setting the values of N and S
TSLA$jaw <- NA
N <- 13 # "N" represents the peroiods of the SMMA
S <- 8  # "S" represents the periods in the future the SMMA will be shifted

# The first value of the SMMA is claculated as a SMA
TSLA$jaw[N+S] <- sum(TSLA$mean[1:N])/N
Q <- N + S + 1
# The second value is calculated with this formula:
# SMMA(i) = (SMMA1*(N-1)+mean(i))/N
TSLA$jaw[Q] <- (TSLA$jaw[N+S]*(N-1)+TSLA$mean[Q])/N

# The subsequent values are calculated with a "while" loop and this formula:
# SMMA(i) = ((SMMA(i-1)*N)-SMMA(i-1) + mean(i))/N
i <- Q + 1
while(i < nrow(TSLA)){
    TSLA$jaw[i] <- ((TSLA$jaw[i-1]* N) - TSLA$jaw[i-1]+ TSLA$mean[i-1])/N
    i <- i+1
}

# Teeth line
TSLA$teeth <- NA
N <- 8
S <- 5

TSLA$teeth[N+S] <- sum(TSLA$mean[1:N])/N
Q <- N + S + 1
TSLA$teeth[Q] <- (TSLA$teeth[N+S]*(N-1)+TSLA$mean[Q])/N

i <- Q + 1
while(i < nrow(TSLA)){
    TSLA$teeth[i] <- (TSLA$teeth[i-1]* N - TSLA$teeth[i-1]+ TSLA$mean[i-1])/N
    i <- i+1
}

# Lips Line
TSLA$lips <- NA
N <- 5
S <- 3

TSLA$lips[N+S] <- sum(TSLA$mean[1:N])/N
Q <- N + S + 1
TSLA$lips[Q] <- (TSLA$lips[N+S]*(N-1)+TSLA$mean[Q])/N

i <- Q + 1
while(i < nrow(TSLA)){
    TSLA$lips[i] <- (TSLA$lips[i-1]* N - TSLA$lips[i-1]+ TSLA$mean[i-1])/N
    i <- i+1
}



TSLA %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close),
                     colour_up = "darkgreen",
                     colour_down = "red",
                     fill_up = "darkgreen",
                     fill_down = "red") +
    labs(title = "TSLA", subtitle = "Alligator Indicator (13,8,8,5,5,3)",
         y = "Closing Price", x = "") +
    scale_y_continuous(labels = scales::dollar) +
    geom_line(aes(x = date, y = jaw), color = "blue") +
    geom_line(aes(x = date, y = teeth), color = "red") +
    geom_line(aes(x = date, y = lips), color = "darkgreen")


# USDMXN Alligator Indicator
# Jaw line
USDMXN$jaw <- NA
N <- 13
S <- 8

USDMXN$jaw[N+S] <- sum(USDMXN$mean[1:N])/N
Q <- N + S + 1
USDMXN$jaw[Q] <- (USDMXN$jaw[N+S]*(N-1)+USDMXN$mean[Q])/N

i <- Q
while(i < nrow(USDMXN)){
    USDMXN$jaw[i] <- (USDMXN$jaw[i-1]* N - USDMXN$jaw[i-1]+ USDMXN$mean[i-1])/N
    i <- i+1
}

# Teeth line
USDMXN$teeth <- NA
N <- 8
S <- 5

USDMXN$teeth[N+S] <- sum(USDMXN$mean[1:N])/N
Q <- N + S + 1
USDMXN$teeth[Q] <- (USDMXN$teeth[N+S]*(N-1)+USDMXN$mean[Q])/N

i <- Q
while(i < nrow(USDMXN)){
    USDMXN$teeth[i] <- (USDMXN$teeth[i-1]* N - USDMXN$teeth[i-1]+ USDMXN$mean[i-1])/N
    i <- i+1
}

# Lips Line
USDMXN$lips <- NA
N <- 5
S <- 3

USDMXN$lips[N+S] <- sum(USDMXN$mean[1:N])/N
Q <- N + S + 1
USDMXN$lips[Q] <- (USDMXN$lips[N+S]*(N-1)+USDMXN$mean[Q])/N

i <- Q
while(i < nrow(USDMXN)){
    USDMXN$lips[i] <- (USDMXN$lips[i-1]* N - USDMXN$lips[i-1]+ USDMXN$mean[i-1])/N
    i <- i+1
}



USDMXN %>%
    ggplot(aes(x = date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close),
                     colour_up = "darkgreen",
                     colour_down = "red",
                     fill_up = "darkgreen",
                     fill_down = "red") +
    labs(title = "USD/MXN", subtitle = "Alligator Indicator (13,8,8,5,5,3)",
         y = "Closing Price", x = "") +
    scale_y_continuous(labels = scales::dollar) +
    geom_line(aes(x = date, y = jaw), color = "blue") +
    geom_line(aes(x = date, y = teeth), color = "red") +
    geom_line(aes(x = date, y = lips), color = "darkgreen")

# Apoyo para cálculo de la SMMA (Media Movil Suavizada):
# https://www.metatrader5.com/es/terminal/help/indicators/trend_indicators/ma
