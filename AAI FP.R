library(quantmod)
rm(list = ls())
options(digits=5)
options(scipen=100)

mylm <- function(retstock,retindex){ 
    stockm <-lm(retstock  ~ retindex)
    #View(summary(stocklm))
    stocklm <- summary(stockm)
    residuals <- df.residual(stockm)+2
    output <- c(stocklm$coefficients[1,1],stocklm$coefficients[1,2],
                stocklm$coefficients[1,4],stocklm$coefficients[2,1],
                stocklm$coefficients[2,2],stocklm$coefficients[2,4],
                residuals)
    names(output)<-c("b0","se(b0)","p-value(b0)","b1","se(b1)","p-value(b1)","Validobs")
    output
}

getSymbols(c("BIMBOA.MX","^MXX"), from = "2019-01-01", to = "2019-12-31")
prices <- Ad(merge(BIMBOA.MX,MXX))
returns.df <-as.data.frame(diff(log(prices)))
retstock<-(returns.df[,1])
retindex<-(returns.df[,2])
results <- mylm(retstock,retindex)



