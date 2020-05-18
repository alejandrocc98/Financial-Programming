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
from <- "2017-01-01"
to <- "2019-12-31"

getSymbols(c("BIMBOA.MX","^MXX"), from = from, to = to, periodicity = "monthly")
prices <- Ad(merge(BIMBOA.MX,MXX))
returns.df <-as.data.frame(diff(log(prices)))
retstock<-(returns.df[,1])
retindex<-(returns.df[,2])
results <- mylm(retstock,retindex)

# CAPM para calcular el rendimiento esperado de la acción
capm <- function(stockr, mktr, rfr) {
    model <- lm((stockr - rfr) ~ (mktr - rfr))
    s <-summary(model)
    s$coefficients
    
    t_critical_value <- abs(qt(0.025,model$df.residual))
    residual <- df.residual(model) + 2
    b0 <- s$coefficients[1,1]
    se_b0 <- s$coefficients[1,2]
    pvalue_b0 <- s$coefficients[1,4]
    min_b0 <- s$coefficients[1,1] - (2*s$coefficients[1,2])
    max_b0 <- s$coefficients[1,1] + (2*s$coefficients[1,2])
    b1 <- s$coefficients[2,1]
    se_b1 <- s$coefficients[2,2]
    min_b1 <- s$coefficients[2,1] - (t_critical_value * s$coefficients[2,2])
    max_b1 <- s$coefficients[2,1] + (t_critical_value * s$coefficients[2,2])
    
    output <- c(b0, se_b0, pvalue_b0, min_b0, max_b0, b1, se_b1, min_b1, max_b1,
                residual)
    names(output)= c("b0","se(b0)","pvalue_b0", "min(b0)","max(b0)","b1","se(b1)"
                     ,"min(b1)","max(b1)","# non missing")
    return(output)
}
rfr.df <- as.data.frame(getSymbols("cap", from = from, to = to, 
                                   periodicity ="monthly", src = "FRED",
                                   auto.assign = FALSE)[index(prices)]/100/12)
capm_results <- capm(returns.df[,1],returns.df[,2],rfr.df[,1])

OER <- function(stock,market,rf) {
    model <- lm((stock-rf) ~ (market-rf))
    s <-summary(model)
    s$coefficients
    
    residual= df.residual(model)+2
    
    output= c(s$coefficients[1,1],s$coefficients[1,2],(s$coefficients[1,1]-2*s$coefficients[1,2]),
              (s$coefficients[1,1]+2*s$coefficients[1,2]),s$coefficients[2,1],s$coefficients[2,2],
              (s$coefficients[2,1]-2*s$coefficients[2,2]),(s$coefficients[2,1]+2*s$coefficients[2,2])
              ,residual)
    names(output)= c("b0","se(b0)","min(b0)","max(b0)","b1","se(b1)","min(b1)","max(b1)","# non missing")
    
    output2 <- s$coefficients[2,1]+2*s$coefficients[2,2]
    if(residual < 24){
        print("The CAPM model was calculated with less than 24 valid observations")
        
    }
    else{
        print(c("The CAPM was calculated with",residual,"observations"))
    }
    if(output2 < 1){
        print(" 95% of the times the stock will give returns above the market")
        return(output)
    }
    else{
        print("There is not significan evidence to say that this stocks offers returns above the market")
        return(output)
    }
    
    
    
    
}
OER(returns.df[,1],returns.df[,2],rfr.df[,1])
