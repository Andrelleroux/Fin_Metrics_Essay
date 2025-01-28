library(tidyverse)
library(PerformanceAnalytics)

Sector_Port_Analysis <- function(data = All_Ret_Data){

    portfolio_returns <- data %>% tbl_xts()

    performance_table <- data.frame(
        Portfolio = colnames(portfolio_returns),
        Annualized_Return = apply(portfolio_returns, 2, Return.annualized),
        Annualized_Risk = apply(portfolio_returns, 2, StdDev.annualized),
        Sharpe_Ratio = apply(portfolio_returns, 2, SharpeRatio.annualized, Rf = 0.02 / 12), # Example risk-free rate
        Max_Drawdown = apply(portfolio_returns, 2, maxDrawdown)
    )




}