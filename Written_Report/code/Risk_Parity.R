library(tidyverse)
library(xts)
library(portfolioBacktest)
library(riskParityPortfolio)

Risk_Parity_LCL <- function(data = LCL_Stocks_dat){

    source("code/impute_missing_returns.r")

    Parity_Data <- data %>%
        mutate(Tickers = str_sub(Tickers, 1, 3)) %>%
        select(date, Tickers, Return) %>%
        spread(key = Tickers, value = Return) %>%
        filter(date > ymd("2016-01-01")) %>%
        impute_missing_returns(., impute_returns_method = "Drawn_Distribution_Own")

    sigma <- cov(Parity_Data[-1])
    mu <- colMeans(Parity_Data[-1])

    rpp_vanilla <- riskParityPortfolio(sigma)

    rpp_naive <- riskParityPortfolio(sigma, formulation = "diag")

    rpp_mu <- riskParityPortfolio(sigma, formulation = "rc-over-b-double-index",
                                  mu = mu, lmd_mu = 1e-3)

    w_all <- cbind("EWP"           = rep(1/nrow(sigma), nrow(sigma)),
                   "RPP (naive)"   = rpp_naive$w,
                   "RPP (vanilla)" = rpp_vanilla$w,
                   "RPP + mu"      = rpp_mu$w)

    barplotPortfolioRisk(w_all, sigma) +
        theme_minimal() +
        theme(legend.position = "bottom")



}