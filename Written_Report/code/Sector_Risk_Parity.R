library(tidyverse)
library(xts)
library(riskParityPortfolio)

Sector_Risk_Parity <- function(data = LCL_Index_dat){

    source("code/impute_missing_returns.r")

    Sector_Data <- data %>% spread(key = Tickers, value = Returns) %>%
        impute_missing_returns(., impute_returns_method = "Drawn_Distribution_Own")

    sigma <- cov(Sector_Data[-1])
    mu <- colMeans(Sector_Data[-1])

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