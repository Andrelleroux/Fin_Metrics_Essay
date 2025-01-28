library(tidyverse)
library(xts)
library(riskParityPortfolio)
library(quadprog)
library(tbl2xts)
library(PerformanceAnalytics)
library(RColorBrewer)

Portfolio_Backtest <- function(data_RPP = LCL_Stocks_dat, data_comp = LCL_indice_init){

    source("code/impute_missing_returns.r")

    Parity_Data <- data_RPP %>%
        mutate(Tickers = str_sub(Tickers, 1, 3)) %>%
        select(date, Tickers, Return) %>%
        spread(key = Tickers, value = Return) %>%
        filter(date > ymd("2016-01-01")) %>%
        impute_missing_returns(., impute_returns_method = "Drawn_Distribution_Own")

    #Risk Parity Function for weights
    risk_parity <- function(dataset, current_date) {

        filter_dataset <- dataset %>% filter(date >= current_date - years(3) & date <= current_date)
        sigma <- cov(filter_dataset[-1])
        mu <- colMeans(filter_dataset[-1])

        risk_parity_mod <- riskParityPortfolio(sigma, formulation = "rc-over-b-double-index",
                                                         mu = mu, lmd_mu = 0.00023)
        return(risk_parity_mod$w)
    }

    #Tangency Portfolio (Max Sharpe Ratio)
    max_sharpe_ratio <- function(dataset, current_date) {
        filter_dataset <- dataset %>% filter(date >= current_date - years(3) & date <= current_date)
        stock_names <- colnames(filter_dataset)[-1]  # Exclude the 'date' column
        # Number of assets (stocks)
        N <- length(stock_names)
        # Calculate covariance and mean returns
        sigma <- cov(filter_dataset[-1])  # Exclude the date column
        mu <- colMeans(filter_dataset[-1])  # Exclude the date column

        # If the mean returns are too small, return zero weights
        if (all(mu <= 1e-8)) {
            return(setNames(rep(0, N), stock_names))
        }

        # Optimization setup
        Dmat <- 2 * sigma
        Amat <- diag(N)
        Amat <- cbind(mu, Amat)
        bvec <- c(1, rep(0, N))
        dvec <- rep(0, N)

        # Solve the quadratic programming problem
        res <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
        # Normalize the weights
        w <- res$solution / sum(res$solution)
        return(setNames(w, stock_names))
    }

    rebalance_dates <- Rebal_Days %>% filter(Date_Type == "Reb Trade Day") %>%
        filter(date >= as.Date("2019-01-01") & date <= as.Date("2024-12-31")) %>% select(date)

    weights_df_RPP <- rebalance_dates %>%
        rowwise() %>%
        mutate(weights = list(risk_parity(Parity_Data, date))) %>%
        unnest_wider(weights)

    weights_df_Tangent <- rebalance_dates %>%
        rowwise() %>%
        mutate(weights = list(max_sharpe_ratio(Parity_Data, date))) %>%
        unnest_wider(weights)

    Plot_1 <- weights_df_RPP %>% tbl_xts() %>%
        chart.StackedBar(main = "Optimal Weights of Portfolio",
                         ylab = "Weight (%)",
                         xlab = "Date",
                         col = rep(brewer.pal(12, "Paired"), length.out = 37),
                         cex.legend = 0.5)

    Plot_2 <- weights_df_Tangent %>% tbl_xts() %>%
        chart.StackedBar(main = "Optimal Weights of Portfolio",
                         ylab = "Weight (%)",
                         xlab = "Date",
                         col = rep(brewer.pal(12, "Paired"), length.out = 37),
                         cex.legend = 0.5)

}