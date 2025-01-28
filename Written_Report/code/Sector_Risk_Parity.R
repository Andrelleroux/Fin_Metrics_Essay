library(tidyverse)
library(xts)
library(riskParityPortfolio)

Sector_Risk_Parity <- function(data = LCL_Index_dat, rebal = Rebal_Days){

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

    plot_1 <- barplotPortfolioRisk(w_all, sigma) +
        theme_minimal() +
        theme(legend.position = "bottom")

    lmd_sweep <- 10^seq(-5, 2, .25)
    mean_return <- c()
    risk_concentration <- c()
    for (lmd_mu in lmd_sweep) {
        rpp <- riskParityPortfolio(sigma, mu = mu, lmd_mu = lmd_mu,
                                   formulation = "rc-over-sd vs b-times-sd")
        mean_return <- c(mean_return, rpp$mean_return)
        risk_concentration <- c(risk_concentration, rpp$risk_concentration)
    }

    plot_2 <- ggplot(data.frame(risk_concentration, mean_return),
           aes(x = risk_concentration, y = mean_return)) +
        geom_line() + geom_point() +
        labs(title = "Expected Return vs Risk Concentration",
             x = "Risk Concentration", y = "Expected Return") +
        theme_minimal()

    risk_parity <- function(dataset, current_date) {

        filter_dataset <- dataset %>% filter(date >= current_date - years(5) & date <= current_date)
        sigma <- cov(filter_dataset[-1])
        mu <- colMeans(filter_dataset[-1])

        risk_parity_mod <- riskParityPortfolio(sigma, formulation = "rc-over-b-double-index",
                                               mu = mu, lmd_mu = 0.00023)
        return(risk_parity_mod$w)
    }

    rebalance_dates <- rebal %>% filter(Date_Type == "Reb Trade Day") %>%
        filter(date >= as.Date("2004-11-30") & date <= as.Date("2024-12-31")) %>% select(date)

    weights_df_RPP <- rebalance_dates %>%
        rowwise() %>%
        mutate(weights = list(risk_parity(Sector_Data, date))) %>%
        unnest_wider(weights)

    source("code/max_sharpe_ratio.r")

    weights_df_Tangent <- rebalance_dates %>%
        rowwise() %>%
        mutate(weights = list(max_sharpe_ratio(Sector_Data, date))) %>%
        unnest_wider(weights)

    weights_df_Equal <- rebalance_dates %>%
        rowwise() %>%
        mutate(weights = list(setNames(rep(1 / (ncol(Sector_Data) - 1), ncol(Sector_Data) - 1),
                                       colnames(Sector_Data)[-1]))) %>%
        unnest_wider(weights)

    Weights_RPP <- weights_df_RPP %>% tbl_xts()
    Weights_Tan <- weights_df_Tangent %>% tbl_xts()
    Weights_Equal <- weights_df_Equal %>% tbl_xts()
    Returns_Rpp <- Sector_Data %>% tbl_xts()

    RPP_RetPort <- rmsfuns::Safe_Return.portfolio(Returns_Rpp,
                                       weights = Weights_RPP, lag_weights = TRUE,
                                       verbose = TRUE, contribution = TRUE,
                                       value = 1000, geometric = TRUE)

    Tan_RetPort <- rmsfuns::Safe_Return.portfolio(Returns_Rpp,
                                       weights = Weights_Tan, lag_weights = TRUE,
                                       verbose = TRUE, contribution = TRUE,
                                       value = 1000, geometric = TRUE)

    Equal_RetPort <- rmsfuns::Safe_Return.portfolio(Returns_Rpp,
                                                    weights = Weights_Equal, lag_weights = TRUE,
                                                    verbose = TRUE, contribution = TRUE,
                                                    value = 1000, geometric = TRUE)

    Sector_plot <- Sector_Data %>%
        filter(date >= as.Date("2004-12-01") & date <= as.Date("2024-12-31")) %>%
        mutate(cumreturn_ALSI = (cumprod(1 + J203))) %>%
        mutate(cumreturn_ALSI = cumreturn_ALSI / first(cumreturn_ALSI)) %>%
        mutate(cumreturn_Bonds = (cumprod(1 + ALBITR))) %>%
        mutate(cumreturn_Bonds = cumreturn_Bonds / first(cumreturn_Bonds)) %>%
        select(date, cumreturn_Bonds, cumreturn_ALSI)

    plot_3 <- RPP_RetPort$returns %>% xts_tbl() %>%
        rename(RPP_Returns = portfolio.returns) %>%
        left_join(., Tan_RetPort$returns %>% xts_tbl(), by = "date") %>%
        rename(Tangency_Returns = portfolio.returns) %>%
        left_join(., Equal_RetPort$returns %>% xts_tbl(), by = "date") %>%
        rename(Equal_Returns = portfolio.returns) %>%
        mutate(cumreturn_Rpp = (cumprod(1 + RPP_Returns))) %>%
        # Start at 1
        mutate(cumreturn_Rpp = cumreturn_Rpp / first(cumreturn_Rpp)) %>%
        mutate(cumreturn_Tan = (cumprod(1 + Tangency_Returns))) %>%
        # Start at 1
        mutate(cumreturn_Tan = cumreturn_Tan / first(cumreturn_Tan)) %>%
        mutate(cumreturn_Equal = (cumprod(1 + Equal_Returns))) %>%
        # Start at 1
        mutate(cumreturn_Equal = cumreturn_Equal / first(cumreturn_Equal)) %>%
        ggplot() +
        geom_line(aes(date, cumreturn_Rpp, color = "Risk Parity Portfolio")) +
        geom_line(data = Sector_plot, aes(date, cumreturn_ALSI, color = "All Share Index")) +
        geom_line(data = Sector_plot, aes(date, cumreturn_Bonds, color = "ALBITR Bonds Index")) +
        geom_line(aes(date, cumreturn_Tan, color = "Maximum Sharpe Ratio Portfolio")) +
        geom_line(aes(date, cumreturn_Equal, color = "Equal Weighting Portfolio")) +
        labs(
            x = "Date",
            y = "Cumulative Returns",
            color = "Portfolios"
        ) +
        theme_minimal() +
        theme(
            legend.position = "bottom"
        )

    plot_4 <- RPP_RetPort$BOP.Weight %>% .[endpoints(.,'months')] %>% chart.StackedBar(main = "Optimal Weights of Portfolio",
                                                                             ylab = "Weight (%)",
                                                                             xlab = "Date",
                                                                             col = rep(brewer.pal(12, "Paired"), length.out = 37),
                                                                             cex.legend = 0.5)

    plot_5 <- Tan_RetPort$BOP.Weight %>% .[endpoints(.,'months')] %>% chart.StackedBar(main = "Optimal Weights of Portfolio",
                                                                                       ylab = "Weight (%)",
                                                                                       xlab = "Date",
                                                                                       col = rep(brewer.pal(12, "Paired"), length.out = 37),
                                                                                       cex.legend = 0.5)

    plot_6 <- Equal_RetPort$BOP.Weight %>% .[endpoints(.,'months')] %>% chart.StackedBar(main = "Optimal Weights of Portfolio",
                                                                                         ylab = "Weight (%)",
                                                                                         xlab = "Date",
                                                                                         col = rep(brewer.pal(12, "Paired"), length.out = 37),
                                                                                         cex.legend = 0.5)

    All_Ret_Data <- data.frame(date = Sector_plot$date,
                               Risk_Parity = RPP_RetPort$returns,
                               Equal_Weighting = Equal_RetPort$returns,
                               Max_Sharpe = Tan_RetPort$returns) %>%
        rename(Risk_Parity = portfolio.returns, Equal_Weighting = portfolio.returns.1,
               Max_Sharpe = portfolio.returns.2)

    Return_List <- list(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6, All_Ret_Data)


}