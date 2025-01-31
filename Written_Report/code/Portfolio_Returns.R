

Portfolio_Returns <- function(weights_1 = weights_df_RPP,
                              weights_2 = weights_df_Tangent,
                              returns = LCL_Stocks_dat,
                              Indices_Ret = LCL_indice_init){

    Ind_join_dat <- Indices_Ret %>%
        filter(date >= as.Date("2019-03-15") & date <= as.Date("2024-12-31")) %>%
        filter(Tickers == "J203" | Tickers == "J200") %>%
        mutate(cumreturn_Ind = (cumprod(1 + Returns))) %>%
        # Start at 1
        mutate(cumreturn_Ind = cumreturn_Ind / first(cumreturn_Ind))

    source("code/impute_missing_returns.r")

    Returns_Data <- returns %>%
        mutate(Tickers = str_sub(Tickers, 1, 3)) %>%
        select(date, Tickers, Return) %>%
        spread(key = Tickers, value = Return) %>%
        filter(date > ymd("2016-01-01")) %>%
        impute_missing_returns(., impute_returns_method = "Drawn_Distribution_Own") %>%
        tbl_xts()

    Weights_RPP <- weights_1 %>% tbl_xts()
    Weights_Tan <- weights_2 %>% tbl_xts()

    RPP_RetPort <-
        rmsfuns::Safe_Return.portfolio(Returns_Data,
                                       weights = Weights_RPP, lag_weights = TRUE,
                                       verbose = TRUE, contribution = TRUE,
                                       value = 1000, geometric = TRUE)

    Tan_RetPort <-
        rmsfuns::Safe_Return.portfolio(Returns_Data,
                                       weights = Weights_Tan, lag_weights = TRUE,
                                       verbose = TRUE, contribution = TRUE,
                                       value = 1000, geometric = TRUE)

    Cum_Plot_Data <- RPP_RetPort$returns %>% xts_tbl() %>%
        rename(RPP_Returns = portfolio.returns) %>%
        left_join(., Tan_RetPort$returns %>% xts_tbl(), by = "date") %>%
        left_join(., Ind_join_dat, by = "date") %>%
        rename(Tangency_Returns = portfolio.returns) %>%
        mutate(cumreturn_Rpp = (cumprod(1 + RPP_Returns))) %>%
        # Start at 1
        mutate(cumreturn_Rpp = cumreturn_Rpp / first(cumreturn_Rpp)) %>%
        mutate(cumreturn_Tan = (cumprod(1 + Tangency_Returns))) %>%
        # Start at 1
        mutate(cumreturn_Tan = cumreturn_Tan / first(cumreturn_Tan)) %>%
        ggplot() +
        geom_line(aes(date, cumreturn_Rpp, color = "Risk Parity")) +
        geom_line(aes(date, cumreturn_Tan, color = "Maximum Sharpe Value")) +
        geom_line(data = Ind_join_dat, aes(date, cumreturn_Ind, color = Tickers)) +
        scale_color_manual(
            values = c("Risk Parity" = "royalblue",
                       "Maximum Sharpe Value" = "gold2")
        ) +
        labs(
            x = "Date",
            y = "Cumulative Returns",
            color = "Portfolios"
        ) +
        theme_minimal() +
        theme(
            legend.position = "bottom"
        )

    plot_1 <- RPP_RetPort$BOP.Weight %>% .[endpoints(.,'months')] %>% chart.StackedBar(main = "Optimal Weights of Portfolio",
                                                                             ylab = "Weight (%)",
                                                                             xlab = "Date",
                                                                             col = rep(brewer.pal(12, "Paired"), length.out = 37),
                                                                             cex.legend = 0.5)

    plot_2 <- Tan_RetPort$BOP.Weight %>% .[endpoints(.,'months')] %>% chart.StackedBar(main = "Optimal Weights of Portfolio",
                                                                             ylab = "Weight (%)",
                                                                             xlab = "Date",
                                                                             col = rep(brewer.pal(12, "Paired"), length.out = 37),
                                                                             cex.legend = 0.5)

    Returns_List <- list(Cum_Plot_Data, plot_1, plot_2)

}