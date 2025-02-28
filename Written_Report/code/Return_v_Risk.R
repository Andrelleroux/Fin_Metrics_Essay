
Return_v_Risk <- function(data = LCL_Stocks_dat){

    source("code/impute_missing_returns.r")

    Parity_Data <- data %>%
        mutate(Tickers = str_sub(Tickers, 1, 3)) %>%
        select(date, Tickers, Return) %>%
        spread(key = Tickers, value = Return) %>%
        filter(date > ymd("2016-01-01")) %>%
        impute_missing_returns(., impute_returns_method = "Drawn_Distribution_Own")

    sigma <- cov(Parity_Data[-1])
    mu <- colMeans(Parity_Data[-1])


lmd_sweep <- 10^seq(-5, 2, .25)
mean_return <- c()
risk_concentration <- c()
for (lmd_mu in lmd_sweep) {
    rpp <- riskParityPortfolio(sigma, mu = mu, lmd_mu = lmd_mu,
                               formulation = "rc-over-sd vs b-times-sd")
    mean_return <- c(mean_return, rpp$mean_return)
    risk_concentration <- c(risk_concentration, rpp$risk_concentration)
}

plot_1 <- ggplot(data.frame(risk_concentration, mean_return),
       aes(x = risk_concentration, y = mean_return)) +
    geom_line() + geom_point() +
    labs(title = "Expected Return vs Risk Concentration",
         x = "Risk Concentration", y = "Expected Return") +
    theme_minimal()

return(plot_1)

}