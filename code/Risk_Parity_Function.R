risk_parity <- function(dataset, current_date) {

    filter_dataset <- dataset %>% filter(date >= current_date - years(5) & date <= current_date)
    sigma <- cov(filter_dataset[-1])
    mu <- colMeans(filter_dataset[-1])

    risk_parity_mod <- riskParityPortfolio(sigma, formulation = "rc-over-b-double-index",
                                           mu = mu, lmd_mu = 0.00023)
    return(risk_parity_mod$w)
}