library(tidyverse)
library(quadprog)

max_sharpe_ratio <- function(dataset, current_date, Rfr = 0.006042) {
    filter_dataset <- dataset %>% filter(date >= current_date - years(5) & date <= current_date)
    stock_names <- colnames(filter_dataset)[-1]  # Exclude the 'date' column
    # Number of assets (stocks)
    N <- length(stock_names)
    # Calculate covariance and mean returns
    sigma <- cov(filter_dataset[-1])  # Exclude the date column
    mu <- colMeans(filter_dataset[-1])  # Exclude the date column

    mu <- mu - Rfr

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