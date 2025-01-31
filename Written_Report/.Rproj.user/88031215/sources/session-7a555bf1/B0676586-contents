library(tidyverse)
library(PerformanceAnalytics)
library(knitr)
library(kableExtra)

Sector_Port_Analysis <- function(data = All_Ret_Data){

    portfolio_returns <- data %>% tbl_xts()

    performance_table <- data.frame(
        Portfolio = colnames(portfolio_returns),
        Ann_Return = apply(portfolio_returns, 2, Return.annualized),
        Ann_Risk = apply(portfolio_returns, 2, StdDev.annualized),
        Sharpe_Ratio = apply(portfolio_returns, 2, SharpeRatio.annualized, Rf = 0.006042),
        Max_Drawdown = apply(portfolio_returns, 2, maxDrawdown),
        E_Shortfall = apply(portfolio_returns, 2, ES, p = 0.95)
    )

    performance_table

    Analysis_0 <- chart.RollingPerformance(
        portfolio_returns,
        width = 12,  # Rolling 12-month window
        FUN = function(R) Return.annualized(R, scale = 12),
        main = " ",
        legend.loc = "topright"
    )

    Analysis_1 <- chart.Drawdown(portfolio_returns, main = " ", legend.loc = "bottomright")

    Analysis_2 <- chart.RiskReturnScatter(portfolio_returns, main = " ")

    Analysis_3 <- chart.RollingPerformance(
        portfolio_returns,
        width = 12,
        FUN = StdDev.annualized, # Use StdDev.annualized for rolling standard deviation
        main = " ",
        legend.loc = "topleft"
    )

    Analysis_Table <- performance_table %>% select(-Portfolio) %>%
        kable("latex", booktabs = TRUE, align = "c") %>%
        kable_styling(latex_options = c("striped", "hold_position"),
                      position = "center", full_width = FALSE) %>%
        row_spec(0, bold = TRUE, color = "white", background = "olive") %>% # Header styling
        column_spec(1, bold = TRUE)

    List_Return <- list(Analysis_0, Analysis_1, Analysis_2, Analysis_3, Analysis_Table)

}