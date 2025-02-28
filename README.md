# Coding for the Financial Econometrics Essay on Risk Parity Portfolios

In this markdown folder I will explain my thinking and working for the
coding aspect of the Financial Econometrics essay submitted in January
2025. I will talk through the coding done to get the results represented
in the written report of this essay.

First I clear the environment and set the specifications of the R chunks
that are used throughout the written report. I can then set the relevant
working directory and call all the relevant functions from the code
folder of the directory.

``` r
rm(list = ls()) # Clean your environment:
gc()
knitr::opts_chunk$set(message=FALSE, warning=FALSE)
pacman::p_load(tidyverse, riskParityPortfolio, xts, quadprog, tbl2xts, PerformanceAnalytics,
               RColorBrewer, knitr, kableExtra)

setwd("~/Masters_2024_stuff/Financial_Econometrics/Fin_Metrics_Project")
list.files('Written_Report/code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
```

## Packages

What follows is the list of packages that have been used throughout the
functions of this project:

-   tidyverse (includes ggplot2, dplyr)
-   Texevier
-   fmxdat
-   PerformanceAnalytics
-   tbl2xts
-   kableExtra
-   RColorBrewer
-   riskParityPortfolio
-   quadprog

## Setting up the Data

``` r
Data <- Data_Setup()

LCL_Index_dat <- Data[[2]]
Rebal_Days <- Data[[3]]
```

The Data_Setup function takes the data folders provider by the
administrator of the module and reads them into data frames. The Stock
specific daily data for the JSE is adapted to only include the current
top 40 stocks listed on the exchange. The index specific data is
filtered for all indices that are relevant to constructing the
portfolio, as referenced in the written report. The mean of the
short-term fixed-interest assets across the sample is used as the risk
free rate, this is used later in the Maximum Sharpe ratio portfolio.

This function then returns a list variable that includes the data of the
stocks, indices and relevant rebalance dates that can be referenced at
later points.

## Assets Portfolios Construction

``` r
Sector_Plots <- Sector_Risk_Parity(data = LCL_Index_dat, rebal = Rebal_Days)
```

Here the data from the initial function is parsed to the
Sector_Risk_Parity function to construct and plot relevant aspects of
the portfolios. All relevant plots that are computed along the way and
the dataset with the returns of the relevant portfolio are returned in a
list variables. The first thing that is done in this function is to
impute the missing values of the indices that have starting points after
other indices. This is a separate function called impute_missing_returns
that, when used with the specifics that I chose, will draw the
replacement values from the distribution of that specific variable.

``` r
Sector_Plots[[1]]
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

The first step was to look at the different methods of risk parity that
is allowed by the riskParityPortfolio. This is compared to an equal
weighting portfolio to demonstrate differences in approach. The RPP + mu
is chosen as it allows for the additional consideration of returns in
the construction of the portfolio.

``` r
Sector_Plots[[2]]
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

Next I can look at the risk versus return balance of the Risk Parity
portfolio when computed with the additional optimisation on returns, as
can be seen there is decreasing marginal returns in Expected Return when
taking on additional risk. This is done by running through a for loop of
different lambda_mu values, a weighting parameter that sets the focus
between risk and returns optimisation. These values are then stored and
plotted.

Next, other function are created in order to calculate the relevant
weights of the different portfolios at each rebalance date. The
risk_parity function takes in a dataset and the current date as
arguments and then returns the relevant weights looking back at the 5
years prior to the current date to optimise the portfolio using the
riskParityPortfolio package and the RPP + mu method. A similar function
is then built for the Maximum Sharpe ratio portfolio, this function is
called max_sharpe_ratio and takes the same arguments as the risk_parity
function. This functio uses the quadprog package to optimise a portfolio
based on the maximum sharpe ratio and standard constraints such as
non-negativity and addition to 1.

The output of these functions for every rebalance date, as well as for
the equally weighted portfolio is the parsed to the
rmsfuns::Safe_return.portfolio function is order to compute the
portfolio returns of each of the three portfolios. This is combined with
the index specific returns of the bond market and the JSE ALSI to plot
the cumulative returns of all the portfolios and other two indices. As
seen below.

``` r
Sector_Plots[[3]]
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

Next the PerformanceAnalytics package allows me to plot the weights of
the portfolio at the start of each month throughout the period. This
shows the different approaches to diversification very clearly, with the
RPP preferring to invest more into bonds throughout the period, while
the Max Sharpe ratio portfolio changes the structure and preference of
the assets very regularly. The plots were originally made with the
PerformanceAnalytics package, however in order to adapt their appearance
the dataset was converted to a data frame and plotted with ggplot.

``` r
Sector_Plots[[4]]
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
Sector_Plots[[5]]
```

![](README_files/figure-markdown_github/unnamed-chunk-7-2.png)

``` r
Sector_Plots[[6]]
```

![](README_files/figure-markdown_github/unnamed-chunk-7-3.png)

``` r
Sector_Analysis <- Sector_Port_Analysis(data = Sector_Plots[[7]])
```

Next the function puts all the returns data and dates together in order
to be able to do thorough analysis of the returns of each of these
portfolios. This dataset is then parsed to a function called
Sector_Port_Analysis in order to look at the performance of each
portfolio. The parsed dataset is then transformed to an xts object in
order to be used in PerformanceAnalytics standardised plots. The
following plots are created using the PerformanceAnalytic package

``` r
Sector_Analysis[[1]]
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
Sector_Analysis[[2]]
```

![](README_files/figure-markdown_github/unnamed-chunk-9-2.png)

``` r
Sector_Analysis[[3]]
```

    ## NULL

``` r
Sector_Analysis[[4]]
```

![](README_files/figure-markdown_github/unnamed-chunk-9-3.png)

However the Risk v Return scatterplot was not included in the final
write-up. A data frame was also constructed with the annualised return
and standard deviation; the maximum drawdown; the expected shortfall and
the Sharpe ratio for each portfolio. This data frame was then used to
create a table using the knitr and KableExtra packages and written in
LaTeX to be knitted by the Rmarkdown file.

## Unused Analysis

Further analysis was done originally to investigate a risk parity
portfolio for just JSE top 40 stocks, a purely equity portfolio.
However, this analysis was eventually left out of the written report, I
include it here for those that are interested. Firstly I invstigated the
different RPP methods for the stocks in the same way as the indices,
again using the riskParityPortfolio package.

``` r
Risk_Parity_Plot <- Risk_Parity_LCL(Data[[1]])
Risk_Parity_Plot
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

Next a similar Return vs Risk plot is created for the local stocks RPP
portfolio by sweeping through a wide range of possible values for
lambda_mu.

``` r
Return_Risk_Plot <- Return_v_Risk(Data[[1]])
Return_Risk_Plot
```

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

Next the weights throughout time are calculated to plot the cumulative
returns of both a Risk Parity Portfolio and a Maximum Sharpe Ratio
portfolio, the monthly returns of the JSE ALSI and JSE Top 40 are also
plotted to show the relative relationship. First the Portfolio_Backtest
function is used to return the weights throughout the period and the
rebalance dates for each technique.

``` r
Stock_Weights <- Portfolio_Backtest(Data[[1]], Data[[2]])

Plots_Stocks <- Portfolio_Returns(Stock_Weights[[1]], Stock_Weights[[2]], Data[[1]], Data[[2]])
```

![](README_files/figure-markdown_github/unnamed-chunk-12-1.png)![](README_files/figure-markdown_github/unnamed-chunk-12-2.png)

These weights are then parsed to a seperate function called
Portfolio_Returns to do further analysis and construct some plots using
PerformanceAnalytics for the balance of weights throughout the period.

``` r
Plots_Stocks[[1]]
```

![](README_files/figure-markdown_github/unnamed-chunk-13-1.png)

This is definitely an interesting potential future direction of study,
however it was not included in this essay.
