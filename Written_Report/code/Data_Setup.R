library(tidyverse)

Data_Setup <- function(){

    LCL_Stocks_init <- read_rds("data/LCL_Stock_Returns.rds")
    LCL_indice_init <- read_rds("data/LCL_Indices.rds")
    Global_indice_init <- read_rds("data/Global_Indices.rds")
    ASISA_rets_init <- read_rds("data/ASISA_Rets.rds")
    Rebal_Days <- read_rds("data/Rebalance_days.rds")

    jse_top_40 <- c(
        "AGL", "AMS", "ANG", "APN", "BHP", "BID", "BTI", "BVT", "CFR", "CLS",
        "CPI", "DSY", "EXX", "FSR", "GFI", "GRT", "IMP", "INL", "INP", "MCG",
        "MNP", "MRP", "MTN", "NED", "NPN", "NRP", "OMU", "PRX", "REM", "RNI",
        "SAP", "SBK", "SHP", "SLM", "SOL", "SSW", "VOD", "WHL"
    )

    LCL_Stocks_dat <- LCL_Stocks_init %>%
        filter(str_sub(Tickers, 1, 3) %in% jse_top_40)

    Indices_of_Interest <- c(
        "J203", "J212", "J310", "J311", "J253", "ALBITR"
        )

    LCL_Index_dat <- LCL_indice_init %>%
        filter(Tickers %in% Indices_of_Interest) %>%
        select(-Name)

    STEFI_mean <- LCL_indice_init %>% filter(Tickers == "STEFI") %>%
        summarise(Mean = mean(Returns))

    List_for_Return <- list(LCL_Stocks_dat, LCL_Index_dat, Rebal_Days)

}