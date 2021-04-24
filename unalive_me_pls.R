# Title     : TODO
# Objective : TODO
# Created by: m20190417
# Created on: 4/6/2021

pacman::p_load(forecast,quantmod, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
               gridExtra, tseries, lmtest, FinTS, mgarchBEKK, ccgarch, xtable, MTS, plm)

DJI <- getSymbols("DJI", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)
DJI_ts <- xts(DJI)
DJI_adj <- DJI_ts$DJI.Adjusted
head(DJI_adj)