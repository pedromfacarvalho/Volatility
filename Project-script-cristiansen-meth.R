# Title     : TODO
# Objective : TODO
# Created by: m20190417
# Created on: 2/7/2021


pacman::p_load(forecast,quantmod, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
               gridExtra, tseries, lmtest, FinTS, mgarchBEKK, ccgarch, xtable, MTS)

DJI <- getSymbols("DJI", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)
DJI_adj <- DJI$DJI.Adjusted
#eGARCH, ARIMA (1,0,0) sstd
GSPC <- getSymbols("^GSPC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)
GSPC_adj <- GSPC$GSPC.Adjusted
#eGARCH, ARIMA (0,0,1) sstd
IXIC <- getSymbols("^IXIC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)
IXIC_adj <- IXIC$IXIC.Adjusted
#eGARCH,ARIMA (0,0,1) sstd


BTC <- getSymbols("BTC-USD", src = "yahoo", auto.assign = FALSE)
BTC_adj <- BTC$`BTC-USD.Adjusted`
#sGARCH, ARIMA(1,0,0) sged
ETH <- getSymbols("ETH-USD", src = "yahoo", auto.assign = FALSE)
ETH_adj <- ETH$`ETH-USD.Adjusted`
#eGARCH, ARIMA(1,1,1) sged
XRP <- getSymbols("XRP-USD", src = "yahoo", auto.assign = FALSE)
XRP_adj <- XRP$`XRP-USD.Adjusted`
#sGARCH, ARIMA (1,0,3) sged

ret_DJI <- dailyReturn(DJI_adj, type = "log")
ret_GSPC <- dailyReturn(GSPC_adj, type = "log")
ret_IXIC <- dailyReturn(IXIC_adj, type = "log")
ret_BTC <- dailyReturn(BTC_adj, type = "log")
ret_ETH <- dailyReturn(ETH_adj, type = "log")
ret_XRP <- dailyReturn(XRP_adj, type = "log")


merge_total <- na.omit(merge(ret_DJI, ret_GSPC, ret_IXIC, ret_BTC, ret_ETH, ret_XRP))
colnames(merge_total) <- c('DJI', 'GSPC', 'IXIC', 'BTC', 'ETH', 'XRP')

rDJI_rBTC <- merge(ret_DJI, ret_BTC, join = "right")
rDJI_rETH <- merge(ret_DJI, ret_ETH, join = 'inner')
rDJI_rXRP <- merge(ret_DJI, ret_XRP, join = 'inner')

rGSPC_rBTC <- merge(ret_GSPC, ret_BTC, join = 'inner')
rGSPC_rETH <- merge(ret_GSPC, ret_ETH, join = 'inner')
rGSPC_rXRP <- merge(ret_GSPC, ret_XRP, join = 'inner')

rIXIC_rBTC <- merge(ret_IXIC, ret_BTC, join = 'inner')
rIXIC_rETH <- merge(ret_IXIC, ret_ETH, join = 'inner')
rIXIC_rXRP <- merge(ret_IXIC, ret_XRP, join = 'inner')


grangertest(daily.returns ~ daily.returns.1, order = 2, data = rDJI_rBTC) #2
grangertest(daily.returns ~ daily.returns.1, order = 4, data = rDJI_rETH) #2 e 4
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rDJI_rXRP) #2 é o melhor 0.1871 não causal, encontrar alternativa
grangertest(daily.returns ~ daily.returns.1, order = 2, data = rGSPC_rBTC) #2+



model.DJI.ar = arima(ret_DJI, order = c(1,0,0))
ret_DJI_BTC_res <- merge(rDJI_rBTC,model.DJI.ar$residuals, join = 'left')
model.DJI_BTC.arima = auto.arima(ret_DJI_BTC_res$daily.returns, seasonal = FALSE, xreg = c(ret_DJI_BTC_res$daily.returns.1, ret_DJI_BTC_res$model.DJI.ar.residuals))



model.DJI.spec = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(1 , 0, 0), include.mean = TRUE), distribution.model = "sstd")

(model.DJI.fit = ugarchfit(spec = model.DJI.spec , data = ret_DJI , solver = 'solnp'))


model.BTC.fit

