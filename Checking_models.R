# Title     : TODO
# Objective : TODO
# Created by: pedro
# Created on: 04/09/20


pacman::p_load(forecast,quantmod, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
               gridExtra, tseries, lmtest, FinTS, mgarchBEKK)


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
#sGARCH, ARIMA(1,0,0) sged`
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

#determining qplots
thing = ret_ETH
jarque.bera.test(thing)
qplot(x = 1:length(thing), y = thing, geom = "line") + geom_line(color = 'yellow') +
  labs( x = '', y = 'Returns', title =  "DOW JONES") + geom_hline(yintercept = mean(thing), color = 'red')


p1 = qplot(x = 1:length(thing) , y = thing , geom = 'line') + geom_line(color = 'darkblue') +
    geom_hline(yintercept = mean(thing) , color = 'red' , size = 1) +
    labs(x = '' , y = 'Daily Returns')

p2 = qplot(thing , geom = 'density') + coord_flip() + geom_vline(xintercept = mean(thing) , color = 'red' , size = 1) +
    geom_density(fill = 'lightblue' , alpha = 0.4) + labs(x = '')

grid.arrange(p1 , p2 , ncol = 2)

adf.test(thing)

model.arima = auto.arima(thing)
model.arima

model.arima$residuals %>% ggtsdisplay(plot.type = 'hist' , lag.max = 14)

ar.res = model.arima$residuals
try = arima(thing, c(2,0,1))
Box.test(model.arima$residuals , lag = 14 , fitdf = 2 , type = 'Ljung-Box')
coeftest(try)
coeftest(model.arima)
model.arima = try

model_residuals <- residuals(model.arima)
ArchTest(model_residuals - mean(model_residuals))



model.spec = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(2 , 2, 0), include.mean = TRUE), distribution.model = "sged")

(model.fit = ugarchfit(spec = model.spec , data = thing , solver = 'solnp'))

