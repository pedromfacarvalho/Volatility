

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

rDJI_rBTC <- merge(ret_DJI, ret_BTC, join = "inner")
rDJI_rETH <- merge(ret_DJI, ret_ETH, join = 'inner')
rDJI_rXRP <- merge(ret_DJI, ret_XRP, join = 'inner')

rGSPC_rBTC <- merge(ret_GSPC, ret_BTC, join = 'inner')
rGSPC_rETH <- merge(ret_GSPC, ret_ETH, join = 'inner')
rGSPC_rXRP <- merge(ret_GSPC, ret_XRP, join = 'inner')

rIXIC_rBTC <- merge(ret_IXIC, ret_BTC, join = 'inner')
rIXIC_rETH <- merge(ret_IXIC, ret_ETH, join = 'inner')
rIXIC_rXRP <- merge(ret_IXIC, ret_XRP, join = 'inner')


model.DJI.spec = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(1 , 0, 0), include.mean = TRUE), distribution.model = "sstd")

(model.DJI.fit = ugarchfit(spec = model.DJI.spec , data = ret_DJI , solver = 'solnp'))











model.GSPC.spec = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(0 , 0, 1), include.mean = TRUE), distribution.model = "sstd")

(model.GSPC.fit = ugarchfit(spec = model.GSPC.spec , data = ret_GSPC , solver = 'solnp'))


model.IXIC.spec = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(0 , 0, 1), include.mean = TRUE), distribution.model = "sstd")

(model.IXIC.fit = ugarchfit(spec = model.IXIC.spec , data = ret_IXIC , solver = 'solnp'))


model.BTC.spec = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(2 , 0, 2), include.mean = TRUE), distribution.model = "sged")

(model.BTC.fit = ugarchfit(spec = model.BTC.spec , data = ret_BTC , solver = 'solnp'))

model.ETH.spec = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(2 , 2, 0), include.mean = TRUE), distribution.model = "sged")

(model.ETH.fit = ugarchfit(spec = model.ETH.spec , data = ret_ETH , solver = 'solnp'))

model.XRP.spec = ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(1 , 0, 3), include.mean = TRUE), distribution.model = "sged")

(model.XRP.fit = ugarchfit(spec = model.XRP.spec , data = ret_BTC , solver = 'solnp'))

#merg_est <- na.omit(merge(sigma(model.DJI.fit),sigma(model.GSPC.fit),sigma(model.IXIC.fit),sigma(model.BTC.fit),
#                  sigma(model.ETH.fit),sigma(model.XRP.fit)))








model.bekk1 <- BEKK(rDJI_rXRP)
model.bekk <- BEKK11(rDJI_rXRP, include.mean = TRUE, cond.dist = "normal")

###############################

## Simulate a BEKK process:
#simulated <- simulateBEKK(2,400, c(1,1), params = NULL)


## Prepare the input for the estimation process:
#simulated1 <- do.call(cbind, simulated$eps)

## Estimate with default arguments:
#stimated <- BEKK(simulated1)




## Show diagnostics:
#diagnoseBEKK(estimated)

## Likewise, you can estimate an mGJR process:
#estimated2 <- mGJR(simulated[,1], simulated[,2])

##########################################
#DJI vs BTC
uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(1 , 0, 0), include.mean = TRUE), distribution.model = "sstd"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(2 , 0, 2), include.mean = TRUE), distribution.model = "sged")))

(multf = multifit(uspec.n, rDJI_rBTC))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = dccfit(spec1, data = rDJI_rBTC, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_DJI_BTC <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_DJI_BTC <- as.xts(cor_DJI_BTC)  # imposes the xts time series format - useful for plotting

plot(cor_DJI_BTC)








#DJI vs ETH


uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1,1)),
                                     mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE), distribution.model = 'sged'),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(2 , 2, 0), include.mean = TRUE), distribution.model = "sged")))

(multf = multifit(uspec.n, rDJI_rETH))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = dccfit(spec1, data = rDJI_rETH, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_DJI_ETH <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_DJI_ETH <- as.xts(cor_DJI_ETH)  # imposes the xts time series format - useful for plotting

plot(cor_DJI_ETH)


#DJI vs XRP

uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1,1)),
                                     mean.model = list(armaOrder = c(1,0,0), include.mean = TRUE), distribution.model = 'sstd'),
                         ugarchspec(variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                                     mean.model = list(armaOrder = c(1,0,3), include.mean = TRUE), distribution.model = 'sged')))
(multf = multifit(uspec.n, rDJI_rXRP))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = dccfit(spec1, data = rDJI_rXRP, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_DJI_XRP <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_DJI_XRP <- as.xts(cor_DJI_XRP)  # imposes the xts time series format - useful for plotting

plot(cor_DJI_XRP)

#GSPC vs BTC

uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(0 , 0, 1), include.mean = TRUE), distribution.model = "sstd"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(2 , 0, 2), include.mean = TRUE), distribution.model = "sged")))

(multf = multifit(uspec.n, rGSPC_rBTC))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvt')
(fit1 = dccfit(spec1, data = rGSPC_rBTC, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_GSPC_BTC <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_GSPC_BTC <- as.xts(cor_GSPC_BTC)  # imposes the xts time series format - useful for plotting

plot(cor_GSPC_BTC)

#GSPC vs ETH
uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(0 , 0, 1), include.mean = TRUE), distribution.model = "sstd"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(1 , 0, 3), include.mean = TRUE), distribution.model = "sged")))

(multf = multifit(uspec.n, rGSPC_rETH))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvt')
(fit1 = dccfit(spec1, data = rGSPC_rETH, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_GSPC_ETH <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_GSPC_ETH <- as.xts(cor_GSPC_ETH)  # imposes the xts time series format - useful for plotting

plot(cor_GSPC_ETH)

#GSPC vs XRP

uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(0 , 0, 1), include.mean = TRUE), distribution.model = "sged"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(1 , 0, 3), include.mean = TRUE), distribution.model = "sged")))

(multf = multifit(uspec.n, rGSPC_rXRP))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = dccfit(spec1, data = rGSPC_rXRP, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_GSPC_XRP <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_GSPC_XRP <- as.xts(cor_GSPC_XRP)  # imposes the xts time series format - useful for plotting

plot(cor_GSPC_XRP)

#IXIC vs BTC
uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(0 , 0, 1), include.mean = TRUE), distribution.model = "sged"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(2 , 0, 2), include.mean = TRUE), distribution.model = "sged")))

(multf = multifit(uspec.n, rIXIC_rBTC))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = dccfit(spec1, data = rIXIC_rBTC, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_IXIC_BTC <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_IXIC_BTC <- as.xts(cor_IXIC_BTC)  # imposes the xts time series format - useful for plotting

plot(cor_IXIC_BTC)

#IXIC vs ETH
uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(0 , 0, 1), include.mean = TRUE), distribution.model = "sstd"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(2 , 2 ,0), include.mean = TRUE), distribution.model = "sged")))

(multf = multifit(uspec.n, rIXIC_rETH))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = dccfit(spec1, data = rIXIC_rETH, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_IXIC_ETH <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_IXIC_ETH <- as.xts(cor_IXIC_ETH)  # imposes the xts time series format - useful for plotting

plot(cor_IXIC_ETH)
#IXIC vs XRP
uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(0 , 0, 1), include.mean = TRUE), distribution.model = "sged"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(1 , 0, 3), include.mean = TRUE), distribution.model = "sged")))

(multf = multifit(uspec.n, rIXIC_rXRP))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = dccfit(spec1, data = rIXIC_rXRP, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
cor1[,,dim(cor1)[3]]
cor_IXIC_XRP <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_IXIC_XRP <- as.xts(cor_IXIC_XRP)  # imposes the xts time series format - useful for plotting

plot(cor_IXIC_XRP)


#ALL OFF THEM

uspec.n = multispec(list(ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(1 , 0, 0), include.mean = TRUE), distribution.model = "sstd"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(0 , 0, 1), include.mean = TRUE), distribution.model = "sstd"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(0 , 0, 1), include.mean = TRUE), distribution.model = "sstd"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(2 , 0, 2), include.mean = TRUE), distribution.model = "sged"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(2 , 2, 0), include.mean = TRUE), distribution.model = "sged"),
                         ugarchspec(variance.model = list(model = 'eGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = c(1 , 0, 3), include.mean = TRUE), distribution.model = "sged")))

(multf = multifit(uspec.n, merge_total))
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = dccfit(spec1, data = merge_total, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix
dim(cor1)
#cor1[,,dim(cor1)[3]]
#cor_total <- as.xts(cor_total)  # imposes the xts time series format - useful for plotting
DJI_BTC <- cov1[4,1,]
DJI_BTC <- as.xts(DJI_BTC)
plot(DJI_BTC)
IXIC_ETH <- cor1[5,3,]
IXIC_ETH <- as.xts(IXIC_ETH)
plot(IXIC_ETH)
GSPC_XRP <- cor1[2,6,]
GSPC_XRP <- as.xts(GSPC_XRP)
plot(GSPC_XRP)

spec1 = cccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')
(fit1 = cccfit(spec1, data = merge_total, fit.control = list(eval.se = TRUE), fit = multf))
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix

DJI_BTC <- cov1[4,1,]
DJI_BTC <- as.xts(DJI_BTC)
plot(DJI_BTC)