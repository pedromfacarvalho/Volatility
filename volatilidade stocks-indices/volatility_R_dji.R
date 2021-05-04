pacman::p_load(forecast,quantmod, tidyquant, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
               gridExtra, tseries, lmtest, FinTS, mgarchBEKK, ccgarch, xtable, MTS, plm,zoo)

AAPL <- na.locf(na.locf(getSymbols("AAPL", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
AAPL_adj <- xts(AAPL$AAPL.Adjusted)
plot(AAPL_adj)
#eGARCH, ARIMA (0,0,1) sstd
MSFT <- na.locf(na.locf(getSymbols("MSFT", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
MSFT_adj <- MSFT$MSFT.Adjusted
plot(MSFT_adj)
#eGARCH,ARIMA (0,0,1) sstd
V <- na.locf(na.locf(getSymbols("V", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
V_adj <- V$V.Adjusted
plot(V_adj)
#eGARCH,ARIMA (0,0,1) sstd
JPM <- na.locf(na.locf(getSymbols("JPM", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
JPM_adj <- JPM$JPM.Adjusted
plot(JPM_adj)
JNJ <- na.locf(na.locf(getSymbols("JNJ", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
JNJ_adj <- JNJ$JNJ.Adjusted
plot(JNJ_adj)
WMT <- na.locf(na.locf(getSymbols("WMT", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
WMT_adj <- WMT$WMT.Adjusted
plot(WMT_adj)
UNH <- na.locf(na.locf(getSymbols("UNH", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
UNH_adj <- UNH$UNH.Adjusted
plot(UNH_adj)
PG <- na.locf(na.locf(getSymbols("PG", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
PG_adj <- PG$PG.Adjusted
plot(PG_adj)
DIS <- na.locf(na.locf(getSymbols("DIS", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
DIS_adj <- DIS$DIS.Adjusted
plot(DIS_adj)
HD <- na.locf(na.locf(getSymbols("HD", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
HD_adj <- HD$HD.Adjusted
plot(HD_adj)
VZ <- na.locf(na.locf(getSymbols("VZ", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
VZ_adj <- VZ$VZ.Adjusted
plot(VZ_adj)
INTC <- na.locf(na.locf(getSymbols("INTC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
INTC_adj <- INTC$INTC.Adjusted
plot(INTC_adj)

ret_AAPL <- dailyReturn(AAPL_adj, type = "log")
ret_MSFT <- dailyReturn(MSFT_adj, type = "log")
ret_V <- dailyReturn(V_adj, type = "log")
ret_JPM <- dailyReturn(JPM_adj, type = "log")
ret_JNJ <- dailyReturn(JNJ_adj, type = "log")
ret_WMT <- dailyReturn(WMT_adj, type = "log")
ret_UNH <- dailyReturn(UNH_adj, type = "log")
ret_PG <- dailyReturn(PG_adj, type = "log")
ret_DIS <- dailyReturn(DIS_adj, type = "log")
ret_HD <- dailyReturn(HD_adj, type = "log")
ret_VZ <- dailyReturn(VZ_adj, type = "log")
ret_INTC <- dailyReturn(INTC_adj, type = "log")

data <- cbind(ret_AAPL, ret_MSFT, ret_V, ret_JPM, ret_JNJ, ret_WMT, ret_UNH, ret_PG, ret_DIS, ret_HD, ret_VZ, ret_INTC)*1000
models_data <- vector("list", length(data))
models <- function(data, stock_order, external = NULL) {
  model.index.spec <- ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = stock_order, include.mean = TRUE, external.regressors = external), distribution.model = "sstd")

  (model.index.fit <- ugarchfit(spec = model.index.spec , data = data, solver = 'hybrid'))
}

models_data <- auto.arima(ret_AAPL)


for(i in 1:12) {
  models_data[i] <- auto.arima(data[,i])
}
models_garch <- vector("list", 12)

models_garch[1] <- models(data[,1], c(2,0,2))
models_garch[2] <- models(data[,2], c(5,0,0))
models_garch[3] <- models(na.locf(na.locf(data[,3]),fromLast = TRUE), c(3,0,3))
models_garch[4] <- models(data[,4], c(3,0,4))
models_garch[5] <- models(data[,5], c(1,0,1))
models_garch[6] <- models(data[,6], c(0,0,2))
models_garch[7] <- models(data[,7], c(0,0,2))
models_garch[8] <- models(data[,8], c(3,0,5))
models_garch[9] <- models(data[,9], c(2,0,0))
models_garch[10] <- models(data[,10], c(5,0,0))
models_garch[11] <- models(data[,11], c(2,0,1))
models_garch[12] <- models(data[,12], c(3,0,3))


resid_vec <- matrix(nrow = length(ret_AAPL), ncol = 12)
for(i in 1:12){
  resid_vec[,i] <- residuals(models_garch[[i]])
}


DJI <- na.locf(na.locf(getSymbols("DJI", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
DJI_adj <- xts(DJI$DJI.Adjusted)*1000
plot(DJI_adj)

ret_DJI <- dailyReturn(DJI_adj, type = "log")

model_DJI <- ARIMA(ret_DJI, order = c(1,0,0), xreg = resid_vec)
(model_DJI)

model_DJI_garch <- models(ret_DJI, c(1,0,0), external = resid_vec)
cond_var_DJI <- sigma(model_DJI_garch)

resid_vec_with_index <- matrix(nrow = length(ret_AAPL), ncol = 13)
resid_vec_with_index[,1:12] <- resid_vec
resid_vec_with_index[,13] <- residuals(model_DJI_garch)

sigma_vec <- matrix(nrow = length(ret_AAPL), ncol = 12)
cond_var_sum <- matrix(0, nrow=length(ret_AAPL), ncol = 1)

sigma_vec_total <- matrix(nrow = length(ret_AAPL), ncol = 13)

for(i in 1:12){
  sigma_vec[,i] <- sigma(models_garch[[i]])
  cond_var_sum <- cond_var_sum + sigma_vec[,i]^2
  sigma_vec_total[,i] <- sigma_vec[,i]^2
}

sigma_vec_total[,13] <- cond_var_DJI^2

phi <- diag(13)
for(i in 1:12){
  phi[13,i] <- coef(model_DJI_garch)[i+2]
}

H <- array(NA,dim=c(13,13,length(ret_AAPL)))

for(i in seq(1:length(ret_AAPL))){
  H[,,i] <- phi*sigma_vec_total[i,]*t(phi)
}
H_scaled <- H*100000
t.test(cbind(H_scaled[2,1,]), mu = 0)

h <- sqrt(cond_var_sum + cond_var_DJI^2)
spillovers <- matrix(nrow = length(ret_AAPL), ncol = 12)
for(i in 1:12){
  spillovers[,i] <- (coef(model_DJI_garch)[i+2]*sigma_vec[,i])/h
}

plot(spillovers[,4], type = "l")

