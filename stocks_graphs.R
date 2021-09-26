# Title     : TODO
# Objective : TODO
# Created by: C097564
# Created on: 2021-09-19

pacman::p_load(BatchGetSymbols, gogarch, vars, keras, rmgarch, doParallel, aTSA, reshape2, purrr, LSTS, fDMA, tidyquant, data.table, pls, dynlm, stats, ggplot2, ggfortify, plotly, zoo, TSA, gridExtra, GeneCycle, qrmdata,XML,quantmod,zoo,chron, rmgarch, rugarch, rvest, dplyr, FinTS, forecast, car, fpp2, lmtest,tseries,stats, seastests)

load("model_sarima_GSPC.Rdata")
load("model_sarima_NDX_gjrGARCH.Rdata")
load("model_index_NDX_gjrGARCH_index.Rdata")
test <- residuals(models_sarima[[1]])
sigma <- matrix(nrow = length(test), ncol = length(models_sarima))
for(i in 1:length(models_sarima)){
  sigma[,i] <- sigma(models_sarima[[i]])
}

load("model_index_DJI_sGARCH.Rdata")

sigma_index <- sigma(temp)
coefs <- coef(temp)[3:98]

h <- coefs[1]^2 * sigma[,1]^2
for (i in 2:length(models_sarima)){
  h <- h+(coefs[i]^2 * sigma[,i]^2)
}
h <- h+sigma_index^2



cond_corr <- matrix(nrow = length(test), ncol = length(models_sarima))
for (i in 1:length(models_sarima)){
  cond_corr[,i] <- (coefs[i]*sigma[,i])/sqrt(h)
}



plot(x = as.Date(dates), cond_corr[,1], ylim = c(0,1), type = 'l')
for (i in 2:length(models_sarima)){
  lines(x = as.Date(dates), cond_corr[,i], type = 'l')
}


VR <- matrix(nrow = length(test), ncol = length(models_sarima))
for (i in 1:length(models_sarima)){
  VR[,i] <- (coefs[i]^2 * sigma[,i]^2)/h
}


