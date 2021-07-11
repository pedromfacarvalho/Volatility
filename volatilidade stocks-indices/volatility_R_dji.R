pacman::p_load(forecast, factoextra, readxl, caret, uroot, quantmod, tidyquant, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
               gridExtra, tseries, lmtest, FinTS, mgarchBEKK, ccgarch, xtable, MTS, plm,zoo)

# tirar os dados e fazer back e forward fill
AAPL <- na.locf(na.locf(getSymbols("AAPL", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
AAPL_adj <- xts(AAPL$AAPL.Adjusted)
#plot(AAPL_adj)
MSFT <- na.locf(na.locf(getSymbols("MSFT", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
MSFT_adj <- MSFT$MSFT.Adjusted
#plot(MSFT_adj)
V <- na.locf(na.locf(getSymbols("V", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
V_adj <- V$V.Adjusted
#plot(V_adj)
JPM <- na.locf(na.locf(getSymbols("JPM", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
JPM_adj <- JPM$JPM.Adjusted
#plot(JPM_adj)
JNJ <- na.locf(na.locf(getSymbols("JNJ", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
JNJ_adj <- JNJ$JNJ.Adjusted
#plot(JNJ_adj)
WMT <- na.locf(na.locf(getSymbols("WMT", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
WMT_adj <- WMT$WMT.Adjusted
#plot(WMT_adj)
UNH <- na.locf(na.locf(getSymbols("UNH", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
UNH_adj <- UNH$UNH.Adjusted
#plot(UNH_adj)
PG <- na.locf(na.locf(getSymbols("PG", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
PG_adj <- PG$PG.Adjusted
#plot(PG_adj)
DIS <- na.locf(na.locf(getSymbols("DIS", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
DIS_adj <- DIS$DIS.Adjusted
#plot(DIS_adj)
HD <- na.locf(na.locf(getSymbols("HD", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
HD_adj <- HD$HD.Adjusted
#plot(HD_adj)
VZ <- na.locf(na.locf(getSymbols("VZ", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
VZ_adj <- VZ$VZ.Adjusted
#plot(VZ_adj)
INTC <- na.locf(na.locf(getSymbols("INTC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
INTC_adj <- INTC$INTC.Adjusted
#plot(INTC_adj)

# fazer log returns dos dados
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

# por os dados em matriz e fazer scaling
data <- na.locf(na.locf(cbind(ret_AAPL, ret_MSFT, ret_V, ret_JPM, ret_JNJ, ret_WMT, ret_UNH, ret_PG, ret_DIS, ret_HD, ret_VZ, ret_INTC)*1000,fromLast = TRUE))
colnames(data) <- c("ret_AAPL", "ret_MSFT", "ret_V", "ret_JPM", "ret_JNJ", "ret_WMT", "ret_UNH", "ret_PG", "ret_DIS", "ret_HD", "ret_VZ", "ret_INTC")

# função que tem o modelo ARMA-GARCH
models <- function(data, stock_order, external = NULL) {
  model.index.spec <- ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = stock_order, include.mean = TRUE, external.regressors = external), distribution.model = "sstd")

  (model.index.fit <- ugarchfit(spec = model.index.spec , data = data, solver = 'hybrid'))
}

lag_data <- function(data, column_to_remove, lag) {

  removed_data <- data[,-column_to_remove]
  rows <- (length(removed_data[,1])-(lag-1)):length(removed_data[,1])
  removed_row <- removed_data[-rows,]
  surplus <- coredata(removed_data[1,])
  if(lag > 1){
    for(i in 1:(lag-1)){
      surplus <- rbind(surplus,coredata(removed_data[1,]))
    }
  }
  exog_var <- rbind(surplus,coredata(removed_row))
}

the_exog_variables <- matrix(nrow = 12, ncol = 3)
the_exog_variables[1,] <- c("ret_HD_3", "ret_DIS", "ret_HD")
the_exog_variables[2,] <- c("ret_HD_3", "ret_HD", "ret_DIS")
the_exog_variables[3,] <- c("ret_HD_3", "ret_HD", "ret_DIS")
the_exog_variables[4,] <- c("ret_HD_3", "ret_DIS", "ret_HD")
the_exog_variables[5,] <- c("ret_HD_3", "ret_HD", "ret_DIS")
the_exog_variables[6,] <- c("ret_HD_3", "ret_DIS", "ret_HD")
the_exog_variables[7,] <- c("ret_HD_3", "ret_HD", "ret_DIS")
the_exog_variables[8,] <- c("ret_HD_3", "ret_HD", "ret_DIS")
the_exog_variables[9,] <- c("ret_HD_3", "ret_HD", "ret_AAPL")
the_exog_variables[10,] <- c("ret_DIS", "ret_PG_7", "ret_AAPL")
the_exog_variables[11,] <- c("ret_HD_3", "ret_HD", "ret_DIS")
the_exog_variables[12,] <- c("ret_HD_3", "ret_HD", "ret_DIS")
# tirar o modelo ARIMA dos dados

#models_garch_auto <- matrix(0,nrow = 12, ncol = 2)
models_garch_auto <- list()
for(i in 1:12) {
  exog_var <- lag_data(data, i, lag = 1)
  for(j in 2:7){
    temp <- lag_data(data, i, lag = j)
    temp_names <- colnames(temp)
    for(z in 1:11){
      temp_names[z] <- paste(temp_names[z],"_",toString(j),sep="")
    }
    colnames(temp) <- temp_names
    exog_var <- cbind(exog_var, temp)
  }
  used_variables <- exog_var[,the_exog_variables[i,]]
  temp <- auto.arima(data[,i],xreg = used_variables, stationary = TRUE, seasonal = TRUE)
  temp
  #print(temp)
  models_garch_auto<-c(models_garch_auto,list(temp))
}
#models_garch <- vector(mode = "list", 12)

#fit do modelo
#for(i in 1:12){
#  exog_var <- lag_data(data, i)
#  if(i != 3){
#
#    models_garch[i] <- models(data[,i], stock_order = c(models_garch_auto[i,]), external = exog_var)
#  } else {
#    models_garch[i] <- models(na.locf(na.locf(data[,i]),fromLast = TRUE), stock_order = c(models_garch_auto[i,]), external = exog_var)
#  }
#}

#vector de residuos
resid_vec <- matrix(nrow = length(ret_AAPL), ncol = 12)
for(i in 1:12){
  resid_vec[,i] <- resid(models_garch_auto[[i]])
}
var(resid_vec)
#dados de DJI
DJI <- na.locf(na.locf(getSymbols("DJI", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
DJI_adj <- xts(DJI$DJI.Adjusted)*1000
plot(DJI_adj)

#log returns de DJI
ret_DJI <- dailyReturn(DJI_adj, type = "log")*1000

model_ind_auto <- auto.arima(ret_DJI, xreg = resid_vec, stationary = TRUE)
resid_ind <- residuals(model_ind_auto)
resid_vec_with_index <- matrix(nrow = length(ret_AAPL), ncol = 13)
resid_vec_with_index[,1:12] <- resid_vec
resid_vec_with_index[,13] <- resid_ind
marginal_test <- diag(13)
for(i in 1:13){
  marginal_test[i,i] <- var(resid_vec_with_index[,i])
}
marginal_var <- var(resid_vec_with_index)
H_2 <- cov2cor(vcov(model_ind_auto))
phi <- diag(13)
for(i in 1:12){
  phi[13,i] <- coef(model_ind_auto)[i+1]
}
#matriz da variancia marginal
H <- phi%*%marginal_var
H <- H%*%t(phi)
principal <- princomp(resid_vec)

###########################################################

# verificar ARMA(1,0) para DJI
model_DJI <- ARIMA(ret_DJI, order = c(0,0,1), xreg = resid_vec)
(model_DJI)
model_DJI_auto <- auto.arima(ret_DJI)

# fit do modelo para DJI com o os residuos como regressores externos
model_DJI_garch <- models(ret_DJI, c(1,1), external = resid_vec)
cond_var_DJI <- sigma(model_DJI_garch)

# adicionar o residuo de DJI ao vector de resíduos
resid_vec_with_index <- matrix(nrow = length(ret_AAPL), ncol = 13)
resid_vec_with_index[,1:12] <- resid_vec
resid_vec_with_index[,13] <- residuals(model_DJI_garch)

# vectores de variancia e somatório das variancias
sigma_vec <- matrix(nrow = length(ret_AAPL), ncol = 12)
cond_var_sum <- matrix(0, nrow=length(ret_AAPL), ncol = 1)

sigma_vec_total <- matrix(nrow = length(ret_AAPL), ncol = 13)

for(i in 1:12){
  sigma_vec[,i] <- sigma(models_garch[[i]])
  cond_var_sum <- cond_var_sum + sigma_vec[,i]^2
  sigma_vec_total[,i] <- sigma_vec[,i]^2
}

sigma_vec_total[,13] <- cond_var_DJI^2

#matriz diagonal com os coeficientes do modelo DJI
phi <- diag(13)
#for(i in 1:12){
#  phi[13,i] <- coef(model_DJI_garch)[i+2]
#}
#
#H <- array(NA,dim=c(13,13,length(ret_AAPL)))
##matriz da variancia condicional
#for(i in seq(1:length(ret_AAPL))){
#  H[,,i] <- phi*sigma_vec_total[i,]*t(phi)
#}
# t.tests para as covariancias


marginal_var <- var(resid_vec_with_index)

for(i in 1:12){
  phi[13,i] <- coef(model_DJI_garch)[i+2]
}
#matriz da variancia marginal
H <- phi%*%marginal_var%*%t(phi)

#determinação dos spillovers
h <- sqrt(cond_var_sum + cond_var_DJI^2)
spillovers <- matrix(nrow = length(ret_AAPL), ncol = 12)
for(i in 1:12){
  spillovers[,i] <- (coef(model_DJI_garch)[i+2]*sigma_vec[,i])/h
}

#graficos
plot(spillovers[,4], type = "l")

