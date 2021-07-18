# Title     : TODO
# Objective : TODO
# Created by: m20190417
# Created on: 4/6/2021

pacman::p_load(BatchGetSymbols, forecast,quantmod, randomForest, mlbench, caret, car, rugarch, rmgarch,coinmarketcapr,xts, tidyverse, ggthemes,
               gridExtra, tseries, lmtest, FinTS, mgarchBEKK, xtable, MTS, plm)

GetSP500Stocks()
# tirar os dados e fazer back e forward fill
AAPL <- na.locf(na.locf(getSymbols("AAPL", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
AAPL_adj <- xts(AAPL$AAPL.Adjusted)
plot(AAPL_adj)
MSFT <- na.locf(na.locf(getSymbols("MSFT", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
MSFT_adj <- MSFT$MSFT.Adjusted
plot(MSFT_adj)
V <- na.locf(na.locf(getSymbols("V", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
V_adj <- V$V.Adjusted
plot(V_adj)
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
data <- na.locf(na.locf(cbind(ret_AAPL, ret_MSFT, ret_V, ret_JPM),fromLast= TRUE))#, ret_JNJ, ret_WMT, ret_UNH, ret_PG, ret_DIS, ret_HD, ret_VZ, ret_INTC)*1000,fromLast = TRUE))
colnames(data) <- c("ret_AAPL", "ret_MSFT", "ret_V", "ret_JPM")#, "ret_JNJ", "ret_WMT", "ret_UNH", "ret_PG", "ret_DIS", "ret_HD", "ret_VZ", "ret_INTC")


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
#
#for(i in 1:4){#12){
#
#  exog_var <- lag_data(data, i, lag = 1)
#  for(j in 2:4){
#    temp <- lag_data(data, i, lag = j)
#    temp_names <- colnames(temp)
#    for(z in 1:3){
#      temp_names[z] <- paste(temp_names[z],"_",toString(j),sep="")
#    }
#    colnames(temp) <- temp_names
#    exog_var <- cbind(exog_var, temp)
#  }
#  temp_data <- data[,1]
#  colnames(temp_data) <- "target"
#  data_for_selection <- cbind(temp_data, exog_var)
#
#  control <- trainControl(method="repeatedcv", number=10, repeats=3)
#  # train the model
#  model <- caret::train(form = target~., data=data_for_selection, method="lm", trControl=control)
#  # estimate variable importance
#  importance <- varImp(model, scale=FALSE)
#  # summarize importance
#  print(importance)
#
#  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
#  # run the RFE algorithm
#  results <- rfe(form = target~., data = data_for_selection, sizes = c(1:4), rfeControl=control)
#  # summarize the results
#  print(results)
#  # list the chosen features
#  predictors(results)
#  # plot the results
#  plot(results, type=c("g", "o"))
#}
#model <- lm(target~., data = data_for_selection)
#Anova(model)

models_garch_auto <- list()
for(i in 1:4) {
  exog_var <- lag_data(data, i, lag = 1)
  for(j in 2:100){
    temp <- lag_data(data, i, lag = j)
    temp_names <- colnames(temp)
    for(z in 1:3){
      temp_names[z] <- paste(temp_names[z],"_",toString(j),sep="")
    }
    colnames(temp) <- temp_names
    exog_var <- cbind(exog_var, temp)
  }
  temp <- auto.arima(data[,i],xreg = exog_var, seasonal = FALSE, stationary = TRUE)
  print("fiz modelo")
  #print(temp)
  models_garch_auto<-c(models_garch_auto,list(temp))
}
resid_vec <- matrix(nrow = length(ret_AAPL), ncol = 4)
for(i in 1:4){
  resid_vec[,i] <- resid(models_garch_auto[[i]])
}
cor(resid_vec)

models_auto_base <- list()
models_linear <- list()
for(i in 1:4){
  modelo <- auto.arima(data[,i])
  models_auto_base <- c(models_auto_base, list(modelo))
  exog_var <- lag_data(data, i, lag = 1)
  for(j in 2:8){
    temp <- lag_data(data, i, lag = j)
    temp_names <- colnames(temp)
    for(z in 1:3){
      temp_names[z] <- paste(temp_names[z],"_",toString(j),sep="")
    }
    colnames(temp) <- temp_names
    exog_var <- cbind(exog_var, temp)
  }
  exog_var<- cbind(resid(modelo), exog_var)
  models_linear <- c(models_linear, list(lm(resid(modelo)~., data = exog_var)))
}
resid_vec_base <- matrix(nrow = length(ret_AAPL), ncol = 4)
for(i in 1:4){
  resid_vec_base[,i] <- resid(models_linear[[i]])
}
var(resid_vec_base)



models_garch_auto <- list()
for(i in 1:4) {
  exog_var <- lag_data(data, i, lag = 1)
  for(j in 2:3){
    temp <- lag_data(data, i, lag = j)
    temp_names <- colnames(temp)
    for(z in 1:3){
      temp_names[z] <- paste(temp_names[z],"_",toString(j),sep="")
    }
    colnames(temp) <- temp_names
    exog_var <- cbind(exog_var, temp)
  }

  temp <- auto.arima(data[,i], xreg = exog_var, seasonal = FALSE)
  print("fiz modelo")
  #print(temp)
  models_garch_auto<-c(models_garch_auto,list(temp))
}
resid_vec <- matrix(nrow = length(ret_AAPL), ncol = 4)
for(i in 1:4){
  resid_vec[,i] <- resid(models_garch_auto[[i]])
}
var(resid_vec)


DJI <- na.locf(na.locf(getSymbols("DJI", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
DJI_adj <- xts(DJI$DJI.Adjusted)
plot(DJI_adj)

#log returns de DJI
ret_DJI <- dailyReturn(DJI_adj, type = "log")

model_ind_auto <- auto.arima(ret_DJI, xreg = resid_vec, stationary = TRUE)
resid_ind <- residuals(model_ind_auto)
resid_vec_with_index <- matrix(nrow = length(ret_AAPL), ncol = 5)
resid_vec_with_index[,1:4] <- resid_vec
resid_vec_with_index[,5] <- resid_ind
marginal_test <- var(resid_vec_with_index[,i])
for(i in 1:5){
  marginal_test[i,i] <- var(resid_vec_with_index[,i])
}
marginal_var <- var(resid_vec_with_index)
H_2 <- cov2cor(vcov(model_ind_auto))
phi <- diag(5)
for(i in 1:4){
  phi[5,i] <- coef(model_ind_auto)[i+2]
}
H <- phi%*%marginal_var
H <- H%*%t(phi)





load <- prcomp(exog_var)$rotation
  exog_var_rotated <- exog_var%*%load
models_garch_auto[1]
plot(, type = 'l')
