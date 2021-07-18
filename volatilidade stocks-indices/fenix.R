pacman::p_load(BatchGetSymbols, zoo, qrmdata,XML,quantmod,zoo,chron, rmgarch, rugarch, rvest, dplyr, FinTS, forecast, car, lmtest,tseries,stats, seastests)


scrape <- read_html('https://www.slickcharts.com/nasdaq100')
tickers_DJI <- c("MMM","AXP","AMGN","AAPL","BA","CAT","CVX","CSCO","KO","DOW","GS","HD","HON","IBM","INTC","JNJ","JPM","MCD","MRK","MSFT","NKE","PG","CRM","TRV","UNH","VZ","V","WMT","WBA","DIS")

table_url <- (scrape %>% html_table(fill = TRUE))[[1]]
tickers_NDX <- c(table_url["Symbol"])[[1]]
tickers_SP500 <- c(GetSP500Stocks()["Tickers"])[[1]]


DJI_const <- BatchGetSymbols(tickers = tickers_DJI, thresh.bad.data = 0, first.date = "2018-01-01", freq.data = "daily", type.return = 'log')
NDX_const <- BatchGetSymbols(tickers = tickers_NDX, thresh.bad.data = 0, first.date = "2018-01-01", freq.data = "daily", type.return = 'log')
SP500_const <- BatchGetSymbols(tickers = tickers_SP500, thresh.bad.data = 0, first.date = "2018-01-01", freq.data = "daily", type.return = 'log')

DJI_const <- DJI_const[[2]]
DJI_const_adj <- select(DJI_const, ticker, ret.adjusted.prices, ref.date)
NDX_const <- NDX_const[[2]]
NDX_const_adj <- select(NDX_const, ticker, ret.adjusted.prices, ref.date)
SP500_const <- SP500_const[[2]]
SP500_const_adj <- select(SP500_const, ticker, ret.adjusted.prices, ref.date)
index <- BatchGetSymbols(tickers = "^DJI" , thresh.bad.data = 0, first.date = "2018-01-01", freq.data = "daily", type.return = 'log')
indexes <- index[[2]]
DJI_adj <- select(indexes, ticker, ret.adjusted.prices,ref.date)
index <- BatchGetSymbols(tickers =  "^GSPC" , thresh.bad.data = 0, first.date = "2018-01-01", freq.data = "daily", type.return = 'log')
indexes <- index[[2]]
GSPC_adj <- select(indexes, ticker, ret.adjusted.prices,ref.date)
index <- BatchGetSymbols(tickers = "^NDX" , thresh.bad.data = 0, first.date = "2018-01-01", freq.data = "daily", type.return = 'log')
indexes <- index[[2]]
NDX_adj <- select(indexes, ticker, ret.adjusted.prices,ref.date)


stock_data_DJI <- data.frame(Date = DJI_adj$ref.date)
for(i in 1:length(tickers_DJI)){
  stock_name <- tickers_DJI[i]
  temp <- data.frame(row.names = DJI_const_adj[DJI_const_adj[,"ticker"] == stock_name, "ref.date"])
  temp[c(stock_name, "Date")] <- DJI_const_adj[DJI_const_adj[,"ticker"] == stock_name, c("ret.adjusted.prices","ref.date")]
  stock_data_DJI <- left_join(stock_data_DJI, temp, by.y = "Date", by.x = "Date", keep = FALSE)
}


stock_data_GSPC <- data.frame(Date = GSPC_adj$ref.date)
for(i in 1:length(tickers_SP500)){
  stock_name <- tickers_SP500[i]
  temp <- data.frame(row.names = SP500_const_adj[SP500_const_adj[,"ticker"] == stock_name, "ref.date"])
  temp[c(stock_name, "Date")] <- SP500_const_adj[SP500_const_adj[,"ticker"] == stock_name, c("ret.adjusted.prices","ref.date")]
  stock_data_GSPC <- left_join(stock_data_GSPC, temp, by.y = "Date", by.x = "Date", keep = FALSE)
}


stock_data_NDX <- data.frame(Date = NDX_adj$ref.date)
for(i in 1:length(tickers_NDX)){
  stock_name <- tickers_NDX[i]
  temp <- data.frame(row.names = NDX_const_adj[NDX_const_adj[,"ticker"] == stock_name, "ref.date"])
  temp[c(stock_name, "Date")] <- NDX_const_adj[NDX_const_adj[,"ticker"] == stock_name, c("ret.adjusted.prices","ref.date")]
  stock_data_NDX <- left_join(stock_data_NDX, temp, by.y = "Date", by.x = "Date", keep = FALSE)
}

DJI_stocks <- stock_data_DJI[names(stock_data_DJI)[-1]]
GSPC_stocks <- stock_data_GSPC[names(stock_data_GSPC)[-1]]
NDX_stocks <- stock_data_NDX[names(stock_data_NDX)[-1]]

data <- na.locf(na.locf(ts(DJI_stocks[-1,]), fromLast = TRUE))
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

ar_maker <- function(exog_var) {
  cool_stuff <- exog_var
  resid_var <- exog_var
  var_names <- colnames(exog_var)
  for(i in 1:length(exog_var[1,])){
    model <- Arima(exog_var[,i], order = c(2,0,0))
    cool_stuff[,i] <- fitted(model)
    resid_var[,i] <- resid(model)
  }
  for(z in 1:(length(tickers_DJI)-1)){
      temp_names[z] <- paste(temp_names[z],"_","resid",sep="")
  }
  if()
  colnames(resid_var) <- temp_names
  wow <- cbind(cool_stuff, resid_var)
  wow
}


models_garch_auto <- list()
for(i in 1:length(data[1,])) {

  model_data <- data[,i]
  exog_var <- data[,-i]
  var_names <- colnames(exog_var)

  for(z in 1:1){

    for(j in 1:length(exog_var[1,])){
          if(z==1 & j==1){
              exog <- matrix(stats::lag(exog_var[,1], -1))
           } else{
                exog <- cbind(exog, stats::lag(exog_var[,j], -z)) %>% head(NROW(model_data))
        }
    }
  }
  #temp_names <- vector("list", length = length(var_names)*z)
  #for(p in 1:z){
  #  for(k in 1:(length(tickers_DJI)-1)){
  #      temp_names[k] <- paste(var_names[k],"_",toString(p),sep="")
  #    }
  #if(p == 1){
  #  exog_names <- temp_names
  #} else {
  #  exog_names <- c(exog_names, temp_names)
  #}
  #}
  #colnames(exog) <- exog_names
  #model_data <- cbind(model_data, stats::lag(data[,-i],-1)) %>% head(NROW(model_data))
  #exog <- model_data[,-1]
  #model_data <- model_data[,1]
  #exog <- stats::lag(data[,-i],-1)
  if(i==19){
    temp <- auto.arima(model_data, xreg = exog, seasonal = TRUE)
    urrr
  }
  temp <- arima(model_data, xreg = exog, include.mean = TRUE, order = c(1,0,0))
  print("fiz modelo")
  #print(temp)
  models_garch_auto<-cbind(models_garch_auto,list(temp))
  print(length(exog[1,])/z)
  rm(exog)
}
resid_vec <- matrix(nrow = length(resid(temp)), ncol = length(tickers_DJI))
for(i in 1:length(tickers_DJI)){
  resid_vec[,i] <- resid(models_garch_auto[[i]])
}
colnames(resid_vec)<-tickers_DJI
correlacoes <- cor(na.locf(na.locf(resid_vec), fromLast = TRUE))
lambda_cor <- coredata(correlacoes[2,1])
M <- 30
corr_sq <- matrix()
for(i in 3:M){
  for(j in 1:(i-1)){
    lambda_cor<-lambda_cor+(coredata(correlacoes[i,j])^2)
    corr_sq <- rbind(corr_sq,coredata(correlacoes[i,j]^2))

  }
}
lambda <- nrow(data)*lambda_cor
qchisq(.05, df=435)
lambda
sum_cor <- cbind(matrix(colSums(abs(correlacoes))), tickers_DJI)

sum_cor_sort <- sum_cor[order(sum_cor[,1], decreasing = TRUE),]

model_ind <- Arima(na.omit(DJI_adj$ret.adjusted.prices), xreg = resid_vec, order = c(1,0,0))

model_ind

uspec = ugarchspec(mean.model = list(armaOrder = c(1,0), include.mean = TRUE), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "sstd")
uspec2 = ugarchspec(mean.model = list(armaOrder = c(3,3), include.mean = TRUE), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "sstd")
spec1 = dccspec(uspec = multispec(list(uspec, uspec2)), dccOrder = c(1,1),  distribution = "mvnorm")
fit1 = dccfit(spec1, data = cbind(data[,4], data[,19]))
print(fit1)
plot(rcor(fit1)[1,2,], type = "l")
plot(data[,4])
plot(data[,19])

wo(data[,2])

models_auto_base <- list()
models_linear <- list()
for(i in 1:length(data[1,])){
  modelo <- auto.arima(data[,i])
  models_auto_base <- c(models_auto_base, list(modelo))
  #exog_var <- lag_data(data, i, lag = 1)
  #for(j in 2:30){
  #  temp <- lag_data(data, i, lag = j)
  #  temp_names <- colnames(temp)
  #  for(z in 1:(length(tickers_DJI)-1)){
  #    temp_names[z] <- paste(temp_names[z],"_",toString(j),sep="")
  #  }
  #  colnames(temp) <- temp_names
  #  exog_var <- cbind(exog_var, temp)
  #}
  #exog_var<- cbind(resid(modelo), exog_var)
  exog_var <- cbind(data[,i], stats::lag(data[,-i],-1))
  colnames(exog_var)[1] <- "y"
  models_linear <- c(models_linear, list(lm(y~., data = data.frame(exog_var[2:nrow(exog_var),]))))
}
resid_vec_base <- matrix(nrow = length(resid(models_linear[[1]])), ncol = length(data[1,]))
for(i in 1:length(data[1,])){
  resid_vec_base[,i] <- resid(models_linear[[i]])
}
correlacoes <- cor(resid_vec_base)

cor(cbind(fitted(models_garch_auto[[1]]), resid(models_garch_auto[[1]])))

jarque.bera.test(resid_vec[,3])



