pacman::p_load(BatchGetSymbols, aTSA, purrr, LSTS, fDMA, tidyquant, data.table, pls, dynlm, stats, ggplot2, ggfortify, plotly, zoo, TSA, gridExtra, GeneCycle, qrmdata,XML,quantmod,zoo,chron, rmgarch, rugarch, rvest, dplyr, FinTS, forecast, car, fpp2, lmtest,tseries,stats, seastests)


scrape <- read_html('https://www.slickcharts.com/nasdaq100')
tickers_DJI <- c("MMM","AXP","AMGN","AAPL","BA","CAT","CVX","CSCO","KO","DOW","GS","HD","HON","IBM","INTC","JNJ","JPM","MCD","MRK","MSFT","NKE","PG","CRM","TRV","UNH","VZ","V","WMT","WBA","DIS")

table_url <- (scrape %>% html_table(fill = TRUE))[[1]]
tickers_NDX <- c(table_url["Symbol"])[[1]]
tickers_SP500 <- c(GetSP500Stocks()["Tickers"])[[1]]


DJI_const <- BatchGetSymbols(tickers = tickers_DJI, first.date = "2018-01-01", freq.data = "daily", type.return = "log")
NDX_const <- BatchGetSymbols(tickers = tickers_NDX, first.date = "2018-01-01", freq.data = "daily", type.return = "log")
SP500_const <- BatchGetSymbols(tickers = tickers_SP500, first.date = "2018-01-01", freq.data = "daily", type.return = "log")

DJI_const <- DJI_const[[2]]
DJI_const_adj <- dplyr::select(DJI_const, ticker, ret.adjusted.prices, ref.date)
NDX_const <- NDX_const[[2]]
NDX_const_adj <- dplyr::select(NDX_const, ticker, ret.adjusted.prices, ref.date)
SP500_const <- SP500_const[[2]]
SP500_const_adj <- dplyr::select(SP500_const, ticker, ret.adjusted.prices, ref.date)
index <- BatchGetSymbols(tickers = "^DJI" , thresh.bad.data = 0, first.date = "2018-01-01", freq.data = "daily", type.return = "log")
indexes <- index[[2]]
DJI_adj <- dplyr::select(indexes, ticker, ret.adjusted.prices,ref.date)
index <- BatchGetSymbols(tickers =  "^GSPC" , thresh.bad.data = 0, first.date = "2018-01-01", freq.data = "daily", type.return = "log")
indexes <- index[[2]]
GSPC_adj <- dplyr::select(indexes, ticker, ret.adjusted.prices,ref.date)
index <- BatchGetSymbols(tickers = "^NDX" , thresh.bad.data = 0, first.date = "2018-01-01", freq.data = "daily", type.return = "log")
indexes <- index[[2]]
NDX_adj <- dplyr::select(indexes, ticker, ret.adjusted.prices,ref.date)


stock_data_DJI <- data.frame(Date = DJI_adj$ref.date)
tickers_DJI <- unique(DJI_const_adj["ticker"])[,1]
for(i in 1:length(tickers_DJI)){
  stock_name <- tickers_DJI[i]
  temp <- data.frame(DJI_const_adj[DJI_const_adj[,"ticker"] == stock_name, "ref.date"])
  temp[c(stock_name, "Date")] <- DJI_const_adj[DJI_const_adj[,"ticker"] == stock_name, c("ret.adjusted.prices","ref.date")]
  stock_data_DJI <- left_join(stock_data_DJI, temp, by.y = "Date", by.x = "Date", keep = FALSE)
}


stock_data_GSPC <- data.frame(Date = GSPC_adj$ref.date)
for(i in 1:length(tickers_SP500)){
  stock_name <- tickers_SP500[i]
  temp <- data.frame(SP500_const_adj[SP500_const_adj[,"ticker"] == stock_name, "ref.date"])
  temp[,c(stock_name, "Date")] <- SP500_const_adj[SP500_const_adj[,"ticker"] == stock_name, c("ret.adjusted.prices","ref.date")]
  stock_data_GSPC <- left_join(stock_data_GSPC, temp, by.y = "Date", by.x = "Date", keep = FALSE)
}

teste <- myGetSymbols("MMM", i.ticker = 1, length.tickers = NULL, first.date = as.Date("2002-01-01"), last.date = Sys.Date())

stock_data_NDX <- data.frame(Date = NDX_adj$ref.date)
for(i in 1:length(tickers_NDX)){
  stock_name <- tickers_NDX[i]
  temp <- data.frame(NDX_const_adj[NDX_const_adj[,"ticker"] == stock_name, "ref.date"])
  temp[c(stock_name, "Date")] <- NDX_const_adj[NDX_const_adj[,"ticker"] == stock_name, c("ret.adjusted.prices","ref.date")]
  stock_data_NDX <- left_join(stock_data_NDX, temp, by.y = "Date", by.x = "Date", keep = FALSE)
}

DJI_stocks <- stock_data_DJI[names(stock_data_DJI)[-1]]
GSPC_stocks <- stock_data_GSPC[names(stock_data_GSPC)[-1]]
NDX_stocks <- stock_data_NDX[names(stock_data_NDX)[-1]]
colnames(DJI_stocks)[1] <- "Date"
weekday_dummies <- DJI_stocks %>%
  mutate(date = as.Date(Date)) %>%
  mutate(weekday = weekdays(Date)) %>%
  dcast(Date ~ weekday, fun.aggregate = length)

monthly_dummies <- DJI_stocks %>%
  mutate(date = as.Date(Date)) %>%
  mutate(month = months(Date)) %>%
  dcast(Date ~ month, fun.aggregate = length)

quarterly_dummies <- DJI_stocks %>%
  mutate(date = as.Date(Date)) %>%
  mutate(quarter = quarters(Date)) %>%
  dcast(Date ~ quarter, fun.aggregate = length)


getSymbols(Symbols = tickers_DJI,  from = "2002-01-01")
prices <- map(tickers_DJI,function(x) Ad(get(x)))
prices <- purrr::reduce(prices,merge)
colnames(prices) <- tickers_DJI

data_1 <- ts(na.locf(na.locf(prices, fromLast = TRUE)), frequency = 1)
data <- na.locf(na.locf(ts(DJI_stocks[,-1], frequency = 1), fromLast = TRUE))
data <- data[, colnames(data) != "BRK.B"]
data <- data[, colnames(data) != "BF.B"]
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
  #exog_var <- data[,-i]
  #var_names <- colnames(exog_var)
  #
  #for(z in 1:3){
  #
  #  for(j in 1:length(exog_var[1,])){
  #        if(z==1 & j==1){
  #            exog <- matrix(stats::lag(exog_var[,1], -1))
  #         } else{
  #              exog <- cbind(exog, stats::lag(exog_var[,j], -z)) %>% head(NROW(model_data))
  #      }
  #  }
  #}
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
  #freq <- c(253, 126.5, 84.3, 21, 5)
  #freque <- 253
  #application <- vector("list", length = 5)
  #for(o in 1:5){
  #  application[o] <- isSeasonal(ts(data[,i], frequency = freq[o]))
  #}
  #value <- match(TRUE, application)
  #if(is.numeric(value)){
  #  season = TRUE
  #  freque = freq[value]
  #} else {
  #  season = FALSE
  #}
  temp <- auto.arima(model_data,start.p = 0, max.p = 0, start.q = 0, max.q = 0, max.P = 4, max.Q = 4, stationary = TRUE, seasonal = TRUE, stepwise = FALSE, parallel = TRUE)
  summary(temp)
  #temp <- arima(model_data, xreg = exog, include.mean = TRUE, order = c(1,0,0))
  print(i)
  #print(temp)
  models_garch_auto<-cbind(models_garch_auto,list(temp))
  #print(length(exog[1,])/z)
  #rm(exog)
}
models <- function(data, stock_order = c(0,0),  external = NULL, pars = NULL) {
  model.index.spec <- ugarchspec(variance.model = list(model = 'gjrGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = stock_order, include.mean = TRUE, external.regressors = external), fixed.pars = pars,  distribution.model = "norm")

  (model.index.fit <- ugarchfit(spec = model.index.spec , data = data, solver = 'hybrid'))
}
models_sarima <- list()
for(i in 1:length(data[1,])) {

  model_data <- data[,i]

  exog_var <- data[,-i]
  var_names <- colnames(exog_var)
  #p <- TSA::periodogram(model_data)
  #f <- data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:3]$period
  #q1 <- fourier(ts(model_data, frequency=f[1]), K=1)

  #q2 <- diff.ts(fourier(ts(model_data, frequency=f[2]), K=1))
  #q3 <- diff.ts(fourier(ts(model_data, frequency=f[3]), K=1))
  ##q4 <- fourier(ts(model_data, frequency=164.57), K=1)
  weeknames <- colnames(weekday_dummies[3:6])
  monthnames <- colnames(monthly_dummies[3:13])
  #quarternames <- colnames(quarterly_dummies[3:5])

  for(z in 1:2){

    for(j in 1:length(exog_var[1,])){
          if(z==1 & j==1){
              exog <- stats::lag(exog_var[,1], -1)
           } else{
                exog <- cbind(exog, stats::lag(exog_var[,j], -z)) %>% head(NROW(model_data))
        }
    }
  }
  for(q in 3:6){
    exog <- cbind(exog, weekday_dummies[q])
  }
  for(q in 3:13){
    exog <- cbind(exog,monthly_dummies[q])
  }
  #for(q in 3:5){
  #  exog <- cbind(exog, quarterly_dummies[q])
  #}
  temp_names <- vector("list", length = length(var_names))
  for(p in 1:z){
    for(k in 1:(length(tickers_DJI)-1)){
        temp_names[k] <- paste(var_names[k],"_",toString(p),sep="")
      }
    if(p == 1){
      exog_names <- temp_names
    } else {
      exog_names <- c(exog_names, temp_names)
    }
  }
  colnames(exog) <- c(exog_names, weeknames, monthnames)
  print(i)
  #print(models_garch_auto[[i]]$arma)
  temp <- arima(model_data[z:length(model_data)],order = c(z,0,0),xreg=exog[z:length(model_data),], include.mean=TRUE)
  teste <- ((1-pnorm(abs(temp$coef)/sqrt(diag(temp$var.coef))))*2)
  teste[teste<0.10] <- NA
  teste[teste>0.10] <- 0

  #p <- TSA::periodogram(resid(na.omit(temp)))
  #f <- data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:3]$period
  #q1 <- fourier(ts(model_data[z:length(model_data)], frequency=f[1]), K=1)
  #q2 <- fourier(ts(model_data[z:length(model_data)], frequency=f[2]), K=1)
  #q3 <- fourier(ts(model_data[z:length(model_data)], frequency=f[3]), K=1)
  fix_var <- teste
  #fix_var <- c(teste,NA,NA,NA,NA,NA,NA)
  #names(fix_var) <- c(names(teste),colnames(q1),colnames(q2),colnames(q3))
  #,q1[,1],q1[,2],q2[,1],q2[,2],q3[,1], q3[,2])
  temp <- arima(model_data[z:length(model_data)], fixed = fix_var, order = c(z,0,0),xreg=exog[z:length(model_data),], include.mean=TRUE, transform.pars = FALSE)
  teste_arch <- ArchTest(resid(temp), lag = 1)$p.value
  if(teste_arch<0.05){
    temp <- models(data = model_data[z:length(model_data)], stock_order = c(z,0), external = exog[z:length(model_data),])
    fix_var_garch <- temp@fit$matcoef[,4]
    fix_var_garch[fix_var_garch<0.10] <- NA
    fix_var_garch[fix_var_garch>0.10] <- 0
    exog_garch <- exog[z:length(model_data),]
    colnames(exog_garch) <- colnames(coef(temp)[(z+1):(z+1+27*z)])
    temp <- models(data = model_data[z:length(model_data)], stock_order = c(z,0), external = exog_garch, pars = as.list(na.omit(fix_var_garch)))
  }

  # seasonal = c(models_garch_auto[[i]]$arma[3], 0, models_garch_auto[[i]]$arma[4]),
  #temp <- arima(model_data, xreg = exog, include.mean = TRUE, order = c(1,0,0))
  #print(i)
  #print(temp)
  models_sarima<-cbind(models_sarima,list(temp))
  #print(length(exog[1,])/z)
  #rm(exog)
}


resid_vec <- matrix(nrow = length(resid(temp)), ncol = length(tickers_DJI))
for(i in 1:length(tickers_DJI)){
  resid_vec[,i] <- resid(models_garch_auto[[i]])
}
colnames(resid_vec)<-tickers_DJI
correlacoes <- cor(na.locf(na.locf(resid_vec), fromLast = TRUE))
lambda_cor <- coredata(correlacoes[2,1])

resid_vec <- matrix(nrow = length(resid(temp)), ncol = length(tickers_DJI))
for(i in 1:length(tickers_DJI)){
  resid_vec[,i] <- resid(models_sarima[[i]])
}
resid_vec <- matrix(nrow = length(residuals(temp)), ncol = length(data[1,]))
for(i in 1:length(data[1,])){
  resid_vec[,i] <- residuals(models_sarima[[i]])
}
colnames(resid_vec)<-tickers_DJI
correlacoes <- cor(na.locf(na.locf(resid_vec), fromLast = TRUE))
lambda_cor <- coredata(correlacoes[2,1])
M <- length(data[1,])
corr_sq <- matrix()
for(i in 3:M){
  for(j in 1:(i-1)){
    lambda_cor<-lambda_cor+(coredata(correlacoes[i,j])^2)
    corr_sq <- rbind(corr_sq,coredata(correlacoes[i,j]^2))

  }
}

lambda <- nrow(data)*lambda_cor
df = M*(M-1)/2
qchisq(.05, df=df)
lambda
sum_cor <- cbind(matrix(colSums(abs(correlacoes))), tickers_DJI)

sum_cor_sort <- sum_cor[order(sum_cor[,1], decreasing = TRUE),]

model_ind <- Arima(na.omit(DJI_adj$ret.adjusted.prices), xreg = resid_vec, order = c(1,0,0))

uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "sged")
uspec2 = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "sged")
spec1 = dccspec(uspec = multispec(list(uspec, uspec2)), dccOrder = c(1,1),  distribution = "mvnorm")
fit1 = dccfit(spec1, data = cbind(na.omit(resid_vec)[,2], na.omit(resid_vec)[,1]), solver = "solnp")
sinal_corr3 <- rcor(fit1)[1,2,]
M <- 30
corr_sq <- matrix()
for(i in 3:M){
  for(j in 1:(i-1)){
  fit1 = dccfit(spec1, data = cbind(na.omit(resid_vec)[,i], na.omit(resid_vec[,j])), solver = "solnp")
  sinal_corr3 <- sinal_corr3 + rcor(fit1)[1,2,]

  }
}
plot(sinal_corr3, type = "l")
for(i in 1:resid_vec)
uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "sstd")
uspec2 = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = FALSE), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "sstd")
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

seasonality <- vector("list", length = length(tickers_DJI))
for(m in 1:length(tickers_DJI)){
  freq <- c(253, 127, 63, 21, 5)
  freque <- 253
  application <- vector("list", length = 5)
  for(o in 1:5){
    application[o] <- isSeasonal(ts(data[,m], frequency = freq[o]))
  }
  value <- match(TRUE, application)
  if(is.na(value)){
    season = FALSE
    freque = FALSE

  } else {
    season = TRUE
    freque = freq[value]
  }
  seasonality[m] <- freque
}

cor(cbind(fitted(models_garch_auto[[1]]), resid(models_garch_auto[[1]])))

jarque.bera.test(resid_vec[,3])

teste <- c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
min(teste)

sinal_corr3 <- resid_vec[,1]
for(i in 2:length(tickers_DJI)){
  sinal_corr3 <- resid_vec[,i]
}
p <- TSA::periodogram(na.omit(sinal_corr3))


sinal_corr3 <- abs(resid_vec[,1])
for(i in 2:length(data[1,])){
  sinal_corr3 <- abs(resid_vec[,i])
}
plot(sinal_corr3, type = 'l')
sinal_corr3 <- data_1[,1]
for(i in 2:length(data_1[1,])){
  sinal_corr3 <- data_1[,i]
}

p <- TSA::periodogram(na.omit(sinal_corr3))
data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:7]$period
dec <- decompose(ts(sinal_corr3, frequency = 6.85))
autoplot(dec)

isSeasonal(ts(sinal_corr3, frequency = 6.85))

teste <- dynlm(formula = MMM~ L(MMM,1) + L(AXP,1), data = data)


models_pcr <- list()
for(i in 1:length(data[1,])) {

  model_data <- data[,i]


  exog_var <- data
  var_names <- colnames(exog_var)
  #p <- TSA::periodogram(model_data)
  #f <- data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:3]$period
  #q1 <- fourier(ts(model_data, frequency=f[1]), K=1)

  #q2 <- diff.ts(fourier(ts(model_data, frequency=f[2]), K=1))
  #q3 <- diff.ts(fourier(ts(model_data, frequency=f[3]), K=1))
  ##q4 <- fourier(ts(model_data, frequency=164.57), K=1)

  #quarternames <- colnames(quarterly_dummies[3:5])

  for(z in 1:2){

    for(j in 1:length(exog_var[1,])){
          if(z==1 & j==1){
              exog <- stats::lag(exog_var[,1], -1)
           } else{
                exog <- cbind(exog, stats::lag(exog_var[,j], -z)) %>% head(NROW(model_data))
        }
    }
  }
  print(i)
  dados <- cbind(exog, model_data)
  colnames(dados) <- c(colnames(exog), "target")
  temp <- pcr(formula = target ~., data = dados)
  models_pcr<-cbind(models_pcr,list(temp))
}

resid_vec <- matrix(nrow = length(residuals(temp)[,,56]), ncol = length(data[1,]))
for(i in 1:length(data[1,])){
  resid_vec[,i] <- residuals(models_pcr[[i]])[,,56]
}
colnames(resid_vec)<-tickers_DJI
correlacoes <- cor(na.locf(na.locf(resid_vec), fromLast = TRUE))
lambda_cor <- coredata(correlacoes[2,1])
M <- length(data[1,])
corr_sq <- matrix()
for(i in 3:M){
  for(j in 1:(i-1)){
    lambda_cor<-lambda_cor+(coredata(correlacoes[i,j])^2)
    corr_sq <- rbind(corr_sq,coredata(correlacoes[i,j]^2))

  }
}

lambda <- nrow(data)*lambda_cor
df = M*(M-1)/2
qchisq(.05, df=df)
lambda