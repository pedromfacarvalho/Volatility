# Title     : TODO
# Objective : TODO
# Created by: C097564
# Created on: 2021-09-19

pacman::p_load(BatchGetSymbols, gogarch, vars, keras, rmgarch, doParallel, aTSA, reshape2, purrr, LSTS, fDMA, tidyquant, data.table, pls, dynlm, stats, ggplot2, ggfortify, plotly, zoo, TSA, gridExtra, GeneCycle, qrmdata,XML,quantmod,zoo,chron, rmgarch, rugarch, rvest, dplyr, FinTS, forecast, car, fpp2, lmtest,tseries,stats, seastests)


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
index <- BatchGetSymbols(tickers = "^DJI" , first.date = "2018-01-01", freq.data = "daily", type.return = "log")
indexes <- index[[2]]
DJI_adj <- dplyr::select(indexes, ticker, ret.adjusted.prices,ref.date)
index <- BatchGetSymbols(tickers =  "^GSPC" , first.date = "2018-01-01", freq.data = "daily", type.return = "log")
indexes <- index[[2]]
GSPC_adj <- dplyr::select(indexes, ticker, ret.adjusted.prices,ref.date)
index <- BatchGetSymbols(tickers = "^NDX", first.date = "2018-01-01", freq.data = "daily", type.return = "log")
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
  reshape2::dcast(Date ~ weekday, fun.aggregate = length)

monthly_dummies <- DJI_stocks %>%
  mutate(date = as.Date(Date)) %>%
  mutate(month = months(Date)) %>%
  reshape2::dcast(Date ~ month, fun.aggregate = length)

quarterly_dummies <- DJI_stocks %>%
  mutate(date = as.Date(Date)) %>%
  mutate(quarter = quarters(Date)) %>%
  reshape2::dcast(Date ~ quarter, fun.aggregate = length)

dates <- DJI_stocks[,1]
dates <- NDX_stocks[,1]
dates <- GSPC_stocks[,1]

data <- DJI_stocks[,-1]
data <- NDX_stocks[,-1]
data <- GSPC_stocks[,-1]

data <- data[-1,]
data <- na.fill(data,0)
data <- data[, qr(data)$pivot[seq_len(qr(data)$rank)]]
models_sgarch <- function(data, stock_order = c(0,0),  external = NULL, pars = NULL) {
  model.index.spec <- ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) ,
                        mean.model = list(armaOrder = stock_order, include.mean = TRUE, external.regressors = external), fixed.pars = pars,  distribution.model = "norm")

  (model.index.fit <- ugarchfit(spec = model.index.spec , data = data, solver = 'hybrid'))
}
models_t<- function(data, stock_order = c(0,0),  external = NULL, pars = NULL) {
  model.index.spec <- ugarchspec(variance.model = list(model = 'fGARCH' , garchOrder = c(1 , 1), submodel = "TGARCH"),
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

  for(z in 1:1){
    for(j in 1:length(exog_var[1,])){
          if(z==1 & j==1){
              exog <- lag_transform(exog_var[,1], 1)
           } else{
                exog <- cbind(exog, lag_transform(exog_var[,j], z))
        }
    }
  }
cols_to_drop = vector("integer")
  for(m in 1:(z*length(exog_var[1,]))){
    for (n in 2:3){
      standard <- sd(exog[,m])
      boolean_vec_pos <- exog[,m]>(n*standard) && exog[,m]<((n+1)*standard)
      boolean_vec_neg <- exog[,m]<(-n*standard) && exog[,m]>(-(n+1)*standard)
      temp_vector_pos <- matrix(ncol = 1, nrow = length(exog[,1]))
      temp_vector_neg <- matrix(ncol = 1, nrow = length(exog[,1]))
      for (a in 1:length(exog[,1])){
        if ((boolean_vec_pos[a] == FALSE) || is.na(boolean_vec_pos[a])){
          temp_vector_pos[a] <- 0
        }
        else{
          temp_vector_pos[a] <- exog[a,m]
        }
        if ((boolean_vec_neg[a] == FALSE) || is.na(boolean_vec_neg[a])){
          temp_vector_neg[a] <- 0
        }
        else{
          temp_vector_neg[a] <- exog[a,m]
        }
      }
      if((all(exog[,(length(exog[1,])-1)]==temp_vector_pos)) || (all(temp_vector_pos == 0))){
        cols_to_drop <- append(cols_to_drop, (length(exog[1,])+1))
      }
      if((all(exog[,(length(exog[1,]))]==temp_vector_neg)) || (all(temp_vector_neg == 0))){
        cols_to_drop = append(cols_to_drop, (length(exog[1,])+2))
      }
      exog <- cbind(exog,temp_vector_pos)
      exog <- cbind(exog,temp_vector_neg)
    }
    standard <- sd(exog[,m])
    boolean_vec_pos <- exog[,m]>((n+1)*standard)
    boolean_vec_neg <- exog[,m]<(-(n+1)*standard)
    temp_vector_pos <- matrix(ncol = 1, nrow = length(exog[,1]))
    temp_vector_neg <- matrix(ncol = 1, nrow = length(exog[,1]))
    for (a in 1:length(exog[,1])){
      if ((boolean_vec_pos[a] == FALSE) || is.na(boolean_vec_pos[a])){
        temp_vector_pos[a] <- 0
      }
      else{
        temp_vector_pos[a] <- exog[a,m]
      }
      if ((boolean_vec_neg[a] == FALSE) || is.na(boolean_vec_neg[a])){
        temp_vector_neg[a] <- 0
      }
      else{
        temp_vector_neg[a] <- exog[a,m]
      }
    }
    if((all(exog[,(length(exog[1,])-1)]==temp_vector_pos)) || (all(temp_vector_pos == 0))){
      cols_to_drop <- append(cols_to_drop, (length(exog[1,])+1))
    }
    if((all(exog[,(length(exog[1,]))]==temp_vector_neg)) || (all(temp_vector_neg == 0))){
      cols_to_drop = append(cols_to_drop, (length(exog[1,])+2))
    }
    exog <- cbind(exog,temp_vector_pos)
    exog <- cbind(exog,temp_vector_neg)
  }

  for(q in 3:6){
    exog <- cbind(exog, weekday_dummies[q])
  }
  for(q in 3:13){
    exog <- cbind(exog,monthly_dummies[q])
  }
  # for(q in 3:5){
  #  exog <- cbind(exog, quarterly_dummies[q])
  # }
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
#  colnames(exog) <- c(exog_names, weeknames, monthnames)
  exog <- exog[, qr(exog)$pivot[seq_len(qr(exog)$rank)]]

  print(i)
  # exog <- na.fill(exog,0)
  # exog <- exog[, qr(exog)$pivot[seq_len(qr(exog)$rank)]]

  #print(models_garch_auto[[i]]$arma)
  #temp <- stats::arima(model_data[z:length(model_data)],order = c(z,0,0), xreg=exog[z:length(model_data),], include.mean=TRUE)
  #teste <- ((1-pnorm(abs(temp$coef)/sqrt(diag(temp$var.coef))))*2)
  #teste[teste<0.10] <- NA
  #teste[teste>0.10] <- 0

  #p <- TSA::periodogram(resid(na.omit(temp)))
  #f <- data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:3]$period
  #q1 <- fourier(ts(model_data[z:length(model_data)], frequency=f[1]), K=1)
  #q2 <- fourier(ts(model_data[z:length(model_data)], frequency=f[2]), K=1)
  #q3 <- fourier(ts(model_data[z:length(model_data)], frequency=f[3]), K=1)
  #fix_var <- teste
  #fix_var <- c(teste,NA,NA,NA,NA,NA,NA)
  #names(fix_var) <- c(names(teste),colnames(q1),colnames(q2),colnames(q3))
  #,q1[,1],q1[,2],q2[,1],q2[,2],q3[,1], q3[,2])
  #temp <- arima(model_data[(z+1):length(model_data)], order = c(z,0,0),xreg=exog[(z+1):length(model_data),], include.mean=TRUE, transform.pars = FALSE)
  #teste_arch <- ArchTest(resid(temp), lag = 1)$p.value
  # if(TRUE){
  temp <- models_sgarch(data = model_data[3:length(model_data)], stock_order = c(z,0), external = as.matrix(exog[3:length(model_data),]))
    #temp <- models(data = model_data[z:length(model_data)], stock_order = c(z,0), external = exog_garch, pars = as.list(na.omit(fix_var_garch)))
  #}

  # seasonal = c(models_garch_auto[[i]]$arma[3], 0, models_garch_auto[[i]]$arma[4]),
  #temp <- arima(model_data, xreg = exog, include.mean = TRUE, order = c(1,0,0))
  #print(i)
  #print(temp)
  models_sarima<-cbind(models_sarima,list(temp))
  #print(length(exog[1,])/z)
  #rm(exog)
}
teste <- varxfit(data, p=2, exogen = as.matrix(exog[,29:92]))
uspec <- multispec(replicate( 29,
                             ugarchspec(mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
                                                   variance.model = list(garchOrder = c(1,1), model = "sGARCH"),
                                                distribution.model = "norm") ) )

# multivariate spec. VAR with 1 lag...(The robust version is slower but
# uses a least trimmed squares procedure (see the references).
mspec = dccspec(uspec, VAR = FALSE, robust = TRUE,
lag = 1, lag.max = 2, lag.criterion = c("AIC", "HQ", "SC",
"FPE"), model = "aDCC", external.regressors = NULL, dccOrder = c(1, 1), distribution = "mvnorm")

# see ?dccfit for option of passing the VAR estimate seperately at this
# stage and the methods applicable to the dccfit object
# (?"DCCfit-class")
fit = dccfit(spec = mspec, data = data, out.sample = 0)



lag_to_try <- VARselect(data, type = "const")
lag_to_try$selection
resid_vec <- matrix(nrow = length(residuals(temp)), ncol = length(data[1,]))
for(i in 1:length(data[1,])){
  resid_vec[,i] <- residuals(models_sarima[[i]])
}
resid_vec <- residuals(fit)
resid_vec <- teste$xresiduals
#resid_vec <- data
correlacoes <- cor(resid_vec)
M <- length(resid_vec[1,])
lambda_cor <- sum(rowSums(correlacoes^2 * upper.tri(correlacoes^2, diag=FALSE)))

lambda <- (length(model_data)-1)*lambda_cor
#lambda <- nrow(resid_crash)*lambda_cor
#lambda <- nrow(resid_not_crash)*lambda_cor

df = M*(M-1)/2
qchisq(.05, df=df)
lambda

lag_transform <- function(x, k){

      lagged =  na.locf(na.locf(c(rep(NA, k), x[1:(length(x)-k)]), fromLast = TRUE))
      return(lagged)
}





dji_data <- DJI_adj$ret.adjusted.prices[-1]
temp <- models_sgarch(data = dji_data[-1], stock_order = c(1,0), external = exog)
fix_var_garch <- temp@fit$matcoef[,4]
while(all(na.omit(fix_var_garch<0.10))==FALSE){
  fix_var_garch[is.na(fix_var_garch)] <- 1
  fix_var_garch[fix_var_garch<0.10] <- NA
  fix_var_garch[fix_var_garch>0.10] <- 0
  temp <- models_sgarch(data = GSPC_adj$ret.adjusted.prices[2:935], stock_order = c(1,0), external = exog, pars = as.list(na.omit(fix_var_garch)))
  fix_var_garch <- temp@fit$matcoef[,4]
}
temp
test <- residuals(models_sarima[[1]])
sigma <- matrix(nrow = length(test), ncol = length(models_sarima))
for(i in 1:length(models_sarima)){
  sigma[,i] <- sigma(models_sarima[[i]])
}
sigma_index <- sigma(temp)
coefs <- coef(temp)[3:31]

h <- coefs[1]^2 * sigma[,1]^2
for (i in 2:length(models_sarima)){
  h <- h+(coefs[i]^2 * sigma[,i]^2)
}
h <- h+sigma_index^2



cond_corr <- matrix(nrow = length(test), ncol = length(models_sarima))
for (i in 1:length(models_sarima)){
  cond_corr[,i] <- (coefs[i]*sigma[,i])/sqrt(h)
}
dates <- dates[c(-1,-2)]

plot(x = as.Date(dates), cond_corr[,1], ylim = c(0,1), type = 'l')
for (i in 2:length(models_sarima)){
  lines(x = as.Date(dates), cond_corr[,i], type = 'l')
}

cond_corr_mean <- rowSums(cond_corr)

plot(x = as.Date(dates), cond_corr_mean, type = 'l')

crash <- dummy_returns_simple <- ifelse(dates %in% seq(as.Date("2020-02-20"), as.Date("2020-04-07"), by="days"), 1, 0)
resid_crash <- resid_vec*crash

exogenous_index <- resid_vec
for (i in 1:length(resid_crash[1,])){
  exogenous_index <- cbind(exogenous_index,resid_crash[,i])
}





temp <- models_sgarch(data = dji_data[-1], stock_order = c(1,0), external = exogenous_index)
fix_var_garch <- temp@fit$matcoef[,4]
while(all(na.omit(fix_var_garch<0.10))==FALSE){
  fix_var_garch[is.na(fix_var_garch)] <- 1
  fix_var_garch[fix_var_garch<0.10] <- NA
  fix_var_garch[fix_var_garch>0.10] <- 0
  temp <- models_sgarch(data = dji_data[-1], stock_order = c(1,0), external = exogenous_index, pars = as.list(na.omit(fix_var_garch)))
  fix_var_garch <- temp@fit$matcoef[,4]
}
temp

sigma <- matrix(nrow = length(test), ncol = length(models_sarima))
for(i in 1:length(models_sarima)){
  sigma[,i] <- sigma(models_sarima[[i]])
}
sigma_index <- sigma(temp)
coefs <- coef(temp)[3:60]

h <- matrix(nrow = length(sigma[,1]), ncol = length(sigma[1,]))
temp_h <- matrix(nrow = sigma[,1], ncol = 1)
for (i in 1:length(models_sarima)){
  for (j in 1:length(sigma[,1]))
  if (crash[j]==1){
    temp_h[j] <- (coefs[i] + coefs[i+29])^2 * sigma[j,i]^2
  }
  else{
    temp_h[j] <- coefs[i]^2 * sigma[j,i]^2
  }
  h[,i] <- temp_h
  temp_h <- matrix(nrow = sigma[,1], ncol = 1)
}

H <- rowSums(h) + coredata(sigma_index)^2



cond_corr <- matrix(nrow = length(sigma[,1]), ncol = length(models_sarima))
for (i in 1:length(models_sarima)){
  cond_corr[,i] <- sqrt(h[,i])/sqrt(H)
}

plot(x = as.Date(dates), cond_corr[,1], ylim = c(0,1), type = 'l')
for (i in 2:length(models_sarima)){
  lines(x = as.Date(dates), cond_corr[,i], type = 'l')
}

VR <- matrix(nrow = length(test), ncol = length(models_sarima))
for (i in 1:length(models_sarima)){
  VR[,i] <- (coefs[i]^2 * sigma[,i]^2)/h
}


