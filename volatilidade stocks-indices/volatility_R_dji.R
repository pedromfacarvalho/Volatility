GSPC <- na.locf(na.locf(getSymbols("^GSPC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
GSPC_adj <- xts(GSPC$GSPC.Adjusted)
plot(GSPC_adj)
#eGARCH, ARIMA (0,0,1) sstd
IXIC <- na.locf(na.locf(getSymbols("^IXIC", src = "yahoo", from = "2002-01-01", auto.assign = FALSE)), fromLast = TRUE)
IXIC_adj <- IXIC$IXIC.Adjusted
plot(IXIC_adj)
#eGARCH,ARIMA (0,0,1) sstd
