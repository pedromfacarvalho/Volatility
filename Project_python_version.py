import yfinance as yf
import pandas as pd
import numpy as np
import statsmodels.api as sm
import scipy.stats as stats
from statsmodels.graphics.gofplots import qqplot
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.stattools import adfuller, pacf, acf
from statsmodels.stats.diagnostic import het_arch
from statsmodels.tsa.arima.model import ARIMA
from statsmodels.tsa.statespace.sarimax import SARIMAX
import arch
import datetime


from pmdarima import auto_arima
import matplotlib.pyplot as plt

# Ignore harmless warnings
import warnings

warnings.filterwarnings("ignore")

DJI = yf.download("DJI", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
GSPC = yf.download("^GSPC", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
IXIC = yf.download("^IXIC", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
BTC = yf.download("BTC-USD", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
ETH = yf.download("ETH-USD", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
XRP = yf.download("XRP-USD", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)

lista_data = [DJI, GSPC, IXIC, BTC, ETH, XRP]
data_price = pd.DataFrame(columns = ["DJI", "GSPC", "IXIC", "BTC", "ETH", "XRP"])
for i,val in enumerate(lista_data):
    data_price[data_price.columns[i]] = val['Close']
    plt.plot(data_price)
    plt.show()

data = np.log(data_price) - np.log(data_price.shift(1))

for i in data.columns:
    plt.plot(data[i])
    plt.show()
    print(f"Jarque Bera for {i}")
    print(stats.jarque_bera(data[i].dropna()))
    print(f"Augmented Dickey-Fuller for {i}")
    print(adfuller(data[i].dropna()))
    #qqplot(data[i].dropna())

modelos = dict()
for i in data.columns:

    print(f"Modelo arima para {i}"
          f"##############################################"
          f"##############################################")
    # Fit auto_arima function to AirPassengers dataset
    modelos[i] = auto_arima(data[i].dropna())  # set to stepwise

    # To print the summary
    print(modelos[i].summary())

for i in data.columns:
    plt.plot(acf(data[i].dropna()))
    plt.title(i)
    plt.show()
for i in data.columns:
    plt.plot(pacf(data[i].dropna()))
    plt.title(i)
    plt.show()

for i in data.columns:
    print(f"ARCHTest para resíduos de {i}")
    print(het_arch(modelos[i].arima_res_.resid,nlags=6))
    print("Time Series")
    print(het_arch(data[i].dropna(),nlags=6))


#correcção de modelos


modelos["BTC"] = ARIMA(data["BTC"].dropna(), order=(1,0,0)).fit()
print(modelos["BTC"].summary())
modelos["ETH"] = ARIMA(data["XRP"].dropna(), order=(1,0,1)).fit()
print(modelos["ETH"].summary())
modelos["XRP"] = ARIMA(data["XRP"].dropna(), order=(1,0,1)).fit()
print(modelos["BTC"].summary())


modelo_arma_DJI = ARIMA(data["DJI"].dropna(),order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())
#constante não significativa mas diminuta, perfeito

##DJI_BTC
outer_var_BTC = pd.concat([data["DJI"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(-1) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["BTC"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["BTC"],exog=data_model_BTC[["DJI","DJI resid"]], order=(1,0,1)).fit()
print(modelo_arma_DJI_BTC.summary())

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["BTC"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(1,0,1)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())

##DJI_ETH
outer_var_BTC = pd.concat([data["ETH"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(-3) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["BTC"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["BTC"],exog=data_model_BTC[["DJI","DJI resid"]], order=(1,0,1)).fit()
print(modelo_arma_DJI_BTC.summary())

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["BTC"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(1,0,1)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())

##DJI e XRP encontrar nova solução


##GSPC BTC
modelo_arma_DJI = ARIMA(data["GSPC"].dropna(),order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())

outer_var_BTC = pd.concat([data["BTC"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(-1) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["BTC"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["BTC"],exog=data_model_BTC[["DJI","DJI resid"]], order=(1,0,1)).fit()
print(modelo_arma_DJI_BTC.summary())

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["BTC"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(1,0,1)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())
arch.arch_model()