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


AAPL = yf.download("AAPL", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
MSFT = yf.download("MSFT", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
AMZN = yf.download("AMZN", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
GOOGL = yf.download("GOOGL", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
FB = yf.download("FB", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
TSLA = yf.download("TSLA", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
BRK = yf.download("BRK", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
V = yf.download("V", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
JNJ = yf.download("JNJ", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
WMT = yf.download("WMT", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)

lista_data = [AAPL, MSFT, AMZN, GOOGL, FB, TSLA, BRK, V, JNJ, WMT]
data_price = pd.DataFrame(columns = ["AAPL", "MSFT", "AMZN", "GOOGL", "FB", "TSLA", "BRK", "V", "JNJ", "WMT"])
for i,val in enumerate(lista_data):
    data_price[data_price.columns[i]] = val['Close']

data = np.log(data_price) - np.log(data_price.shift(1))

for i in data.columns:
    print(f"Jarque Bera for {i}")
    print(stats.jarque_bera(data[i].dropna()))
    print(f"Augmented Dickey-Fuller for {i}")
    print(adfuller(data[i].dropna()))


modelos = dict()
for i in data.columns:

    print(f"Modelo arima para {i}"
          f"##############################################"
          f"##############################################")
    # Fit auto_arima function to AirPassengers dataset
    modelos[i] = auto_arima(data[i].dropna())  # set to stepwise

    # To print the summary
    print(modelos[i].summary())
"""
modelos_AR = dict()
for i in data.columns:

    print(f"Modelo arima para {i}"
          f"##############################################"
          f"##############################################")
    # Fit auto_arima function to AirPassengers dataset
    modelos_AR[i] = ARIMA(data[i].dropna(), order = (1,0,0)).fit()  # set to stepwise

    # To print the summary
    print(modelos_AR[i].summary())
"""
for i in data.columns:
    print(f"ARCHTest para resíduos de {i}")
    print(het_arch(modelos[i].arima_res_.resid,nlags=6))
    print("Time Series")
    print(het_arch(data[i].dropna(),nlags=6))

modelos_garch = dict()
for i in data.columns:

    print(f"Modelo GARCH para {i}"
          f"##############################################"
          f"##############################################")
    # Fit auto_arima function to AirPassengers dataset
    modelos_garch[i] = arch.arch_model(modelos[i].arima_res_.resid, vol = "GARCH", rescale= True).fit()  # set to stepwise

    # To print the summary
    print(modelos_garch[i].summary())


indice = yf.download("^GSPC", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
indice_ret = np.log(indice["Close"]) - np.log(indice["Close"].shift(1))

indice_modelo = auto_arima(indice_ret.dropna())
print(indice_modelo.summary())
indice_garch = arch.arch_model(indice_modelo.arima_res_.resid, vol = "GARCH", rescale=True).fit()
print(indice_garch.summary())

resid_series = []
conditional_volatilities_stocks = []
for i in data.columns:
    resid_series.append(pd.Series(modelos[i].arima_res_.resid, index= data[i].dropna().index))
    conditional_volatilities_stocks.append(pd.Series(modelos_garch[i].conditional_volatility, index= data[i].dropna().index))


dados_para_reg = pd.DataFrame()
conditional_volatilities_frame = pd.DataFrame()
for i in resid_series:
    dados_para_reg = pd.concat([dados_para_reg, i], axis=1)
for i in conditional_volatilities_stocks:
    conditional_volatilities_frame = pd.concat([conditional_volatilities_frame, i], axis=1)

dados_para_reg.columns = data.columns
conditional_volatilities_frame.columns = data.columns
dados_para_reg.info()

from sklearn.linear_model import LinearRegression
model = LinearRegression().fit(X = dados_para_reg.fillna(0), y = indice_modelo.arima_res_.resid)
model.score(X = dados_para_reg.fillna(0), y = indice_modelo.arima_res_.resid)

pred = model.predict(dados_para_reg.fillna(0))
residuos_lin = indice_modelo.arima_res_.resid - pred
het_arch(residuos_lin, nlags=3)
stats.jarque_bera(residuos_lin)
residuos_lin.mean()
modelo_res_lin = arch.arch_model(residuos_lin, vol="GARCH", rescale=True, mean='zero').fit()
stats.jarque_bera(residuos_lin)
modelo_res_lin.summary()


indice_volatility_calc = pd.Series(0, index=conditional_volatilities_frame.index)
for i, val in enumerate(data.columns):
    indice_volatility_calc = indice_volatility_calc + model.coef_[i]**2*conditional_volatilities_frame[val]
indice_volatility_calc += modelo_res_lin.conditional_volatility

import matplotlib.pyplot as plt
def func(time,column):
    plt.plot(time)
    plt.title(f"variance for {column}")
    plt.show()

for i in conditional_volatilities_frame.columns:
    func(conditional_volatilities_frame[i],i)

func(indice_volatility_calc, i)
func(modelo_res_lin.conditional_volatility,i)
correlations = pd.DataFrame(columns = conditional_volatilities_frame.columns)
for j,i in enumerate(conditional_volatilities_frame.columns):
    correlations[i] = (model.coef_[j]*np.sqrt(conditional_volatilities_frame[i].fillna(0)))/indice_volatility_calc

for i in correlations.columns:
    func(correlations[i], i)