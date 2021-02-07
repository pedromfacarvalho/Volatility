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


modelos["BTC"] = ARIMA(data["BTC"].dropna(), order=(1,0,1)).fit()
print(modelos["BTC"].summary())
modelos["ETH"] = ARIMA(data["XRP"].dropna(), order=(1,0,1)).fit()
print(modelos["ETH"].summary())
modelos["XRP"] = ARIMA(data["XRP"].dropna(), order=(1,0,1)).fit()
print(modelos["BTC"].summary())





