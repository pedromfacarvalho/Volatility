
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
import bekk
from sklearn.preprocessing import MinMaxScaler
#from models_and_graphs import models_charts
from scipy.signal import detrend
import matplotlib.pyplot as plt

from pmdarima import auto_arima
import matplotlib.pyplot as plt

# Ignore harmless warnings
import warnings
lista_data = []
for i in ["^DJI", "^GSPC", "^IXIC", "BTC-USD", "ETH-USD", "XRP-USD", "XMR-USD"]:
    tickerData = yf.Ticker(i)
    lista_data.append(tickerData.history(period='1d', start='2002-1-1').fillna(method='ffill'))

def scaler_timeseries(time_series):
    scaler = MinMaxScaler(feature_range=(-1,1))
    scaled_data = scaler.fit_transform(X=time_series.to_numpy().reshape(-1, 1))
    data_scaled_indexed = pd.Series(scaled_data.reshape(1, -1)[0], index=time_series.index)
    return data_scaled_indexed

"""
DJI = yf.download("DJI", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=False)
GSPC = yf.download("^GSPC", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
IXIC = yf.download("^IXIC", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
BTC = yf.download("BTC-USD", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
ETH = yf.download("ETH-USD", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
XRP = yf.download("XRP-USD", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)
XMR = yf.download("XMR-USD", start="2002-01-01",interval="1d", group_by='ticker', auto_adjust=True)

plt.plot(DJI['Close']["2019-04-01":"2020-04-01"])
plt.show()
DJI.info()
"""
data_price = pd.DataFrame(columns = ["DJI", "GSPC", "IXIC", "BTC", "ETH", "XRP", "XMR"])
for i,val in enumerate(lista_data):
    data_price[data_price.columns[i]] = val['Close']


data = np.log(data_price) - np.log(data_price.shift(1))

for i in data.columns:
    print(f"Jarque Bera for {i}")
    print(stats.jarque_bera(data[i].dropna()))
    print(f"Augmented Dickey-Fuller for {i}")
    print(adfuller(data[i].dropna()))
    #qqplot(data[i].dropna())

scaled_data = pd.DataFrame()
for i in data.columns:
    scaled_data[i] = scaler_timeseries(data[i])

"""
modelos = dict()
for i in scaled_data.columns:

    print(f"Modelo arima para {i}"
          f"##############################################"
          f"##############################################")
    # Fit auto_arima function to AirPassengers dataset
    modelos[i] = auto_arima(scaled_data[i].dropna())  # set to stepwise

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

"""
#correcção de modelos

"""
modelos["BTC"] = ARIMA(data["BTC"].dropna(), order=(0,0,0)).fit()
print(modelos["BTC"].summary())
het_arch(modelos["BTC"].resid,nlags=1)
modelos["ETH"] = ARIMA(data["ETH"].dropna(), order=(0,0,0)).fit()
print(modelos["ETH"].summary())
het_arch(modelos["ETH"].resid, nlags=1)
modelos["XRP"] = ARIMA(data["XRP"].dropna(), order=(0,0,0)).fit()
print(modelos["XRP"].summary())
het_arch(modelos["XRP"].resid)
modelos["XMR"] = ARIMA(data["XMR"].dropna(),order=(0,0,0)).fit()
print(modelos["XMR"].summary())
het_arch(modelos["XMR"].resid)
"""
models_charts(scaled_data["DJI"], scaled_data["BTC"], (1,0,0), (0,0,0), 1)
models_charts(scaled_data["DJI"], scaled_data["ETH"], (1,0,0), (0,0,0), 2)
models_charts(scaled_data["DJI"], scaled_data["XRP"], (1,0,0), (1,0,1), 2)
models_charts(scaled_data["DJI"], scaled_data["XMR"], (1,0,0), (2,0,3), 2)
models_charts(scaled_data["IXIC"], scaled_data["BTC"], (1,0,0), (0,0,0), 2)
models_charts(scaled_data["IXIC"], scaled_data["ETH"], (1,0,0), (0,0,0), 2)
models_charts(scaled_data["IXIC"], scaled_data["XRP"], (1,0,0), (1,0,1), 2)
models_charts(scaled_data["IXIC"], scaled_data["XMR"], (1,0,0), (2,0,3), 2)
models_charts(scaled_data["GSPC"], scaled_data["BTC"], (1,0,0), (0,0,0), 2)
models_charts(scaled_data["GSPC"], scaled_data["ETH"], (1,0,0), (0,0,0), 2)
models_charts(scaled_data["GSPC"], scaled_data["XRP"], (1,0,0), (1,0,1), 2)
models_charts(scaled_data["GSPC"], scaled_data["XMR"], (1,0,0), (2,0,3), 2)


"""

##DJI_BTC###################################################################

modelo_arma_DJI = ARIMA(scaled_data["DJI"],order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())
var_DJI = arch.arch_model(modelo_arma_DJI.resid.dropna(), vol="GARCH").fit()
print(var_DJI.summary())
cond_var_DJI = var_DJI.conditional_volatility**2
#constante não significativa mas diminuta, perfeito

dummy_DJI = pd.Series(0, index=scaled_data["DJI"].index, name="dummy_DJI")
dummy_DJI['2020-02-15':'2020-03-24'] = 1
modelo_arma_DJI = ARIMA(endog=scaled_data["DJI"], exog=dummy_DJI, order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())
var_DJI = arch.arch_model(modelo_arma_DJI.resid.dropna(), vol="GARCH").fit()
print(var_DJI.summary())
cond_var_DJI = var_DJI.conditional_volatility ** 2


outer_var_BTC = pd.concat([scaled_data["DJI"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(-4) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,scaled_data["BTC"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["BTC"],exog=data_model_BTC[["DJI","DJI resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC.summary())

het_arch(resid=modelo_arma_DJI_BTC.resid)

var_DJI_BTC = arch.arch_model(modelo_arma_DJI_BTC.resid.dropna(), vol="GARCH", dist='normal').fit()
print(var_DJI.summary())

plt.plot(var_DJI_BTC.resid)
plt.show()

cond_var_DJI_BTC = var_DJI_BTC.conditional_volatility**2

h = modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI.shift(-4) + cond_var_DJI_BTC

variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI.shift(-4))/h).dropna()
plt.plot(variance_ratio_DJI_BTC)

dummy_resid = dummy_DJI
dummies = pd.concat([dummy_DJI, dummy_resid], axis=1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis=1).dropna(how='any')
mod = ARIMA(endog=dummy_data_model_BTC["BTC"],
            exog=dummy_data_model_BTC[["DJI", "DJI resid", "dummy_DJI", "dummy_resid"]], order=(0, 0, 0))
# with mod.fix_params(params=modelo_arma_DJI_BTC.params.to_dict()):
#   modelo_arma_DJI_BTC_dummy = mod.fit()
modelo_arma_DJI_BTC_dummy = mod.fit()
print(modelo_arma_DJI_BTC_dummy.summary())

#plt.plot(np.log(data_price["BTC"]))
plt.title("Variance ratio DJI into BTC")
plt.show()

detrended_data = pd.Series(detrend(np.log(data_price["BTC"]).dropna()),index=data_price["BTC"].dropna().index)
fig = plt.figure()
ax1 = fig.add_subplot(111)
ax1.plot(detrended_data[len(detrended_data)-100:])
ax1.set_ylabel('y1')

ax2 = ax1.twinx()
ax2.plot(cond_var_DJI[len(cond_var_DJI)-100:], 'r-')
#ax2.plot(variance_ratio_DJI_BTC, 'r-')
ax2.set_ylabel('y2', color='r')
for tl in ax2.get_yticklabels():
    tl.set_color('r')
plt.show()


VR_array = variance_ratio_DJI_BTC.to_numpy()
VR_array.mean()
VR_array.std()

dummy_resid = pd.Series(dummy_DJI, name="dummy_resid")
dummy_resid.name
dummy_data_model_BTC = pd.concat([data_model_BTC, dummy_resid], axis=1).shift(-4).dropna(how='any')
mod = ARIMA(endog=dummy_data_model_BTC["BTC"],
            exog=dummy_data_model_BTC[["DJI", "DJI resid", "dummy_resid"]].shift(-4), order=(0, 0, 0))
# with mod.fix_params(params=modelo_arma_DJI_BTC.params.to_dict()):
#   modelo_arma_DJI_BTC_dummy = mod.fit()
modelo_arma_DJI_BTC_dummy = mod.fit()
print(modelo_arma_DJI_BTC_dummy.summary())


dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI = dummy_DJI['2020-02-01':]
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid = dummy_resid[:'2020-09-01']
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
mod = ARIMA(endog=dummy_data_model_BTC["BTC"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(0,0,0))
with mod.fix_params(params=modelo_arma_DJI_BTC.params.to_dict()):
    modelo_arma_DJI_BTC_dummy = mod.fit()
#modelo_arma_DJI_BTC_dummy = mod.fit()
print(modelo_arma_DJI_BTC_dummy.summary())

yhat = modelo_arma_DJI_BTC_dummy.predict(start=scaled_data.index[0], end=scaled_data.index[-1])

res_dataframe = pd.DataFrame(columns=["measured", "predicted"])
res_dataframe["measured"] = dummy_data_model_BTC["BTC"]
res_dataframe["predicted"] = yhat
res_dataframe.info()

het_arch(modelo_arma_DJI_BTC_dummy.resid)
var_DJI_dummy = arch.arch_model(modelo_arma_DJI_BTC_dummy.resid.dropna()*10, vol="EGARCH").fit()
print(var_DJI_dummy.summary())

dummy_cond_var_DJI_BTC = var_DJI_dummy.conditional_volatility**2
h_dummy = (modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI.shift(-4) + dummy_cond_var_DJI_BTC)[:'2020-03-20']
h_dummy = h_dummy.append(((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI.shift(4) + dummy_cond_var_DJI_BTC)['2020-03-20':])

dummy_variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI.shift(-4))/h_dummy).dropna()[:'2020-03-20']
dummy_variance_ratio_DJI_BTC = dummy_variance_ratio_DJI_BTC.append((((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI.shift(4))/h_dummy).dropna()['2020-03-20':])
plt.plot(dummy_variance_ratio_DJI_BTC)
plt.title("Variance ratio with dummies DJI into BTC")
plt.show()

dummy_VR_array = dummy_variance_ratio_DJI_BTC.to_numpy()
dummy_VR_array.mean()
dummy_VR_array.std()







##DJI_ETH##############################################################
outer_var_BTC = pd.concat([data["DJI"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(4) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["ETH"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["ETH"],exog=data_model_BTC[["DJI","DJI resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC.summary())

het_arch(resid=modelo_arma_DJI_BTC.resid, nlags=4)

var_DJI_BTC = arch.arch_model(modelo_arma_DJI_BTC.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI_BTC = var_DJI_BTC.conditional_volatility**2

h = modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI + cond_var_DJI_BTC

variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI)/h).dropna()
plt.plot(variance_ratio_DJI_BTC)
plt.title("Variance ratio DJI into ETH")
plt.show()

VR_array = variance_ratio_DJI_BTC.to_numpy()
VR_array.mean()
VR_array.std()

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["ETH"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(0,0,0)).fix_params().fit()

print(modelo_arma_DJI_BTC_dummy.summary())

het_arch(modelo_arma_DJI_BTC_dummy.resid)
var_DJI_dummy = arch.arch_model(modelo_arma_DJI_BTC_dummy.resid.dropna(), vol="GARCH").fit(update_freq=5)
print(var_DJI_dummy.summary())

dummy_cond_var_DJI_BTC = var_DJI_dummy.conditional_volatility**2
h_dummy = (modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)[:'2020-03-20']
h_dummy = h_dummy.append(((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)['2020-03-20':])

dummy_variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI)/h_dummy).dropna()[:'2020-03-20']
dummy_variance_ratio_DJI_BTC = dummy_variance_ratio_DJI_BTC.append((((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI)/h_dummy).dropna()['2020-03-20':])
plt.plot(dummy_variance_ratio_DJI_BTC)
plt.title("Variance ratio with dummies DJI into BTC")
plt.show()

dummy_VR_array = dummy_variance_ratio_DJI_BTC.to_numpy()
dummy_VR_array.mean()
dummy_VR_array.std()
##DJI e XRP encontrar nova solução##################################################

modelo_arima_XRP = ARIMA(data["XRP"].dropna(), order = (2,0,2)).fit()
print(modelo_arima_XRP.summary())
residuos = pd.concat([modelo_arma_DJI.resid, modelo_arima_XRP.resid],axis = 1).dropna()
model_DJI_XRP = bekk.bekk_estimation.BEKK(residuos)
model_DJI_XRP.estimate(restriction='full')
##DJI_XMR##############################################################
outer_var_BTC = pd.concat([data["DJI"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(2) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["XMR"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["XMR"],exog=data_model_BTC[["DJI","DJI resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC.summary())


het_arch(resid=modelo_arma_DJI_BTC.resid, nlags=4)

var_DJI_BTC = arch.arch_model(modelo_arma_DJI_BTC.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI_BTC = var_DJI_BTC.conditional_volatility**2

h = modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI + cond_var_DJI_BTC

variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI)/h).dropna()
plt.plot(variance_ratio_DJI_BTC)
plt.title("Variance ratio DJI into ETH")
plt.show()

VR_array = variance_ratio_DJI_BTC.to_numpy()
VR_array.mean()
VR_array.std()

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["XMR"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())

het_arch(modelo_arma_DJI_BTC_dummy.resid)
var_DJI_dummy = arch.arch_model(modelo_arma_DJI_BTC_dummy.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI_dummy.summary())

dummy_cond_var_DJI_BTC = var_DJI_dummy.conditional_volatility**2
h_dummy = (modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)[:'2020-03-20']
h_dummy = h_dummy.append(((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)['2020-03-20':])

dummy_variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI)/h_dummy).dropna()[:'2020-03-20']
dummy_variance_ratio_DJI_BTC = dummy_variance_ratio_DJI_BTC.append((((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI)/h_dummy).dropna()['2020-03-20':])
plt.plot(dummy_variance_ratio_DJI_BTC)
plt.title("Variance ratio with dummies DJI into BTC")
plt.show()

dummy_VR_array = dummy_variance_ratio_DJI_BTC.to_numpy()
dummy_VR_array.mean()
dummy_VR_array.std()

##GSPC BTC#############################################
modelo_arma_DJI = ARIMA(data["GSPC"].dropna(),order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())
var_DJI = arch.arch_model(modelo_arma_DJI.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI = var_DJI.conditional_volatility**2

outer_var_BTC = pd.concat([data["GSPC"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(4) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["BTC"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["BTC"],exog=data_model_BTC[["DJI","DJI resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC.summary())

het_arch(resid=modelo_arma_DJI_BTC.resid, nlags=4)

var_DJI_BTC = arch.arch_model(modelo_arma_DJI_BTC.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI_BTC = var_DJI_BTC.conditional_volatility**2

h = modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI + cond_var_DJI_BTC

variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI)/h).dropna()
plt.plot(variance_ratio_DJI_BTC)
plt.title("Variance ratio GSPC into BTC")
plt.show()

VR_array = variance_ratio_DJI_BTC.to_numpy()
VR_array.mean()
VR_array.std()

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["BTC"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())

het_arch(modelo_arma_DJI_BTC_dummy.resid)
var_DJI_dummy = arch.arch_model(modelo_arma_DJI_BTC_dummy.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI_dummy.summary())

dummy_cond_var_DJI_BTC = var_DJI_dummy.conditional_volatility**2
h_dummy = (modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)[:'2020-03-20']
h_dummy = h_dummy.append(((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)['2020-03-20':])

dummy_variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI)/h_dummy).dropna()[:'2020-03-20']
dummy_variance_ratio_DJI_BTC = dummy_variance_ratio_DJI_BTC.append((((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI)/h_dummy).dropna()['2020-03-20':])
plt.plot(dummy_variance_ratio_DJI_BTC)
plt.title("Variance ratio with dummies DJI into BTC")
plt.show()

dummy_VR_array = dummy_variance_ratio_DJI_BTC.to_numpy()
dummy_VR_array.mean()
dummy_VR_array.std()

#GSPC_ETH#################################################################
modelo_arma_DJI = ARIMA(data["GSPC"].dropna(),order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())
var_DJI = arch.arch_model(modelo_arma_DJI.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI = var_DJI.conditional_volatility**2

outer_var_BTC = pd.concat([data["GSPC"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(1) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["ETH"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["ETH"],exog=data_model_BTC[["DJI","DJI resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC.summary())

het_arch(resid=modelo_arma_DJI_BTC.resid, nlags=4)

var_DJI_BTC = arch.arch_model(modelo_arma_DJI_BTC.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI_BTC = var_DJI_BTC.conditional_volatility**2

h = modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI + cond_var_DJI_BTC

variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI)/h).dropna()
plt.plot(variance_ratio_DJI_BTC)
plt.title("Variance ratio GSPC into BTC")
plt.show()

VR_array = variance_ratio_DJI_BTC.to_numpy()
VR_array.mean()
VR_array.std()

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["ETH"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())

het_arch(modelo_arma_DJI_BTC_dummy.resid)
var_DJI_dummy = arch.arch_model(modelo_arma_DJI_BTC_dummy.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI_dummy.summary())

dummy_cond_var_DJI_BTC = var_DJI_dummy.conditional_volatility**2
h_dummy = (modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)[:'2020-03-20']
h_dummy = h_dummy.append(((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)['2020-03-20':])

dummy_variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI)/h_dummy).dropna()[:'2020-03-20']
dummy_variance_ratio_DJI_BTC = dummy_variance_ratio_DJI_BTC.append((((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI)/h_dummy).dropna()['2020-03-20':])
plt.plot(dummy_variance_ratio_DJI_BTC)
plt.title("Variance ratio with dummies DJI into BTC")
plt.show()

dummy_VR_array = dummy_variance_ratio_DJI_BTC.to_numpy()
dummy_VR_array.mean()
dummy_VR_array.std()

#GSPC_XRP#################################################################


#GSPC_XMR#################################################################
modelo_arma_DJI = ARIMA(data["GSPC"].dropna(),order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())
var_DJI = arch.arch_model(modelo_arma_DJI.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI = var_DJI.conditional_volatility**2

outer_var_BTC = pd.concat([data["GSPC"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(2) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["XMR"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["XMR"],exog=data_model_BTC[["DJI","DJI resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC.summary())

het_arch(resid=modelo_arma_DJI_BTC.resid, nlags=4)

var_DJI_BTC = arch.arch_model(modelo_arma_DJI_BTC.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI_BTC = var_DJI_BTC.conditional_volatility**2

h = modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI + cond_var_DJI_BTC

variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI)/h).dropna()
plt.plot(variance_ratio_DJI_BTC)
plt.title("Variance ratio GSPC into BTC")
plt.show()

VR_array = variance_ratio_DJI_BTC.to_numpy()
VR_array.mean()
VR_array.std()

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["XMR"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())

het_arch(modelo_arma_DJI_BTC_dummy.resid)
var_DJI_dummy = arch.arch_model(modelo_arma_DJI_BTC_dummy.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI_dummy.summary())

dummy_cond_var_DJI_BTC = var_DJI_dummy.conditional_volatility**2
h_dummy = (modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)[:'2020-03-20']
h_dummy = h_dummy.append(((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)['2020-03-20':])

dummy_variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI)/h_dummy).dropna()[:'2020-03-20']
dummy_variance_ratio_DJI_BTC = dummy_variance_ratio_DJI_BTC.append((((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI)/h_dummy).dropna()['2020-03-20':])
plt.plot(dummy_variance_ratio_DJI_BTC)
plt.title("Variance ratio with dummies DJI into BTC")
plt.show()

dummy_VR_array = dummy_variance_ratio_DJI_BTC.to_numpy()
dummy_VR_array.mean()
dummy_VR_array.std()

#IXIC_BTC##############################################################################
modelo_arma_DJI = ARIMA(data["IXIC"].dropna(),order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())
var_DJI = arch.arch_model(modelo_arma_DJI.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI = var_DJI.conditional_volatility**2

outer_var_BTC = pd.concat([data["IXIC"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(-2) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["BTC"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["BTC"],exog=data_model_BTC[["DJI","DJI resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC.summary())

het_arch(resid=modelo_arma_DJI_BTC.resid, nlags=4)

var_DJI_BTC = arch.arch_model(modelo_arma_DJI_BTC.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI_BTC = var_DJI_BTC.conditional_volatility**2

h = modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI.shift(-2) + cond_var_DJI_BTC

variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI.shift(-2))/h).dropna()
plt.plot(variance_ratio_DJI_BTC)
plt.title("Variance ratio GSPC into BTC")
plt.show()

VR_array = variance_ratio_DJI_BTC.to_numpy()
VR_array.mean()
VR_array.std()

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["BTC"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())

het_arch(modelo_arma_DJI_BTC_dummy.resid)
var_DJI_dummy = arch.arch_model(modelo_arma_DJI_BTC_dummy.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI_dummy.summary())

dummy_cond_var_DJI_BTC = var_DJI_dummy.conditional_volatility**2
h_dummy = (modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)[:'2020-03-20']
h_dummy = h_dummy.append(((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)['2020-03-20':])

dummy_variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI)/h_dummy).dropna()[:'2020-03-20']
dummy_variance_ratio_DJI_BTC = dummy_variance_ratio_DJI_BTC.append((((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI)/h_dummy).dropna()['2020-03-20':])
plt.plot(dummy_variance_ratio_DJI_BTC)
plt.title("Variance ratio with dummies DJI into BTC")
plt.show()

dummy_VR_array = dummy_variance_ratio_DJI_BTC.to_numpy()
dummy_VR_array.mean()
dummy_VR_array.std()

#IXIC_BTC##############################################################################
modelo_arma_DJI = ARIMA(data["IXIC"].dropna(),order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())
var_DJI = arch.arch_model(modelo_arma_DJI.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI = var_DJI.conditional_volatility**2

outer_var_BTC = pd.concat([data["IXIC"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(2) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["BTC"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["BTC"],exog=data_model_BTC[["DJI","DJI resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC.summary())

het_arch(resid=modelo_arma_DJI_BTC.resid, nlags=4)

var_DJI_BTC = arch.arch_model(modelo_arma_DJI_BTC.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI_BTC = var_DJI_BTC.conditional_volatility**2

h = modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI + cond_var_DJI_BTC

variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI)/h).dropna()
plt.plot(variance_ratio_DJI_BTC)
plt.title("Variance ratio GSPC into BTC")
plt.show()

VR_array = variance_ratio_DJI_BTC.to_numpy()
VR_array.mean()
VR_array.std()

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["BTC"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())

het_arch(modelo_arma_DJI_BTC_dummy.resid)
var_DJI_dummy = arch.arch_model(modelo_arma_DJI_BTC_dummy.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI_dummy.summary())

dummy_cond_var_DJI_BTC = var_DJI_dummy.conditional_volatility**2
h_dummy = (modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)[:'2020-03-20']
h_dummy = h_dummy.append(((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)['2020-03-20':])

dummy_variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI)/h_dummy).dropna()[:'2020-03-20']
dummy_variance_ratio_DJI_BTC = dummy_variance_ratio_DJI_BTC.append((((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI)/h_dummy).dropna()['2020-03-20':])
plt.plot(dummy_variance_ratio_DJI_BTC)
plt.title("Variance ratio with dummies DJI into BTC")
plt.show()

dummy_VR_array = dummy_variance_ratio_DJI_BTC.to_numpy()
dummy_VR_array.mean()
dummy_VR_array.std()

#IXIC_ETH##############################################################################
modelo_arma_DJI = ARIMA(data["IXIC"].dropna(),order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())
var_DJI = arch.arch_model(modelo_arma_DJI.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI = var_DJI.conditional_volatility**2

outer_var_BTC = pd.concat([data["IXIC"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(2) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["ETH"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["ETH"],exog=data_model_BTC[["DJI","DJI resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC.summary())

het_arch(resid=modelo_arma_DJI_BTC.resid, nlags=4)

var_DJI_BTC = arch.arch_model(modelo_arma_DJI_BTC.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI_BTC = var_DJI_BTC.conditional_volatility**2

h = modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI + cond_var_DJI_BTC

variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI)/h).dropna()
plt.plot(variance_ratio_DJI_BTC)
plt.title("Variance ratio GSPC into BTC")
plt.show()

VR_array = variance_ratio_DJI_BTC.to_numpy()
VR_array.mean()
VR_array.std()

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["ETH"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())

het_arch(modelo_arma_DJI_BTC_dummy.resid)
var_DJI_dummy = arch.arch_model(modelo_arma_DJI_BTC_dummy.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI_dummy.summary())

dummy_cond_var_DJI_BTC = var_DJI_dummy.conditional_volatility**2
h_dummy = (modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)[:'2020-03-20']
h_dummy = h_dummy.append(((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)['2020-03-20':])

dummy_variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI)/h_dummy).dropna()[:'2020-03-20']
dummy_variance_ratio_DJI_BTC = dummy_variance_ratio_DJI_BTC.append((((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI)/h_dummy).dropna()['2020-03-20':])
plt.plot(dummy_variance_ratio_DJI_BTC)
plt.title("Variance ratio with dummies DJI into BTC")
plt.show()

dummy_VR_array = dummy_variance_ratio_DJI_BTC.to_numpy()
dummy_VR_array.mean()
dummy_VR_array.std()


#IXIC_XRP##############################################################################
modelo_arma_DJI = ARIMA(data["IXIC"].dropna(),order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())
var_DJI = arch.arch_model(modelo_arma_DJI.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI = var_DJI.conditional_volatility**2

outer_var_BTC = pd.concat([data["IXIC"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(2) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["XRP"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["XRP"],exog=data_model_BTC[["DJI","DJI resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC.summary())

het_arch(resid=modelo_arma_DJI_BTC.resid, nlags=4)

var_DJI_BTC = arch.arch_model(modelo_arma_DJI_BTC.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI_BTC = var_DJI_BTC.conditional_volatility**2

h = modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI + cond_var_DJI_BTC

variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI)/h).dropna()
plt.plot(variance_ratio_DJI_BTC)
plt.title("Variance ratio GSPC into BTC")
plt.show()

VR_array = variance_ratio_DJI_BTC.to_numpy()
VR_array.mean()
VR_array.std()

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["XRP"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())

het_arch(modelo_arma_DJI_BTC_dummy.resid)
var_DJI_dummy = arch.arch_model(modelo_arma_DJI_BTC_dummy.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI_dummy.summary())

dummy_cond_var_DJI_BTC = var_DJI_dummy.conditional_volatility**2
h_dummy = (modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)[:'2020-03-20']
h_dummy = h_dummy.append(((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)['2020-03-20':])

dummy_variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI)/h_dummy).dropna()[:'2020-03-20']
dummy_variance_ratio_DJI_BTC = dummy_variance_ratio_DJI_BTC.append((((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI)/h_dummy).dropna()['2020-03-20':])
plt.plot(dummy_variance_ratio_DJI_BTC)
plt.title("Variance ratio with dummies DJI into BTC")
plt.show()

dummy_VR_array = dummy_variance_ratio_DJI_BTC.to_numpy()
dummy_VR_array.mean()
dummy_VR_array.std()


#IXIC_XMR##############################################################################
modelo_arma_DJI = ARIMA(data["IXIC"].dropna(),order=(1,0,0)).fit()
print(modelo_arma_DJI.summary())
var_DJI = arch.arch_model(modelo_arma_DJI.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI = var_DJI.conditional_volatility**2

outer_var_BTC = pd.concat([data["IXIC"], modelo_arma_DJI.resid], axis=1).fillna(0).shift(2) #lag 2 debvido a causalidade
outer_var_BTC.columns = ["DJI", "DJI resid"]
data_model_BTC = pd.concat([outer_var_BTC,data["XMR"]],axis=1).dropna(how='any')
modelo_arma_DJI_BTC = ARIMA(endog=data_model_BTC["XMR"],exog=data_model_BTC[["DJI","DJI resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC.summary())

het_arch(resid=modelo_arma_DJI_BTC.resid, nlags=4)

var_DJI_BTC = arch.arch_model(modelo_arma_DJI_BTC.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI.summary())
cond_var_DJI_BTC = var_DJI_BTC.conditional_volatility**2

h = modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI + cond_var_DJI_BTC

variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC.params["DJI resid"]**2 * cond_var_DJI)/h).dropna()
plt.plot(variance_ratio_DJI_BTC)
plt.title("Variance ratio GSPC into BTC")
plt.show()

VR_array = variance_ratio_DJI_BTC.to_numpy()
VR_array.mean()
VR_array.std()

dummy_DJI = pd.Series()
dummy_DJI = data_model_BTC["DJI"].copy(deep=True)
dummy_DJI[:'2020-03-20'] = 0
dummy_resid=pd.Series()
dummy_resid = data_model_BTC["DJI resid"].copy(deep=True)
dummy_resid[:'2020-03-20'] = 0
dummies = pd.concat([dummy_DJI,dummy_resid], axis = 1)
dummies.columns = ["dummy_DJI", "dummy_resid"]
dummy_data_model_BTC = pd.concat([data_model_BTC, dummies], axis = 1).dropna(how='any')
modelo_arma_DJI_BTC_dummy = ARIMA(endog=dummy_data_model_BTC["XMR"],exog=dummy_data_model_BTC[["DJI","DJI resid","dummy_DJI","dummy_resid"]], order=(0,0,0)).fit()
print(modelo_arma_DJI_BTC_dummy.summary())

het_arch(modelo_arma_DJI_BTC_dummy.resid)
var_DJI_dummy = arch.arch_model(modelo_arma_DJI_BTC_dummy.resid.dropna(), vol="EGARCH").fit(update_freq=5)
print(var_DJI_dummy.summary())

dummy_cond_var_DJI_BTC = var_DJI_dummy.conditional_volatility**2
h_dummy = (modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)[:'2020-03-20']
h_dummy = h_dummy.append(((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI + dummy_cond_var_DJI_BTC)['2020-03-20':])

dummy_variance_ratio_DJI_BTC = ((modelo_arma_DJI_BTC_dummy.params["DJI resid"]**2 * cond_var_DJI)/h_dummy).dropna()[:'2020-03-20']
dummy_variance_ratio_DJI_BTC = dummy_variance_ratio_DJI_BTC.append((((modelo_arma_DJI_BTC_dummy.params["DJI resid"]+modelo_arma_DJI_BTC_dummy.params["dummy_DJI"])**2 * cond_var_DJI)/h_dummy).dropna()['2020-03-20':])
plt.plot(dummy_variance_ratio_DJI_BTC)
plt.title("Variance ratio with dummies DJI into BTC")
plt.show()

dummy_VR_array = dummy_variance_ratio_DJI_BTC.to_numpy()
dummy_VR_array.mean()
dummy_VR_array.std()
"""