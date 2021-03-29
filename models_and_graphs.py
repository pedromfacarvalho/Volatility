import pandas as pd
from statsmodels.tsa.arima.model import ARIMA
import arch
import matplotlib.pyplot as plt
import warnings
import yfinance as yf
from sklearn.preprocessing import MinMaxScaler
import numpy as np

date_beginning = '2020-02-20'
date_ending = '2020-04-07'
pre_date_beginning = '2020-02-19'
post_date_ending = '2020-04-08'

lista_data = []
for i in ["^DJI", "^GSPC", "^IXIC", "BTC-USD", "ETH-USD", "XRP-USD", "XMR-USD"]:
    tickerData = yf.Ticker(i)
    lista_data.append(tickerData.history(period='1d', start='2002-1-1').fillna(method='ffill'))

def scaler_timeseries(time_series):
    scaler = MinMaxScaler(feature_range=(-1,1))
    scaled_data = scaler.fit_transform(X=time_series.to_numpy().reshape(-1, 1))
    data_scaled_indexed = pd.Series(scaled_data.reshape(1, -1)[0], index=time_series.index)
    return data_scaled_indexed

data_price = pd.DataFrame(columns = ["DJI", "GSPC", "IXIC", "BTC", "ETH", "XRP", "XMR"])
for i,val in enumerate(lista_data):
    data_price[data_price.columns[i]] = val['Close']


data = np.log(data_price) - np.log(data_price.shift(1))


scaled_data = pd.DataFrame()
for i in data.columns:
    scaled_data[i] = scaler_timeseries(data[i])


scaled_index = scaled_data["DJI"]
scaled_crypto = scaled_data["BTC"]
order_index = (1,0,0)
order_crypto = (0,0,0)
lag = 2

#def models_charts(scaled_index, scaled_crypto, order_index, order_crypto, lag):

modelo_index = ARIMA(scaled_index, order=order_index).fit()
print(modelo_index.summary())
modelo_index_garch = arch.arch_model(modelo_index.resid.dropna(), vol="GARCH").fit()
print(modelo_index_garch.summary())
cond_var_index = modelo_index_garch.conditional_volatility ** 2

exog_crypto = pd.concat([scaled_index, modelo_index.resid], axis=1).shift(-1 * lag)  # lag 2 debvido a causalidade
exog_crypto.columns = ["Index", "Index resid"]
dados_crypto = pd.concat([exog_crypto, scaled_crypto], axis=1).dropna(how='any')
modelo_crypto = ARIMA(endog=dados_crypto[scaled_crypto.name], exog=dados_crypto[["Index", "Index resid"]],
                      order=order_crypto).fit()
print(modelo_crypto.summary())
modelo_crypto_garch = arch.arch_model(modelo_crypto.resid.dropna(), vol="GARCH", dist='normal').fit()
print(modelo_crypto_garch.summary())

cond_var_crypto = modelo_crypto_garch.conditional_volatility ** 2

h = modelo_crypto.params["Index resid"] ** 2 * cond_var_index.shift(-1 * lag) + cond_var_crypto

variance_ratio = ((modelo_crypto.params["Index resid"] ** 2 * cond_var_index.shift(-1 * lag)) / h).dropna()
plt.plot(variance_ratio)
plt.title(f"Variance ratio {scaled_index.name} into {scaled_crypto.name}]")
plt.show()

dummy_index = pd.Series(0, index=scaled_index.index, name="Dummy index")
dummy_index[date_beginning:date_ending] = 1
modelo_index_dummy = ARIMA(endog=scaled_index, exog=dummy_index, order=order_index).fit()
print(modelo_index_dummy.summary())
modelo_index_garch_dummy = arch.arch_model(modelo_index_dummy.resid.dropna(), vol="GARCH").fit()
print(modelo_index_garch_dummy.summary())
cond_var_index_dummy = modelo_index_garch_dummy.conditional_volatility ** 2


dummy_resid = pd.Series(dummy_index, index= scaled_crypto.index, name="Dummy crypto")
exog_crypto_dummy = pd.concat([scaled_index, modelo_index_dummy.resid, dummy_resid], axis=1).shift(-1 * lag)
exog_crypto_dummy.columns = ["Index", "Index resid", "Dummy crypto"]
dados_crypto_dummy = pd.concat([exog_crypto_dummy, scaled_crypto], axis=1).dropna(how='any')
modelo_crypto_dummy = ARIMA(endog=dados_crypto_dummy[scaled_crypto.name],
                            exog=dados_crypto_dummy[["Index", "Index resid", "Dummy crypto"]],
                            order=(0, 0, 0),).fit()
# with mod.fix_params(params=modelo_arma_DJI_BTC.params.to_dict()):
#   modelo_arma_DJI_BTC_dummy = mod.fit()
print(modelo_crypto_dummy.summary())

modelo_crypto_garch_dummy = arch.arch_model(modelo_crypto_dummy.resid, vol="GARCH").fit()
print(modelo_crypto_garch_dummy.summary())
cond_var_crypto_garch_dummy = modelo_crypto_garch_dummy.conditional_volatility ** 2

h_dummy = (modelo_crypto_dummy.params["Index resid"] ** 2 * cond_var_index_dummy.shift(-1*lag) + cond_var_crypto_garch_dummy)[:pre_date_beginning]
h_dummy = pd.concat([h_dummy,((modelo_crypto_dummy.params["Index resid"] + modelo_crypto_dummy.params["Dummy crypto"]) ** 2
                          * cond_var_index_dummy.shift(-1 * lag)[date_beginning:date_ending] + cond_var_crypto_garch_dummy[date_beginning:date_ending])])
h_dummy = pd.concat([h_dummy,(((modelo_crypto_dummy.params["Index resid"] ** 2) * cond_var_index_dummy.shift(-1*lag)) + cond_var_crypto_garch_dummy)[post_date_ending:]])


variance_ratio_dummy = ((modelo_crypto_dummy.params["Index resid"] ** 2 * cond_var_index_dummy.shift(-1*lag))/h_dummy)[:pre_date_beginning]
variance_ratio_dummy = pd.concat([variance_ratio_dummy, (((modelo_crypto_dummy.params["Index resid"] + modelo_crypto_dummy.params["Dummy crypto"]) ** 2 * cond_var_index_dummy.shift(-1*lag))/h_dummy)[date_beginning:date_ending]])
variance_ratio_dummy = pd.concat([variance_ratio_dummy, ((modelo_crypto_dummy.params["Index resid"] ** 2 * cond_var_index_dummy.shift(-1*lag))/h_dummy)[post_date_ending:]])

plt.plot(variance_ratio_dummy)

plt.title(f"Variance ratio with dummies {scaled_index.name} into {scaled_crypto.name}")
plt.show()


