import yfinance as yf
from fbprophet import Prophet
from fbprophet.diagnostics import cross_validation
import pandas as pd
import matplotlib.pyplot as plt
from datetime import timedelta

lista_data = []
for i in ["^DJI", "^GSPC", "^IXIC", "BTC-USD", "ETH-USD", "XRP-USD", "XMR-USD"]:
    tickerData = yf.Ticker(i)
    lista_data.append(tickerData.history(period='1d', start='2002-1-1', end='2021-03-12'))

data_price = pd.DataFrame(columns = ["DJI", "GSPC", "IXIC", "BTC", "ETH", "XRP", "XMR"])
for i,val in enumerate(lista_data):
    data_price[data_price.columns[i]] = val['Close']

data_to_test = pd.DataFrame()
data_to_test["y"] = data_price["DJI"][:"2020-02-01"]
data_to_test.reset_index(inplace=True)
data_to_test.rename(columns={"Date": "ds"}, inplace=True)

plt.plot(data_price["DJI"][:"2020-02-01"])
plt.show()

model = Prophet(interval_width=0.95)
model.fit(data_to_test)

cutoffs = pd.to_datetime(['2019-09-01'])
df_cv2 = cross_validation(model, cutoffs=cutoffs, horizon='180 days')

from fbprophet.plot import plot_cross_validation_metric
fig = plot_cross_validation_metric(df_cv2, metric='mape')
plt.show()
"""
from fbprophet.diagnostics import cross_validation
df_cv = cross_validation(model, initial='730 days', period='180 days', horizon = '365 days')

fig1 = plot_cross_validation_metric(df_cv, metric='mape')
plt.show()
"""
future = model.make_future_dataframe(periods=600)
forecast = model.predict(future)


model.plot(forecast)

plt.plot(data_price["DJI"]["2020-02-01":], 'r-')
plt.xlim(data_price["DJI"]["2020-02-01":].index[0] - timedelta(weeks = 52), data_price.index[-1])
plt.show()

