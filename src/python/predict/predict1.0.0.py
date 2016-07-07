from sqlalchemy import create_engine
from pandas import DataFrame
from numpy import arange, argmax, array

user = 'dima'
password = '5nfUNO732'
host = '185.128.234.3'
database = 'dima_db'

engine = create_engine('mysql://{0}:{1}@{2}/{3}'.format(user, password, host, database))

data = []
for row in engine.execute('SELECT * FROM predictTable'):
    data.append(dict(row))
data = DataFrame(data)

X = data.drop('y', 1)
product = X['ProductID']
X = X.drop('ProductID', 1)
weight = X.mounth_publish
X = X.drop('mounth_publish', 1)
y = data['y']
y_class = y>0
price = X.price_real

def prof(x):
    x=float(x)
    if x<20:
        return 1+0.15*x
    else:
        if x<70:
            return 2+0.1*x
        else:
            if x<200:
                return 5+0.06*x
            else:
                return 7+0.05*x

profit = array(map(prof, price))

from numpy import maximum

alfa = 0.95

myWeight = alfa**maximum((array(weight-9)),0)

from sklearn.ensemble import RandomForestClassifier
from numpy import hstack, array, unique


rf = RandomForestClassifier(n_estimators = 1000,
                            class_weight = 'balanced',
                            min_samples_split = 5)


rf.fit(X, y_class, myWeight)

pred = rf.predict_proba(X)

prob = pred[:,1]

predProfit = prob*profit

def my_int(x):
    if x != 0:
        return int(1./x)
    return -1

predData = DataFrame(data.ProductID)
predData.columns = ['ProductID']
predData['mounth_publish'] = weight
predData['price'] = X.price_real
predData['profit'] = profit
predData['prob'] = prob
predData['profit_from_publish'] = predProfit
predData['mean_time_to_sold'] = map(my_int, prob)

delta_prob = 1.16
predData['new_prob'] = map(lambda x: x*delta_prob/(1+x*(delta_prob-1)), predData.prob)
predData['new_profit_from_publish'] = predData.new_prob*profit 
predData['sold'] = y

group = predData.groupby(by = 'ProductID')
res = group.last()
res['ProductID'] = res.index

conn = engine.connect()

res.to_sql('predict',
           con = conn,
           flavor='mysql', 
           if_exists='replace',
           index=False)

conn.close()

































