import pandas as pd
import holidays
import os
from datetime import timedelta
os.chdir("/home/onyxia/work/train_delays/")

df=pd.read_csv("data/raw/pt_results.csv")
df.BETRIEBSTAG=pd.to_datetime(df.BETRIEBSTAG, format='%d.%m.%Y')

df_day=pd.DataFrame({'day': pd.date_range(min(df.BETRIEBSTAG),max(df.BETRIEBSTAG)-timedelta(days=1),freq='d')})

df_day['holiday_zurich']=df_day.day.apply(lambda x: x in holidays.country_holidays('CH', subdiv='ZH'))
df_day['holiday_munich']=df_day.day.apply(lambda x: x in holidays.country_holidays('DE', subdiv='BY'))
df_day.to_csv("data/raw/holidays.csv", index=False)
