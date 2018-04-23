#!/usr/bin/python
import subprocess
from datetime import datetime, date, time, timedelta

tdy = datetime.now()
todaystr = tdy.strftime("%Y%m%d")

def datestr(d, i):
  d2 = d + timedelta(days=i)
  return d2.strftime("%Y%m%d")

i=0
d = date(2013, 12, 07) # Google trends, seem to only work from this date, onwards.
loop_date = ''
while loop_date != todaystr:
  loop_date = datestr(d, i)
  #print loop_date
  subprocess.call(['./curl_trend.sh', loop_date])
  i=i+1
