library(xts)
library(pdfetch) #Library for loading FRED data
library(ggplot2) #Library for plotting
library(mFilter) #Library for HP filter
library(rollRegres) #Library for Regres
library(tidyverse)

# Short-run GDP
  
  data_pc_raw <- pdfetch_FRED(c("GDPC1", "GDPPOT", "CPALTT01USM661S"))
  #Date adjustment
  data_pc <- data_pc_raw["1961-04-01/2023-10-26"]
  # Convert data to quarterly frequency
  data_pc <- to.period(data_pc, period = "quarter", OHLC = FALSE)

  data_pc$GDPPOT_2017 <- data_pc$GDPPOT/(228.326/243.839)
  data_pc$pct_dev <- (data_pc$GDPC1 - data_pc$GDPPOT_2017)/data_pc$GDPPOT_2017

  plot.xts(data_pc$pct_dev)
  
  df1 = data.frame(DATE=index(data_pc), coredata(data_pc)) %>% 
    mutate(cpi_num = as.numeric(CPALTT01USM661S)) %>% 
    mutate(inf_change = (cpi_num - lag(cpi_num)))
  
  
  # Short-run GDP before 2009
  
  #Date adjustment
  data_pc_before2009 <- data_pc_raw["1961-04-01/2008-12-31"]
  # Convert data to quarterly frequency
  data_pc_before2009 <- to.period(data_pc_before2009, period = "quarter", OHLC = FALSE)
  
  data_pc_before2009$GDPPOT_2017 <- data_pc_before2009$GDPPOT/(228.326/243.839)
  data_pc_before2009$pct_dev <- (data_pc_before2009$GDPC1 - data_pc_before2009$GDPPOT_2017)/data_pc_before2009$GDPPOT_2017
  
  plot.xts(data_pc$pct_dev)
  
  df1_before2009 = data.frame(DATE=index(data_pc_before2009), coredata(data_pc_before2009)) %>% 
    mutate(cpi_num = as.numeric(CPALTT01USM661S)) %>% 
    mutate(inf_change = (cpi_num - lag(cpi_num)))
  
  
  # Short-run GDP after 2008
  
  #Date adjustment
  data_pc_after2008 <- data_pc_raw["2009-01-01/2023-10-26"]
  # Convert data to quarterly frequency
  data_pc_after2008 <- to.period(data_pc_after2008, period = "quarter", OHLC = FALSE)
  
  data_pc_after2008$GDPPOT_2017 <- data_pc_after2008$GDPPOT/(228.326/243.839)
  data_pc_after2008$pct_dev <- (data_pc_after2008$GDPC1 - data_pc_after2008$GDPPOT_2017)/data_pc_after2008$GDPPOT_2017
  
  plot.xts(data_pc$pct_dev)
  
  df1_after2008 = data.frame(DATE=index(data_pc_after2008), coredata(data_pc_after2008)) %>% 
    mutate(cpi_num = as.numeric(CPALTT01USM661S)) %>% 
    mutate(inf_change = (cpi_num - lag(cpi_num)))
  
  # Short-run GDP precoivd
  
  #Date adjustment
  data_pc_precovid <- data_pc_raw["2008-01-01/2020-12-31"]
  # Convert data to quarterly frequency
  data_pc_precovid <- to.period(data_pc_precovid, period = "quarter", OHLC = FALSE)
  
  data_pc_precovid$GDPPOT_2017 <- data_pc_precovid$GDPPOT/(228.326/243.839)
  data_pc_precovid$pct_dev <- (data_pc_precovid$GDPC1 - data_pc_precovid$GDPPOT_2017)/data_pc_precovid$GDPPOT_2017
  
  plot.xts(data_pc$pct_dev)
  
  df1_precovid = data.frame(DATE=index(data_pc_precovid), coredata(data_pc_precovid)) %>% 
    mutate(cpi_num = as.numeric(CPALTT01USM661S)) %>% 
    mutate(inf_change = (cpi_num - lag(cpi_num)))

  
#Graph 1
ggplot(df1, aes(x = pct_dev, y = inf_change))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Short-Run GDP", y = "Change in Inflation", title = "1961-04-01/2008-12-31")

#Graph 2
ggplot(df1_before2009, aes(x = pct_dev, y = inf_change))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Short-Run GDP", y = "Change in Inflation", title = "1961-04-01/2008-12-31")

#Graph 3
ggplot(df1_after2008, aes(x = pct_dev, y = inf_change))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Short-Run GDP", y = "Change in Inflation", title = "2009-01-01/2023-10-26")

#Graph 3
ggplot(df1_precovid, aes(x = pct_dev, y = inf_change))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Short-Run GDP", y = "Change in Inflation", title = "2008-01-01/2020-12-31")

  

  

  
  

  