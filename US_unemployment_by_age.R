#unemployment by age
library(pdfetch) #Library for loading FRED data
library(ggplot2)
library(xts) # Library for plotting
library(broom)
library(tidyr)
library(scales)
library(dplyr)


#16-24, 24-54, 55-65
data_pc_raw <- pdfetch_FRED(c("LNS14024887", "LNS14000060", "LNS14024230","USREC"))
#Date adjustment
data_pc <- data_pc_raw["2005-01-01/2020-02-01"]
# Convert data to quarterly frequency
data_pc <- to.period(data_pc, period = "months", OHLC = FALSE)
#View(data_pc)

recessions.df = read.table(textConnection(
  "Peak, Trough
  2007-12-01, 2009-06-01
  2020-02-01, 2020-05-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)


ggplot() +
  geom_line(data = data_pc$LNS14024887, aes(x = Index, y = data_pc$LNS14024887,  linetype = "16 to 24 years"), lwd = 1) +
  geom_line(data = data_pc$LNS14000060, aes(x = Index, y = data_pc$LNS14000060, linetype = "25 to 54 years"), lwd = 1) +
  geom_line(data = data_pc$LNS14024230, aes(x = Index, y = data_pc$LNS14024230,linetype = "55 years and older"), lwd = 1) +
  geom_rect(data=recessions.df, inherit.aes=F, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='darkgray', alpha=0.5) +
  theme_bw() +
  labs(color="Legend", linetype = "Legend") +
  theme(legend.position="bottom")+
  labs(x = "Year", y = "Unemployment Rate (%)")
