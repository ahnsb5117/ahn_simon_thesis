library(xts)
library(pdfetch) #Library for loading FRED data
library(ggplot2) #Library for plotting
library(mFilter) #Library for HP filter

#data_pc <- pdfetch_FRED(c("GDPC1", "UNRATE", "CPIAUCSL", "CPILFESL"))
data_pc <- pdfetch_FRED(c("JPNRGDPEXP", "LRUN64TTJPM156S", "JPNCPIALLMINMEI", "JPNCPICORMINMEI"))
# Convert data to quarterly frequency
data_pc <- to.period(data_pc, period = "quarter", OHLC = FALSE)
#View(data_pc)

#Transformations
data_pc$lgdp <- log(data_pc$JPNRGDPEXP) # Take logs
hp_gdp <- hpfilter(data_pc$lgdp, freq = 1600, type="lambda")
data_pc$gdpgap <- 100*hp_gdp$cycle 
data_pc$l_cpi <- log(data_pc$JPNCPIALLMINMEI) # Consumer Price Index of All Items in Japan
data_pc$l_cpi_core <- log(data_pc$JPNCPICORMINMEI)
data_pc$unrate <- (data_pc$LRUN64TTJPM156S) # seasonally adjusted

#Quarterly inflation, annualized
data_pc$inflation_q = 4*100*diff(data_pc$l_cpi)

#Inflation expectations as an average of 4 past y-o-y inflation rates
data_pc$infexp <- 1/4*(lag(data_pc$inflation, k=1) + lag(data_pc$inflation, k=2) + lag(data_pc$inflation, k=3) + lag(data_pc$inflation, k=4))

plot.xts(data_pc$inflation, col = "black", lwd = 2)
addSeries(data_pc$infexp, on = 1, col = "red", lwd = 2 )

#Creating inflation gap
data_pc$infgap <- data_pc$inflation_q-data_pc$infexp
plot.xts(data_pc$inflation_q)
addSeries(data_pc$infgap, on = 1, col = "red", lwd = 2 )

#Supply shocks
data_pc$ss1 <- 4*diff(data_pc$l_cpi)*100 - 4*diff(data_pc$l_cpi_core)*100

model1 <- lm(infgap ~ unrate, data = data_pc)
summary(model1)
model2 <- lm(infgap ~ 0 + gdpgap, data = data_pc)
summary(model2)

