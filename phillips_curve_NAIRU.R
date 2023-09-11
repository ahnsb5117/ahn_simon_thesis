library(xts)
library(pdfetch) #Library for loading FRED data
library(ggplot2) #Library for plotting
library(mFilter) #Library for HP filter

data_pc <- pdfetch_FRED(c("GDPC1", "UNRATE", "CPIAUCSL", "PPIACO", "CPILFESL"))

# Convert data to quarterly frequency
data_pc_xts <- to.period(data_pc, period = "quarter", OHLC = FALSE)
data_pc_df <- data.frame(data_pc_xts)

data_pc <- cbind(Date = rownames(data_pc_df), data_pc_df)

#Transformations
data_pc_xts$lgdp <- log(data_pc_xts$GDPC1) # Take logs
hp_gdp <- hpfilter(data_pc_xts$lgdp, freq = 1600, type="lambda")
data_pc_xts$gdpgap <- 100*hp_gdp$cycle
data_pc_xts$l_cpi <- log(data_pc_xts$CPIAUCSL)
data_pc_xts$l_cpi_core <- log(data_pc_xts$CPILFESL)
data_pc_xts$l_ppiaco <- log(data_pc_xts$PPIACO)
data_pc_xts$unrate <- (data_pc_xts$UNRATE)

#Series for plots of the Phillips curve

data_pc_xts$inflation <- 100*diff(data_pc$l_cpi, 4)


#plot.xts(data_pc$inflation)

#Add recession bars
recessions.df = read.table(textConnection(
  "Peak, Trough
  1957-08-01, 1958-04-01
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01
  2020-02-01, 2020-05-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

ggplot() +
  geom_line(data = data_pc, aes(x = Index, y = unrate, color = "Unemployment"), lwd = 1) +
  geom_line(data = data_pc$inflation, aes(x = Index, y = data_pc$inflation, color = "Inflation"), lwd = 1) +
  geom_rect(data=recessions.df, inherit.aes=F, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='darkgray', alpha=0.5) +
  theme_classic() +
  labs(title = "US Unemployment rate and Inflation", x = "Quarter", y = "") +
  labs(color="Legend") +
  theme(legend.position="bottom")

#Quarterly inflation, annualized
data_pc$QuarterlyInflation = 4*100*diff(data_pc$l_cpi)

#Inflation expectations as an average of 4 past y-o-y inflation rates
data_pc$InflationExpectations <- 1/4*(lag(data_pc$inflation, k=1) + lag(data_pc$inflation, k=2) + lag(data_pc$inflation, k=3) + lag(data_pc$inflation, k=4))

ggplot()+
  geom_line(data = data_pc$inflation, aes(x = Index, y = data_pc$inflation, color = "Inflation"), lwd = .8)+
  geom_line(data = data_pc$infexp, aes(x = Index, y = data_pc$infexp, color = "infexp", ), lwd = .8)+
  labs(title = "US Inflation Expectation",subtitle = "An average of four past year over year inflation rates", x = "Quarter", y = "") +
  labs(color="Legend") +
  theme_bw()

#Creating inflation gap
data_pc$InflationGap <- data_pc$QuarterlyInflation-data_pc$InflationExpectations

ggplot()+
  geom_line(data = data_pc$inflation, aes(x = Index, y = data_pc$QuarterlyInflation, color = "QuarterlyInflation"), lwd = .8)+
  geom_line(data = data_pc$infexp, aes(x = Index, y = data_pc$InflationGap, color = "InflationGap", ), lwd = .8)+
  labs(title = "Inflation Gap", x = "Quarterly Change", y = "") +
  labs(color="Legend") +
  scale_x_datetime(date_labels = '%b %Y', date_breaks = '3 months')


#Supply shocks
data_pc$SupplyShock1 <- 4*diff(data_pc$l_cpi)*100 - 4*diff(data_pc$l_cpi_core)*100
data_pc$SupplyShock2 <- 100*diff(data_pc$l_ppiaco)

model1 <- lm(InflationGap ~ unrate, data = data_pc)
summary(model1)

model2 <- lm(InflationGap ~ 0 + gdpgap, data = data_pc)
summary(model2)

model3a <- lm(InflationGap ~ unrate + SupplyShock1, data = data_pc)
summary(model3a)

model3b <- lm(InflationGap ~ unrate + SupplyShock2, data = data_pc)
summary(model3b)

model4 <- lm(InflationGap ~ 0 + gdpgap + SupplyShock1, data = data_pc)
summary(model4)

library(rollRegres)
data1 <- na.omit(data_pc)
pc_rolling <- roll_regres(data1$infgap ~ data1$unrate + data1$ss1, width = 40, do_downdates = TRUE)
data1$slope <- pc_rolling$coefs[1:255, 2:2]

plot.xts(data1$slope)


data_pc$un_pi_gap <- data_pc$unrate + data_pc$infgap/0.3
#Note that -0.3 was the estimated coefficient at unemployment rate in model 3a.
plot.xts(data_pc$un_pi_gap)
#Get trend using the HP filter with high lambda (much higner than for business cycles frequencies)
hp_un_pi_gap <- hpfilter(data1$un_pi_gap, freq = 100000, type="lambda")
plot(hp_un_pi_gap)

#This plot however hasn't been very informative...
data1$nairu <- hp_un_pi_gap$trend
plot.xts(data1$unrate, col = "black", lwd = 2)
addSeries(data1$nairu, on = 1, col = "red", lwd = 2 )