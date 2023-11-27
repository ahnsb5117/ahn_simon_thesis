library(xts)
library(pdfetch) #Library for loading FRED data
library(ggplot2) #Library for plotting
library(mFilter) #Library for HP filter
library(rollRegres) #Library for Regression 

#data_pc <- pdfetch_FRED(c("GDPC1", "UNRATE", "CPIAUCSL", "CPILFESL"))
data_pc_raw <- pdfetch_FRED(c("NGDPRSAXDCFRQ", "LRUN64TTFRQ156S", "FRACPIALLMINMEI", "FRACPICORMINMEI"))
#Date adjustment
data_pc <- data_pc_raw["2005-01-01/2020-04-01"]
# Convert data to quarterly frequency
data_pc <- to.period(data_pc, period = "quarter", OHLC = FALSE)
#View(data_pc)

#Transformations
data_pc$lgdp <- log(data_pc$NGDPRSAXDCFRQ) # Take logs
hp_gdp <- hpfilter(data_pc$lgdp, freq = 1600, type="lambda")
data_pc$gdpgap <- 100*hp_gdp$cycle
data_pc$l_cpi <- log(data_pc$FRACPIALLMINMEI) # Consumer Price Index of All Items in Canada
data_pc$l_cpi_core <- log(data_pc$FRACPICORMINMEI)  # Consumer Price Index of All Items Non-food and Non-energy in Canada
data_pc$unemployment_rate <- (data_pc$LRUN64TTFRQ156S)# seasonally adjusted

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
data_pc$supply_shock <- 4*diff(data_pc$l_cpi)*100 - 4*diff(data_pc$l_cpi_core)*100

model1 <- lm(infgap ~ unemployment_rate, data = data_pc)
summary(model1)
model2 <- lm(infgap ~ 0 + gdpgap, data = data_pc)
summary(model2)
model3 <- lm(infgap ~ unemployment_rate + supply_shock, data = data_pc)
summary(model3)


# Convert the coefficients part of the summary to a data frame
coefficients_df <- tidy(model3)

coefficients_df$significance <- ifelse(coefficients_df$p.value < 0.001, "***",
                                       ifelse(coefficients_df$p.value < 0.01, "**",
                                              ifelse(coefficients_df$p.value < 0.05, "*",
                                                     "")))

coefficients_df <- coefficients_df %>%
  dplyr::rename( t.value = statistic)


coefficients_df$estimate <- round(coefficients_df$estimate, 4)
coefficients_df$std.error <- round(coefficients_df$std.error, 4)
coefficients_df$t.value <- round(coefficients_df$t.value, 4)
coefficients_df$p.value <- round(coefficients_df$p.value, 4)
coefficients_df$term[coefficients_df$term == "(Intercept)"] <- "Inflation Gap Intercept"
coefficients_df$term[coefficients_df$term == "unemployment_rate"] <- "Unemployment Rate"
coefficients_df$term[coefficients_df$term == "supply_shock"] <- "Supply Shock"

# Create a gt table
coefficients_table <- flextable(coefficients_df)

# Add a caption note
note_text <- "Signif. codes: 0 '***' 0.001 '**' 0.01 '*'"
coefficients_table <- coefficients_table %>%
  add_footer_lines(note_text) %>% 
  fontsize(size = 8, part = "footer")

# Print the table
coefficients_table

#getting rid of NA values
data1 <- na.omit(data_pc)

pc_rolling <- roll_regres(data1$infgap ~ data1$unemployment_rate + data1$supply_shock, width = 40, do_downdates = TRUE)
data1$un_pi_gap <- data1$unemployment_rate + data1$infgap/(0.3399 *100)
#Note that 0.3399 was the estimated coefficient of unemployment rate in model 3.
plot.xts(data1$un_pi_gap)
#Get trend using the HP filter with high lambda (much higner than for business cycles frequencies)
data1_1 <- na.omit(data1)
hp_un_pi_gap <- hpfilter(data1_1$un_pi_gap, freq = 100, type="lambda") 

hpgap_dat <- data.frame(hp_un_pi_gap$trend) %>% 
  tibble::rownames_to_column("date") %>% 
  dplyr::rename(nairu = un_pi_gap)

data2 <- data.frame(data1) %>% 
  tibble::rownames_to_column("date")

data3 <- merge(hpgap_dat, data2, by ="date") %>% 
  tibble::column_to_rownames("date")

data4 <- as.xts(data3)

data5 <- na.omit(data4)
plot.xts(data5$unemployment_rate, col = "black", lwd = 3, main = "France NAIRU", main.timespan = FALSE, lty = 3, ylim = c(2, 14)) #unemployment rate
addSeries(data5$nairu, on = 1, col = "red", lwd = 1) # NAIRU
addLegend("topleft", on=1, 
          legend.names = c("Unemployment Rate", "NAIRU"), 
          lty=c(3, 1), lwd=c(3, 1),
          col=c("black", "red"))

