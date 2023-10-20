#International Inflation Rate
library(tidyverse)
library(tidyquant)
library(janitor)
library(reshape2)


dat_us <- c("USACPALTT01CTGYM", "CPALTT01GBM659N", "DEUCPALTT01CTGYM", "CPALTT01FRM659N", 
            "ITACPALTT01CTGYM", "CPALTT01CAM659N", "FPCPITOTLZGJPN") %>% 
  tq_get(get = "economic.data", from ="2005-01-01", to = "2020-01-01") %>% 
  mutate(Country = recode(symbol,
                          "USACPALTT01CTGYM" = "U.S.",
                          "CPALTT01GBM659N" = "U.K.",
                          "DEUCPALTT01CTGYM" = "Germany",
                          "CPALTT01FRM659N" = "France",
                          "ITACPALTT01CTGYM" = "Italy",
                          "CPALTT01CAM659N" = "Canada",
                          "FPCPITOTLZGJPN" = "Japan"))

ggplot(dat_us, aes(x=date, y=price, color =Country, group=Country )) +
  geom_line() +
  scale_x_date(date_labels="%Y",date_breaks  ="24 month")+
  theme_bw()+
  labs(x = "Year", y = "Headline Inflation (%)")


