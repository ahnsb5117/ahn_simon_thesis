#G7 Unemplyoment Rate
library(tidyverse)
library(tidyquant)
library(janitor)
library(reshape2)


dat_us <- c("UNRATE", "LRUNTTTTGBQ156S", "LRUNTTTTCAM156S", "LRUNTTTTFRQ156S", 
            "LRUNTTTTITQ156S", "LRUNTTTTDEQ156S", "LRUNTTTTJPM156S") %>% 
  tq_get(get = "economic.data", from ="2005-01-01", to = "2020-01-01") %>% 
  mutate(Country = recode(symbol,
                          "UNRATE" = "U.S.",
                          "LRUNTTTTGBQ156S" = "U.K.",
                          "LRUNTTTTDEQ156S" = "Germany",
                          "LRUNTTTTFRQ156S" = "France",
                          "LRUNTTTTITQ156S" = "Italy",
                          "LRUNTTTTCAM156S" = "Canada",
                          "LRUNTTTTJPM156S" = "Japan"))

ggplot(dat_us, aes(x=date, y=price, color =Country, group=Country )) +
  geom_line() +
  scale_x_date(date_labels="%Y",date_breaks  ="24 month")+
  theme_bw()+
  labs(x = "Year", y = "Unemployment Rate (%)")


