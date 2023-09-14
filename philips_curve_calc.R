library(tidyverse)
library(tidyquant)
library(janitor)

dat_us <- c("UNRATE", "CPIAUCSL") %>% 
  tq_get(get = "economic.data", from ="1948-12-31")


#Change around the lag

#US
dat_us <- c("UNRATE", "CPIAUCSL") %>% 
  tq_get(get = "economic.data", from ="1948-12-31")

dat_infl <- dat_us %>% 
  pivot_wider(names_from = symbol, values_from = price) %>% 
  mutate(infl = 100 * (CPIAUCSL - lag(CPIAUCSL, 12)) / lag(CPIAUCSL, 12), 
        infl_change = infl - lag(infl, 1),
        decade = paste0(10* as.numeric(substr(year(date), 1,3)), "s")) %>% 
  drop_na() %>% 
  clean_names()

ggplot(dat_infl, aes(x = unrate, y = infl)) +
  geom_point(aes(color = decade), size =2 , alpha =.5) +
  geom_smooth(se = F, method = "gam", aes (color = decade), formula = y ~ poly(x, 2))+
  scale_color_brewer(palette = "Set1") +
  labs(x = "Unemployment (%)", y = "Inflation (%)") +
  theme_minimal()

#JAPAN
dat_jpn <- c("LRUN64TTJPM156S", "JPNCPIALLMINMEI") %>% 
  tq_get(get = "economic.data", from ="1960-01-01")

dat_infl_jpn <- dat_jpn %>% 
  pivot_wider(names_from = symbol, values_from = price) %>% 
  mutate(infl = 100 * (JPNCPIALLMINMEI - lag(JPNCPIALLMINMEI, 12)) / lag(JPNCPIALLMINMEI, 12), 
         infl_change = infl - lag(infl, 1),
         decade = paste0(10* as.numeric(substr(year(date), 1,3)), "s")) %>% 
  drop_na() %>% 
  rename(unrate = LRUN64TTJPM156S)

ggplot(dat_infl_jpn, aes(x = unrate, y = infl)) +
  geom_point(aes(color = decade), size =2 , alpha =.5) +
  geom_smooth(se = F, method = "gam", aes (color = decade), formula = y ~ poly(x, 2))+
  scale_color_brewer(palette = "Set1") +
  labs(x = "Unemployment (%)", y = "Inflation (%)") +
  theme_minimal()


#CANADA
dat_can <- c("LRUNTTTTCAM156S", "CANCPIALLMINMEI") %>% 
  tq_get(get = "economic.data", from ="1960-01-01")

dat_infl_can <- dat_can %>% 
  pivot_wider(names_from = symbol, values_from = price) %>% 
  mutate(infl = 100 * (CANCPIALLMINMEI - lag(CANCPIALLMINMEI, 12)) / lag(CANCPIALLMINMEI, 12), 
         infl_change = infl - lag(infl, 1),
         decade = paste0(10* as.numeric(substr(year(date), 1,3)), "s")) %>% 
  drop_na() %>% 
  rename(unrate = LRUNTTTTCAM156S)

ggplot(dat_infl_can, aes(x = unrate, y = infl)) +
  geom_point(aes(color = decade), size =2 , alpha =.5) +
  geom_smooth(se = F, method = "gam", aes (color = decade), formula = y ~ poly(x, 2))+
  scale_color_brewer(palette = "Set1") +
  labs(x = "Unemployment (%)", y = "Inflation (%)") +
  theme_minimal()

