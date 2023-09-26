#G7 Import and Exports to GDP trend
library(tidyverse)

data_GDP_G7<- read_csv("import_export_to_GDP_G7.csv")

data_GDP_G7 <- data_GDP_G7 %>% 
  mutate(ex_im_sum = EXPORT+ IMPORT) %>% 
  mutate(Country = recode(LOCATION, "CAN" = "Canada", "DEU" = "Germany",
                          "FRA" = "France", "GBR" = "U.K.",
                          "ITA" = "Italy", "JPN" = "Japan",
                          "USA" = "U.S."))

ggplot(data_GDP_G7, aes(x = TIME, y = ex_im_sum, group = Country)) + 
  geom_line(aes(linetype = Country)) +
  theme_bw() +
  labs(x = "Year", y = " Import and Export to GDP (%)")
