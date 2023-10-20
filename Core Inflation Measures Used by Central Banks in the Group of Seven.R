library(readxl)
library(flextable)
library(dplyr)

g7<- read_excel("Core Inflation Measures Used by Central Banks in the Group of Seven.xlsx")
ft <- flextable(g7) %>% 
  autofit()


