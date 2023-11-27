library(flextable)

# Create a data frame with the provided data
data <- data.frame(
  "Variable/Countries" = c("Real GDP", "Unemployment Rate", "Headline CPI", "Core CPI"),
  "Canada" = c("NGDPRSAXDCCAQ", "LRUNTTTTCAM156S", "CANCPIALLMINMEI", "CANCPICORMINMEI"),
  "France" = c("NGDPRSAXDCFRQ", "LRUN64TTFRQ156S", "FRACPIALLMINMEI", "FRACPICORMINMEI"),
  "Germany" = c("CLVMNACSCAB1GQDE", "LRUN64TTDEQ156S", "DEUCPIALLMINMEI", "DEUCPICORMINMEI"),
  "Italy" = c("NGDPRSAXDCITQ", "LRUNTTTTITQ156S", "ITACPIALLMINMEI", "ITACPICORMINMEI"),
  "Japan" = c("JPNRGDPEXP", "LRUN64TTJPM156S", "JPNCPIALLMINMEI", "JPNCPICORMINMEI"),
  "U.K." = c("NGDPRSAXDCGBQ", "LRUNTTTTGBQ156S", "GBRCPIALLMINMEI", "GBRCPICORMINMEI"),
  "U.S." = c("GDPC1", "UNRATE", "CPIAUCSL", "CPILFESL")
)

# Create a flextable
ft <- flextable(data)

# Print the flextable
ft