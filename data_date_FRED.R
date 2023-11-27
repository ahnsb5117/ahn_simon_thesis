library(flextable)

# Create a data frame with the provided data
data <- data.frame(
  Variable = c("Real GDP", "Unemployment Rate", "Headline CPI", "Core CPI"),
  Canada = c("1961", 1976, 1955, 1961),
  France = c("1980", 2003, 1955, 1960),
  Germany = c("1991", 2005, 1955, 1962),
  Italy = c("1996", 1998, 1955, 1960),
  Japan = c("1994", 1970, 1955, 1957),
  U.K. = c("1955", 1971, 1955, 1970),
  U.S. = c("1947", 1948, 1947, 1957)
)

# Create a flextable
ft <- flextable(data)

# Print the flextable
ft



