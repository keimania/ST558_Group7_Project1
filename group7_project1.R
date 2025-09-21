library(httr)
library(tibble)
library(jsonlite)

# 1. helper function
helper <- function(year = 2022,
                   numeric_vars = c("AGEP", "PWGTP"),
                   categorical_vars = c("SEX"),
                   geography = "All",
                   geo_level = "*",
                   arguments = NULL) {
  
  # Input Validation
  ## year validation
  if (!is.numeric(year) || length(year) != 1 || year < 2010 || year > 2022) {
    stop("Error: 'year' must be a single number between 2010 and 2022.")
  }
  
  ## variable validation
  valid_numeric_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
  if (!all(numeric_vars %in% valid_numeric_vars)) {
    stop("Error: Invalid numeric variables requested.")
  }
  
  valid_categorical_vars <- c("FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "SEX")
  if (!all(categorical_vars %in% valid_categorical_vars)) {
    stop("Error: Invalid categorical variables requested.")
  }
  
  ## Geography validation : region, division, state should be lowercase
  valid_geography <- c("All", "region", "division", "state")
  if (!all(geography %in% valid_geography)) {
    stop("Error: Invalid geography requested.")
  }
  
  base <- paste("https://api.census.gov/data/", year, "/acs/acs1/pums", sep = "")
  
  # Construct query parameters
  ## Ensure PWGTP is always included for calculations
  all_vars <- unique(c("PWGTP", numeric_vars, categorical_vars))
  ## If geography is "All", do not include 'for' parameter
  if (geography == "All") {
    query_params <- c(
      list('get' = paste(all_vars, collapse = ",")),
      arguments
    )
    ## If geography is specified, include geography and geo level in 'for' parameter
  } else {
    query_params <- c(
      list('get' = paste(all_vars, collapse = ",")),
      list('for' = paste0(geography, ":", geo_level)),
      arguments
    )
  }
  
  # request data from API
  response <- GET(url = base, query = query_params)
  if (status_code(response) != 200) {
    stop("Error: The Census API returned an error. Status code: ", status_code(response))
  }
  parsed <- fromJSON(rawToChar(response$content))
  # Set Column names as first row and convert to tibble
  header <- parsed[1,]
  data <- parsed[-1,]
  colnames(data) <- header
  result_tibble <- as_tibble(data)
  return(result_tibble)
}

# Example 1: Single Year Query
message("Example 1: Single Year Query")

# a. Get data for region:3, year:2021, SCHL:24 including AGEP, GASP, SEX
data_2021 <- helper(
  year = 2021,
  numeric_vars = c("AGEP", "GASP"),
  categorical_vars = c("SEX"),
  geography = c("region"),
  geo_level = c("3"),
  arguments = list('SCHL' = "24")
)

print(head(data_2021))

# b. Get data for state:6, year:2022, JWTRNS:10 including GRPIP, JWAP, JWDP, JWMNP, FER, HHL, HISPEED, SCH
data_2022 <- helper(
  year = 2022,
  numeric_vars = c("GRPIP", "JWAP", "JWDP", "JWMNP"),
  categorical_vars = c("FER", "HHL", "HISPEED", "SCH"),
  geography = c("state"),
  geo_level = c("6"),
  arguments = list('JWTRNS' = "10")
)

print(head(data_2022))