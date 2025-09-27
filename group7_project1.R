library(httr)
library(tibble)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(lubridate)


# --- NEW: Dynamically Generate Lookup Tables from API Data Dictionary ---

# Helper function to extract 'item' list for a given variable
extract_item <- function(variable_data, var_name) {
  # Extract the 'values'->'item' list for the specific variable
  item_list <- variable_data$variables[[var_name]]$values$item
  return(item_list)
}

# Function to create lookup tables for specified variables
create_lookup_tables <- function(year, var_names) {
  # 1. Construct the URL for the data dictionary JSON
  variables_url <- paste0("https://api.census.gov/data/", year, "/acs/acs1/pums/variables.json")
  
  message("Fetching data dictionary from: ", variables_url)
  
  # 2. GET the data and parse it
  response <- GET(variables_url)
  if (status_code(response) != 200) {
    stop("Failed to download the data dictionary. Status code: ", status_code(response))
  }
  
  variable_data <- fromJSON(content(response, "text"))
  
  # 3. Create a lookup list for each requested variable
  lookup_list <- lapply(setNames(nm = var_names), function(current_var) {
    extract_item(variable_data = variable_data, var_name = current_var)
  })
  
  return(lookup_list)}

# calculate midpoints from time ranges
time_range_to_midpoint <- function(time_range_str) {
  
  # "9:40 p.m. to 9:44 p.m." → c("9:40 p.m.", "9:44 p.m.")
  parts <- str_split(time_range_str, " to ", simplify = TRUE)
  # if there are not exactly two parts, return original string
  if (length(parts) != 2) return(NA_POSIXct_)
  
  # a.m. -> am, p.m -> pm
  t1_str <- gsub("\\.", "", parts[1])
  t2_str <- gsub("\\.", "", parts[2])
  
  # time parsing using lubridate::parse_date_time
  t1 <- parse_date_time(t1_str, orders = c("I:M p"))
  t2 <- parse_date_time(t2_str, orders = c("I:M p"))
  
  # calculate midpoint
  mid <- t1 + as.numeric(difftime(t2, t1, units = "secs")) / 2
  
  return(mid)
  
  # If you want to return as formatted string instead of POSIXct, uncomment below:
  # mid_str <- format(mid, "%I:%M %p")
  # mid_str <- gsub(" AM", " a.m.", mid_str)
  # mid_str <- gsub(" PM", " p.m.", mid_str)
  # return(mid_str)
}

# 1. helper function
# The functions to create the lookup tables remain the same.

helper <- function(year = 2022,
                   numeric_vars = c("AGEP", "PWGTP"),
                   categorical_vars = c("SEX"),
                   geography = "All",
                   geo_level = "*",
                   arguments = NULL,
                   census_lookups = NULL) {
  
  # Input Validation
  ## year validation
  if (!is.numeric(year) || length(year) != 1 || year < 2010 || year > 2022) {
    stop("Error: 'year' must be a single number between 2010 and 2022.")
  }
  
  ## variable validation
  valid_numeric_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
  if (!all(numeric_vars %in% valid_numeric_vars)) {
    stop("Error: Invalid numeric variables inputted")
  }
  
  valid_categorical_vars <- c("FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "SEX")
  if (!all(categorical_vars %in% valid_categorical_vars)) {
    stop("Error: Invalid categorical variables inputted")
  }
  
  ## Geography validation : region, division, state should be lowercase
  valid_geography <- c("All", "region", "division", "state")
  if (!all(geography %in% valid_geography)) {
    stop("Error: Invalid geography requested.")
  }
  
  # API Request Construction
  base <- paste("https://api.census.gov/data/", year, "/acs/acs1/pums", sep = "")
  all_vars <- unique(c("PWGTP", numeric_vars, categorical_vars))
  
  ## If geography is "All", do not include 'for' parameter
  if (geography == "All") {
    query_params <- c(list('get' = paste(all_vars, collapse = ",")), arguments)
  }
  ## If geography is specified, include geography and geo level in 'for' parameter
  else {
    query_params <- c(list('get' = paste(all_vars, collapse = ",")),
                      list('for' = paste0(geography, ":", geo_level)),
                      arguments)
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
  
  # Convert numeric variables as numeric
  all_numeric_vars <- unique(c("PWGTP", numeric_vars))
  # --- NEW : Exclude time variables from numeric conversion ---
  all_numeric_vars_except_time <- setdiff(all_numeric_vars, c("JWAP", "JWDP"))
  for (num_var in all_numeric_vars_except_time) {
    if (num_var %in% colnames(result_tibble)) {
      result_tibble[[num_var]] <- as.numeric(result_tibble[[num_var]])
    }
  }
  
  # Converting categorical variables to be factors
  for (cat_var in categorical_vars) {
    if (cat_var %in% colnames(result_tibble)) {
      result_tibble[[cat_var]] <- factor(result_tibble[[cat_var]])
    }
  }
  
  # --- NEW: Process Time Variables ---
  if ("JWAP" %in% all_vars) {
    result_tibble <- result_tibble %>%
      mutate(
        # Apply the conversion to each element safely
        JWAP = sapply(as.character(JWAP), function(code) {
          # 1. Look up the label for the current code
          label <- census_lookups$JWAP[[code]]
          # 2. If the lookup fails (returns NULL), return NA
          if (is.null(label)) {
            return(NA_POSIXct_)
          }
          # 3. Otherwise, calculate the midpoint. The function handles non-time strings.
          time_range_to_midpoint(label)
        }, USE.NAMES = FALSE) %>% lubridate::as_datetime()
      )
  }
  
  if ("JWDP" %in% all_vars) {
    result_tibble <- result_tibble %>%
      mutate(
        # Apply the same safe conversion to JWDP
        JWDP = sapply(as.character(JWDP), function(code) {
          label <- census_lookups$JWDP[[code]]
          if (is.null(label)) {
            return(NA_POSIXct_)
          }
          time_range_to_midpoint(label)
        }, USE.NAMES = FALSE) %>% lubridate::as_datetime()
      )
  }
  
  return(result_tibble)
}

# --- NEW : Example Usage for JWAP, JWDP ---

# First, ensure the lookup tables have been created for a relevant year (e.g., 2022)

census_lookups <- create_lookup_tables(year = 2022, var_names = c("JWAP", "JWDP"))

# Now, call the helper function requesting JWAP and JWDP
# We'll get data for North Carolina (state code: 37)
nc_work_times <- helper(
  year = 2022,
  numeric_vars = c("AGEP", "JWAP", "JWDP"),
  categorical_vars = c("SEX"),
  geography = "state",
  geo_level = "37",
  arguments = list('JWAP' = "258"),  # Example: Filter for Bachelor's degree holders
  census_lookups = census_lookups
)

print(nc_work_times)

# 2. multiple helper function

multiple_years_helper<- function(years, ...) {
  
  # Use lapply to call the single-year function for each year in the vector
  multiple_years_data <- lapply(years, function(y) {
    message("Fetching data for year: ", y)
    
    # Call the single-year function, passing along extra arguments
    result_tibble <- helper(year = y, ...)
    
    # Add a year column
    result_tibble$year <- y
    
    # print(result_tibble)
    return(result_tibble)
  })
  
  # Combine all the tibbles into a single one
  combined_result_tibble <- bind_rows(multiple_years_data)
  
  return(combined_result_tibble)
}

# Example 1: Single Year Query

census_lookups <- create_lookup_tables(year = 2021, var_names = c("JWAP", "JWDP"))

# a. Get data for region:3, year:2021, SCHL:24 including AGEP, GASP, SEX
data_2021 <- helper(
  year = 2021,
  numeric_vars = c("AGEP", "GASP", "JWDP", "JWAP" ),
  categorical_vars = c("SEX"),
  geography = c("region"),
  geo_level = c("3"),
  arguments = list('SCHL' = "24"),
  census_lookups = census_lookups
)

print(data_2021)


# b. Get data for state:6, year:2022, JWTRNS:10 including GRPIP, JWAP, JWDP, JWMNP, FER, HHL, HISPEED, SCH
data_2022 <- helper(
  year = 2022,
  numeric_vars = c("GRPIP", "JWAP", "JWDP", "JWMNP"),
  categorical_vars = c("FER", "HHL", "HISPEED", "SCH"),
  geography = c("state"),
  geo_level = c("6"),
  arguments = list('JWTRNS' = "10")
)

print(data_2022)

# Example 2: Multiple Year Query
message("Example 2: Multiple Year Query")

# a. Get data for region:3, years:2021-2022, SCHL:24 including
data_2021_2022 <- multiple_years_helper(
  years = c(2021, 2022),
  numeric_vars = c("AGEP", "GASP"),
  categorical_vars = c("SEX"),
  geography = c("region"),
  geo_level = c("3"),
  arguments = list('SCHL' = "24")
)

print(data_2021_2022)

# b. Get data for state:6, years:2019, 2021, 2022, JWTRNS:10 including GRPIP, JWAP, JWDP, JWMNP, FER, HHL, HISPEED, SCH
data_2019_2022 <- multiple_years_helper(
  # Caution : There is no data in 2020
  years = c(2019, 2021, 2022),
  numeric_vars = c("GRPIP", "JWAP", "JWDP", "JWMNP"),
  categorical_vars = c("FER", "HHL", "HISPEED", "SCH"),
  geography = c("state"),
  geo_level = c("6"),
  arguments = list('JWTRNS' = "10")
)

print(data_2019_2022)


##### Summary Method #####

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
  
  #Convert numeric variables as numeric
  for (num_var in numeric_vars) {
    if (num_var %in% colnames(result_tibble)) {
      result_tibble[[num_var]] <- as.numeric(result_tibble[[num_var]])}}
  
  # Converting categorical variables to be factors
  for (cat_var in categorical_vars) {
    if (cat_var %in% colnames(result_tibble)) {
      result_tibble[[cat_var]] <- factor(result_tibble[[cat_var]])
    }
  }
  
  # The following line is our new modification, it adds a new class (census) to our result_tibble
  class(result_tibble) <- c("census", class(result_tibble))
  
  return(result_tibble)
  
}


#Defining default Numeric Variables for the method
numeric_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")

#Defining Default Categorical Variables for the method
categorical_vars <- c("FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "SEX")

summary.census <- function(cen_tbl,num_vars=numeric_vars, cat_vars=categorical_vars ){
  
  # initialize list to return information
  ret_list<-list() 
  
  # First, lets get counts of categorical variables
  # We append the count tables to the list
  for (cat_var in cat_vars){
    
    #create summary table for column
    sum_tab<-table(cen_tbl[[cat_var]])
    
    # add table to return list
    item_name<-paste(cat_var, "count")
    ret_list[[item_name]] <- sum_tab
  }
  
  
  # Next, we get the means and standard deviations for our numeric variables
  for (num_var in num_vars){
    
    # Get column
    num_var_col<-cen_tbl[[num_var]]
    
    # get mean and SD of col, ignore NA values
    num_var_col_mean<-(mean(as.numeric(num_var_col, na.rm = TRUE)))
    num_var_col_sd<-(sd(as.numeric(num_var_col, na.rm = TRUE)))
    
    # Add items to return list
    
    mean_item_name<-paste(num_var, "mean")
    sd_item_name<-paste(num_var,"SD")
    ret_list[[mean_item_name]] <- num_var_col_mean
    ret_list[[sd_item_name]] <- num_var_col_sd
  }
  
  print(ret_list)
}

## TESTING
data_2021 <- helper(
  year = 2021,
  numeric_vars = c("AGEP", "GASP"),
  categorical_vars = c("SEX"),
  geography = c("region"),
  geo_level = c("3"),
  arguments = list('SCHL' = "23,24")
) 

summary(data_2021)

summary(data_2021,c("GASP", "AGEP"), c("SEX","SCHL"))

print("Table by SEX Variable")
table(data_2021[["SEX"]])

print("Table by SCHL Variable")
table(data_2021[["SCHL"]])

print("GASP mean")
mean(as.numeric(data_2021[["GASP"]]),na.rm=TRUE)

print("GASP SD")
sd(as.numeric(data_2021[["GASP"]]),na.rm=TRUE)

print("AGEP mean")
mean(as.numeric(data_2021[["AGEP"]]),na.rm=TRUE)

print("AGEP SD")
sd(as.numeric(data_2021[["AGEP"]]),na.rm=TRUE)

# First attempt at plot.census function
plot.census<-function(cen_tbl,cat_var,num_var){
  
  ## variable validation
  valid_numeric_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP")
  if (!(num_var %in% valid_numeric_vars)) {
    stop("Error: Invalid numeric variables inputted")
  }
  
  valid_categorical_vars <- c("FER", "HHL", "HISPEED", "JWTRNS", "SCH", "SCHL", "SEX")
  if (!(cat_var %in% valid_categorical_vars)) {
    stop("Error: Invalid categorical variables inputted")
  }
  # The all_of command helps handle string column names
  plt_tbl <- cen_tbl %>%
    select(all_of(c(cat_var, num_var, "PWGTP"))) %>%
    drop_na(all_of(c(cat_var, num_var)))
  
  
  ggplot(plt_tbl, aes(x = get(cat_var), y = as.numeric(get(num_var)), weight = as.numeric(PWGTP))) +
    geom_boxplot()
}

# making sure the stop clause works

# plot.census(data_2021, cat_var = "SCL", num_var = "GASP")
# 
# plot.census(data_2021, cat_var = "SCHL", num_var = "GSP")

# stop clause does work. Commented these lines out so document could render error free.

plot.census(data_2021, cat_var = "SCHL", num_var = "GASP")

plot.census(data_2021, cat_var = "SCHL", num_var = "AGEP")

plot.census(data_2021, cat_var = "SEX", num_var = "GASP")

data_2010_NC <- helper(
  year = 2010,
  numeric_vars = c("GRPIP"),
  categorical_vars = c("SEX"),
  geography = c("state"),
  geo_level = c("37"),
) 

data_2010_NY <- helper(
  year = 2010,
  numeric_vars = c("GRPIP"),
  categorical_vars = c("SEX"),
  geography = "state",
  geo_level = "36"
)

data_2022_NC <- helper(
  year = 2022,
  numeric_vars = c("GRPIP"),
  categorical_vars = c("SEX"),
  geography = c("state"),
  geo_level = c("37"),
) 

data_2022_NY <- helper(
  year = 2022,
  numeric_vars = c("GRPIP"),
  categorical_vars = c("SEX"),
  geography = "state",
  geo_level = "36"
)

summary(data_2010_NC,c("GRPIP"), c("SEX"))

summary(data_2022_NC,c("GRPIP"), c("SEX"))

summary(data_2010_NY,c("GRPIP"), c("SEX"))

summary(data_2022_NY,c("GRPIP"), c("SEX"))

plot.census(data_2010_NC, cat_var = "SEX", num_var = "GRPIP")

plot.census(data_2022_NC, cat_var = "SEX", num_var = "GRPIP")

plot.census(data_2010_NY, cat_var = "SEX", num_var = "GRPIP")

plot.census(data_2022_NY, cat_var = "SEX", num_var = "GRPIP")

###### -----PERSON LEVEL RECORDS------