## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 7,
  warning = FALSE,
  message = FALSE
)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("bivariateLeaflet")

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("maduprey/bivariateLeaflet")

## ----eval=FALSE---------------------------------------------------------------
# library(tidycensus)
# library(tidyr)
# library(dplyr)
# library(sf)
# 
# # Get census API key if you haven't already
# # census_api_key("YOUR_KEY_HERE")
# 
# # Get ACS data for DC census tracts
# tract_data <- get_acs(
#   geography = "tract",
#   variables = c(
#     "B01003_001", # Total population
#     "B19013_001"  # Median household income
#   ),
#   state = "DC",
#   year = 2020,
#   geometry = TRUE
# )
# 
# # Pivot data to wide format
# tract_data_wide <- tract_data %>%
#   select(-moe) %>%
#   pivot_wider(
#     names_from = variable,
#     values_from = estimate
#   )

## ----eval=FALSE---------------------------------------------------------------
# # Get county-level data for entire US
# county_data <- get_acs(
#   geography = "county",
#   variables = c(
#     "B01003_001", # Total population
#     "B19013_001"  # Median household income
#   ),
#   year = 2020,
#   geometry = TRUE
# )
# 
# county_data_wide <- county_data %>%
#   select(-moe) %>%
#   pivot_wider(
#     names_from = variable,
#     values_from = estimate
#   )

## ----eval=FALSE---------------------------------------------------------------
# # Get block group data for DC
# blockgroup_data <- get_acs(
#   geography = "block group",
#   variables = c(
#     "B01003_001", # Total population
#     "B19013_001"  # Median household income
#   ),
#   state = "DC",
#   year = 2020,
#   geometry = TRUE
# )
# 
# blockgroup_data_wide <- blockgroup_data %>%
#   select(-moe) %>%
#   pivot_wider(
#     names_from = variable,
#     values_from = estimate
#   )

## -----------------------------------------------------------------------------
library(bivariateLeaflet)

## ----eval=FALSE---------------------------------------------------------------
# # Create basic map
# tract_map <- create_bivariate_map(
#   data = tract_data_wide,
#   var_1 = "B01003_001",    # Total population
#   var_2 = "B19013_001"     # Median household income
# )
# 
# # Display the map
# tract_map

## -----------------------------------------------------------------------------
# Display the default color matrix
create_default_color_matrix()

## ----eval=FALSE---------------------------------------------------------------
# sequential_colors <- matrix(c(
#   "#49006a", "#2d004b", "#1a0027",
#   "#8c96c6", "#8856a7", "#810f7c",
#   "#edf8fb", "#bfd3e6", "#9ebcda"
# ), nrow = 3, byrow = TRUE)
# 
# sequential_map <- create_bivariate_map(
#   data = tract_data_wide,
#   var_1 = "B01003_001",
#   var_2 = "B19013_001",
#   color_matrix = sequential_colors
# )
# 
# sequential_map

## ----eval=FALSE---------------------------------------------------------------
# # Identify tracts with missing data
# missing_data <- tract_data_wide %>%
#   mutate(
#     missing_pop = is.na(B01003_001),
#     missing_income = is.na(B19013_001)
#   )
# 
# # Create a map excluding missing data
# clean_map <- create_bivariate_map(
#   data = missing_data %>% filter(!missing_pop & !missing_income),
#   var_1 = "B01003_001",
#   var_2 = "B19013_001"
# )
# 
# clean_map

## ----eval=FALSE---------------------------------------------------------------
# # Create a map of US counties
# county_map <- create_bivariate_map(
#   data = county_data_wide,
#   var_1 = "B01003_001",
#   var_2 = "B19013_001"
# )
# 
# county_map

## ----eval=FALSE---------------------------------------------------------------
# # Create a detailed block group map
# blockgroup_map <- create_bivariate_map(
#   data = blockgroup_data_wide,
#   var_1 = "B01003_001",
#   var_2 = "B19013_001"
# )
# 
# blockgroup_map

## ----eval=FALSE---------------------------------------------------------------
# # Create custom labels with tract names and formatted values
# custom_labels <- sprintf(
#   "<strong>Census Tract:</strong> %s<br/>
#    <strong>Population:</strong> %s<br/>
#    <strong>Median Income:</strong> $%s",
#   tract_data_wide$NAME,
#   format(tract_data_wide$B01003_001, big.mark = ","),
#   format(tract_data_wide$B19013_001, big.mark = ",")
# )
# 
# # Create map with custom labels
# map_custom_labels <- create_bivariate_map(
#   data = tract_data_wide,
#   var_1 = "B01003_001",
#   var_2 = "B19013_001",
#   custom_labels = custom_labels
# )
# 
# map_custom_labels

## ----error=TRUE---------------------------------------------------------------
try({
# Missing variable
try(calculate_tertiles(data.frame(x = 1:5), "nonexistent", "also_nonexistent"))

# Too few unique values
test_data <- data.frame(
  var1 = c(1,1,1,2),
  var2 = c(1,2,3,4)
)
calculate_tertiles(test_data, "var1", "var2")
})

## ----eval=FALSE---------------------------------------------------------------
# # Identify outliers using IQR method
# outlier_data <- tract_data_wide %>%
#   mutate(
#     pop_outlier = B01003_001 > quantile(B01003_001, 0.75, na.rm = TRUE) +
#       1.5 * IQR(B01003_001, na.rm = TRUE),
#     income_outlier = B19013_001 > quantile(B19013_001, 0.75, na.rm = TRUE) +
#       1.5 * IQR(B19013_001, na.rm = TRUE)
#   )
# 
# # Create map excluding outliers
# no_outliers_map <- create_bivariate_map(
#   data = outlier_data %>%
#     filter(!pop_outlier & !income_outlier),
#   var_1 = "B01003_001",
#   var_2 = "B19013_001"
# )

## ----eval=FALSE---------------------------------------------------------------
# # Reproject data if needed
# reprojected_data <- tract_data_wide %>%
#   st_transform(4326)  # WGS 84
# 
# # Create map with reprojected data
# projected_map <- create_bivariate_map(
#   data = reprojected_data,
#   var_1 = "B01003_001",
#   var_2 = "B19013_001"
# )

