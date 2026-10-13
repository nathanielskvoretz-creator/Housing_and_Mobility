library(tidyverse)
library(purrr)
library(arrow)
library(sf)
library(usmap)
library(ggpmisc)

# Import county shape data for spatial plots
counties <- read_sf("data/County Borders/cb_2018_us_county_20m.shp")

#Stores base file names for custom import function
acs_pop_base_files<- c("ACSDP5Y2014.DP05", "ACSDP5Y2019.DP05", "ACSDP5Y2023.DP05")
acs_house_mortgage_base_files <- c("ACSST5Y2014.S2506","ACSST5Y2019.S2506","ACSST5Y2023.S2506" )
acs_house_no_mortgage_base_files <- c("ACSST5Y2014.S2507","ACSST5Y2019.S2507","ACSST5Y2023.S2507")

# Custom import function for ACS csv data
acs_import <- function(file_location, base_file_names){
  
  data_file <- "-Data.csv"                       # Stores data file tag as string
  name_file <- "-Column-Metadata.csv"            # Stores label names file tag as string
  
  clean_census_label <- function(text) {         # Function to clean up the label names to more easily work with
    text |> 
      str_remove("^.*?!!") |>                    # Remove the "Estimate!!" or similar prefix
      str_remove("^[A-Z\\s,]+!!") |>             # Remove broad categories like "SEX AND AGE!!"
      str_remove("Estimate!!") |> 
      str_replace_all("Owner-occupied housing units without a mortgage!!+", "") |>
      str_replace_all("Owner-occupied housing units with a mortgage!!+", "") |>
      str_remove("\\,") |> 
      str_remove("^Total population!!") |>       # Remove "Total population!!" if present
      str_remove("\\s*\\(.*?\\)$") |>            # Remove parenthetical details at the end (e.g., "(males...)")
      str_replace_all("(!!|\\s)", "_") |>        # Convert any internal '!!' separators or spaces to underscores
      str_to_lower()                             # Convert any upper case letters to lower case
  }
  
  data_list <- list()                            # Empty list used to store data frames from following forloop
  
  # This forloop imports a file containing the variable code and the corresponding label name.
  # It cleans up the label name, drops duplicates, and creates name map to apply to the ACS data columns (which use the variable code)
  
  for (i in base_file_names){                   
    acs_labels <- read_csv(paste0(file_location, i, name_file)) |> 
      filter(`Column Name` %in% c("GEO_ID", "NAME")| str_detect(`Column Name`,"\\d+E$")) |> # Looks for estimate columns only; i.e no margin of error or percent column
      mutate(Label = clean_census_label(Label)) |> 
      distinct(Label, .keep_all = TRUE) # drops duplicates
    
    variable_name_map <- setNames(acs_labels$`Column Name`, acs_labels$Label)
    
    valid_cols <- c("GEO_ID", "NAME", acs_labels$`Column Name`) # This vector feeds into the data import step.
    
    acs <- read_csv(paste0(file_location, i, data_file), 
                         col_select = all_of(valid_cols),  # Ensures duplicate columns aren't selected. Without this the duplicate doesn't have a label mapping and shows up as the raw variable code
                         col_types = cols(.default = "c"), # Ensures no issues when binding data sets
                         id = "file"                       # creates file column with file_path. Helpful in merging errors and identifying the year variable
                         ) |>
      rename(any_of(variable_name_map)) |>                 # Rename mapping step.
      slice(-1)                                            # Removes the first row which contains the label information.
      
    data_list[[as.character(i)]] <- acs                    # Stores imported dataset for each data file.
  }
bind_rows(data_list) |> 
  mutate(across(!(file:geographic_area_name), \(x) na_if(x, y = "N")),  # Converts N values to NA
         across(!(file:geographic_area_name), \(x) as.numeric(x)),
         year = as.character(str_extract(file, "20\\d{2}")),            # Extracts the year from the file name as a character
         geography = str_remove(geography, "0500000US")                 # Cleans up the fips code
  ) |> 
  rename(fips = geography) |>                                           # Rename fips code for easier use
  relocate(year) |>                                                     
  select(-file)                                                         # Relocates year as the first variable and drop file as it is no longer needed since year serves a similar function
}

acs_pop<- acs_import(file_location = "data/ACS5Y/population/", base_file_names = acs_pop_base_files)   # Runs the above function and stores it.
acs_house_mortgage <- acs_import(file_location = "data/ACS5Y/housing/", base_file_names = acs_house_mortgage_base_files)
acs_house_no_mortgage <- acs_import(file_location = "data/ACS5Y/housing/", base_file_names = acs_house_no_mortgage_base_files)

# Merge housing and population data
acs_house <- acs_house_mortgage |> 
  left_join(acs_house_no_mortgage, by = join_by(year, fips, geographic_area_name), suffix = c("_m","_nm"))

# Plot housing and population 
acs_pop |> 
  ggplot(aes(x = total_population, y = total_housing_units, color = year)) +
  geom_point(alpha = .3) +
  scale_y_continuous(
    labels = function(y) paste0(y / 1000, "k"),
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x / 1000, "k")
  ) +
  geom_smooth(method = "lm")

# Plot housing and population log scale
acs_pop |> 
  ggplot(aes(x = log(total_population), y = log(total_housing_units), color = year)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm")

# housing total density distribution
acs_pop |> 
  ggplot(aes(x = log(total_housing_units), color = year)) +
  geom_density(aes(fill  = year), alpha = 0.1)

# Join clean ACS data with spatial geometry for visuals and plot. Not as nice as usmaps version 

# Total population by county. Color mapping is on a base 10 log scale. 
acs_pop |> 
  left_join(x = counties, by = join_by(GEOID == fips)) |> 
  filter(year == 2023) |> 
  ggplot() +
  geom_sf(aes(fill = total_population), color = "transparent") +
  scale_fill_viridis_c(
    trans = "log10",
    labels = scales::comma
  )

# usmaps version of total housing units by county on base 10 log color scale. 
acs_pop |> 
  filter(year == 2023) |> 
  select(fips = fips, values = total_housing_units) |> 
  plot_usmap(regions = "counties", data = _, values = "values") +
  scale_fill_viridis_c(
    trans = "log10",
    labels = scales::comma
  )

# Spatial distribution of population growth between 2014 and 2023
acs_pop |> 
  select(year:total_population) |> 
  pivot_wider(
    id_cols = c(fips, geographic_area_name),
    names_from = year, 
    values_from = total_population
  ) |> 
  summarise(
    g14_23 = ((`2023`-`2014`)/`2014`) * 100, 
    g14_19 = ((`2019`-`2014`)/`2014`) * 100, 
    g19_23 = ((`2023`-`2019`)/`2019`) * 100, 
    .by = c(fips, geographic_area_name)
  ) |> 
  plot_usmap(regions = "counties", data = _, values = "g14_23") +
  scale_fill_viridis_c()

#spatial distribution of growth median housing prices for units without a mortgage between 2014 and 2023
acs_house |> 
  select(year:geographic_area_name, value_median_nm) |>
  pivot_wider(
    id_cols = c(fips, geographic_area_name),
    names_from = year, 
    values_from = value_median_nm
  ) |> 
  summarise(
    g14_23 = ((`2023`-`2014`)/`2014`) * 100, 
    g14_19 = ((`2019`-`2014`)/`2014`) * 100, 
    g19_23 = ((`2023`-`2019`)/`2019`) * 100, 
    .by = c(fips, geographic_area_name)
  ) |> 
  plot_usmap(regions = "counties", data = _, values = "g14_23") +
  scale_fill_viridis_c(
    limits = c(-30, 200), 
    oob = scales::squish
  )

# Relationship between median housing prices of units with and without a mortgage
acs_house |> 
  select(year:geographic_area_name, value_median_nm, value_median_m) |> 
  ggplot(aes(x = value_median_nm, y = value_median_m, color = year)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ year) +
  scale_x_continuous(
    labels = function(x) paste0(x/ 1000, "k")
  ) +
  scale_y_continuous(
    labels = function(y) paste0(y/ 1000, "k")
  ) + 
  geom_smooth(method = "lm") +
  stat_poly_eq(use_label("eq"), formula = y ~ x)


# Relationship between population growth and growth in median housing prices.
acs_house |> 
  left_join(acs_pop, by = join_by(year, fips, geographic_area_name)) |> 
  select(year:geographic_area_name, value_median_nm, total_population) |> 
  pivot_wider(
    id_cols = c(fips, geographic_area_name),
    names_from = year, 
    values_from = c(value_median_nm, total_population)
  ) |> 
  summarise(
    pg14_23 = ((total_population_2023 - total_population_2014) / total_population_2014) * 100, 
    pg14_19 = ((total_population_2019 - total_population_2014) / total_population_2014) * 100, 
    pg19_23 = ((total_population_2023 - total_population_2019) / total_population_2019) * 100, 
    hg14_23 = ((value_median_nm_2023 - value_median_nm_2014) / value_median_nm_2014) * 100, 
    hg14_19 = ((value_median_nm_2019 - value_median_nm_2014) / value_median_nm_2014) * 100,
    hg19_23 = ((value_median_nm_2023 - value_median_nm_2019) / value_median_nm_2019) * 100,
    .by = c(fips, geographic_area_name)
  ) |> 
  ggplot(aes(x = pg14_23, y = hg14_23)) +
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm") +
  stat_poly_eq(use_label("eq"), formula = y ~ x)
  
