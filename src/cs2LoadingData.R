library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(httr)
library(rvest)

# show_col_types = FALSE

data343_location <- "data/rawdata_343.txt"
data373_location <- "data/rawdata_373.csv"
data347_location <- "data/rawdata_347.txt"

# Load data
# Define the widths of the fields
pos343 <- fwf_positions(start = c(9, 75), end = c(74, 79))
pos347 <- fwf_positions(start = c(9, 72), end = c(71, 79))

# Read files into dataframes
rawdata_343 <- read_fwf(data343_location, col_positions = pos343)
rawdata_373 <- read_csv(data373_location)
rawdata_347 <- read_fwf(data347_location, col_positions = pos347)
# Drop first 2 rows
rawdata_343 <- rawdata_343[-c(1, 2), ]
rawdata_347 <- rawdata_347[-c(1, 2), ]

# Set the column names
colnames(rawdata_343) <- c("country_name", "median_age")
colnames(rawdata_347) <- c("country_name", "net_migration_rate")

# Merge the data sets containing raw data using dplyr function on the unique keys. Keep the union of all
# observations in the tables. What key are you using for merging? Return the dimension of the merged data
# set.

# Merge the data sets
rawdata <- merge(rawdata_343, rawdata_347, by = "country_name", all = TRUE)
rawdata <- merge(rawdata, rawdata_373, by = "country_name", all = TRUE)

# Return the dimension of the merged data set
print(dim(rawdata))


# You will acquire more country level information such as the classification of the country based on income.
# Such an information can be found at https://datahelpdesk.worldbank.org/knowledgebase/articles/906519.
# From there extract the classification for 2020 into low/lower-middle/upper-middle/high income countries.

# Read the xlsx file
income_classification <- read_excel("data/OGHIST.xlsx", sheet = "Country Analytical History")
# Extract relevant information. That is:
# All elements from the 11th row and the column which at the 5th row has value 2020
# And the respective country name, which is on the 2nd column
country_classification <- income_classification[11:228, c(1, 2, which(income_classification[5, ] == 2020))]
colnames(country_classification) <- c("country_code", "country_name", "income_status")


# Merge the data sets
rawdata_wrong <- merge(rawdata, country_classification, by = "country_name", all = TRUE)

# Load country codes
raw_ccodes <- read_csv("data/Country Data Codes.csv")

# Join raw_ccodes by Name with rawdata by country_name
# From raw_ccode, get only GENC column
rawdata <- left_join(rawdata, raw_ccodes[, c("Name", "GENC")], by = c("country_name" = "Name"))

# Now join can be done by country_code
rawdata <- left_join(rawdata, country_classification[, c("country_code", "income_status")], by = c("GENC" = "country_code"))



continent_table_web <- "https://statisticstimes.com/geography/countries-by-continents.php"

# Read the html
response <- httr::GET(continent_table_web) 
continent_table <- content(response, as="text") %>%
  rvest::read_html() %>%
  rvest::html_nodes("#table_id") %>%
  rvest::html_table(na.strings = "")
# Extract to a table
continent_table <- continent_table[[1]]

# Add information of continent, region and sub-region to the rawdata
# Join by ISO-alpha3 Code and GENC
df_vars <- left_join(rawdata, continent_table[, c("ISO-alpha3 Code", "Continent", "Region 1", "Region 2")], by = c("GENC" = "ISO-alpha3 Code"))