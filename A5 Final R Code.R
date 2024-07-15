# Set the working directory and verify it
setwd('D:\\SCMA632__FIRE632\\Stats\\Assignment\\A5 Arvind')
getwd()

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA", "sf")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for UP
df <- data %>%
  filter(state_1 == "HP")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
hpnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
hpnew$Meals_At_Home <- impute_with_mean(hpnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  hpnew <- remove_outliers(hpnew, col)
}

# Summarize consumption
hpnew$total_consumption <- rowSums(hpnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- hpnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
# District mapping for Himachal Pradesh
district_mapping_hp <- c(
  "1" = "Chamba",
  "2" = "Kangra",
  "3" = "Lahul & Spiti",
  "4" = "Kullu",
  "5" = "Mandi",
  "6" = "Hamirpur",
  "7" = "Una",
  "8" = "Bilaspur",
  "9" = "Solan",
  "10" = "Sirmaur",
  "11" = "Shimla",
  "12" = "Kinnaur"
)
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

hpnew$District <- as.character(hpnew$District)
hpnew$Sector <- as.character(hpnew$Sector)
hpnew$District <- ifelse(hpnew$District %in% names(district_mapping_hp), district_mapping_hp[hpnew$District], hpnew$District)
hpnew$Sector <- ifelse(hpnew$Sector %in% names(sector_mapping), sector_mapping[hpnew$Sector], hpnew$Sector)

View(hpnew)

hist(hpnew$total_consumption, breaks = 10, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in HP State")

HP_consumption <- aggregate(total_consumption ~ District, data = hpnew, sum) 
View(HP_consumption)

barplot(HP_consumption$total_consumption, 
        names.arg = HP_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed

# b) Plot {'any variable of your choice'} on the Karnataka state map using NSSO68.csv data

data_map <- st_read("D:\\SCMA632__FIRE632\\Stats\\Assignment\\A5 Arvind\\HIMACHAL PRADESH_DISTRICTS.geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 
data_map_data <- merge(HP_consumption,data_map,by = "District") 
View(data_map_data)
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "green", high = "blue") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "green", high = "blue") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")