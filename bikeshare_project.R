# Load Libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# ============================================
# DATA LOADING & PREPROCESSING FUNCTION
# ============================================

load_and_clean <- function(city) {
  
  # Construct file name
  filename <- paste0(city, ".csv")
  
  # Read CSV dataset
  df <- read.csv(filename)
  
  # Convert Start.Time string to datetime format
  df$Start.Time <- ymd_hms(df$Start.Time)
  
  # Extract time components: month, day of week, hour
  df$month <- month(df$Start.Time, label = TRUE, abbr = FALSE)
  df$day_of_week <- wday(df$Start.Time, label = TRUE, abbr = FALSE)
  df$hour <- hour(df$Start.Time)
  
  # Create a route feature (Start Station -> End Station)
  df$route <- paste(df$Start.Station, "->", df$End.Station)
  
  # Add city column for future data concatenation/merging
  df$city <- city
  
  return(df)
}

# Test data loading and processing on Chicago dataset
chi <- load_and_clean("chicago")
print("Chicago dataset loaded successfully. First 3 rows:")
head(chi, 3)

# ============================================
# CITY DATA EXPLORATION & SUMMARY FUNCTION
# ============================================

explore_city <- function(df, city_name) {
  
  cat("\n", rep("=", 50), "\n", sep = "")
  cat("CITY DATA SUMMARY:", city_name, "\n")
  cat(rep("=", 50), "\n\n")
  
  # ----- 1. POPULAR TIME METRICS -----
  cat("1. POPULAR RIDE TIMES:\n")
  cat("   Most frequent month:       ", names(sort(table(df$month), decreasing = TRUE)[1]), "\n")
  cat("   Most frequent day of week: ", names(sort(table(df$day_of_week), decreasing = TRUE)[1]), "\n")
  cat("   Most frequent hour:        ", names(sort(table(df$hour), decreasing = TRUE)[1]), ":00\n\n")
  
  # ----- 2. POPULAR STATIONS AND ROUTES -----
  cat("2. POPULAR STATIONS AND ROUTES:\n")
  cat("   Most frequent start station:\n    ", names(sort(table(df$Start.Station), decreasing = TRUE)[1]), "\n\n")
  cat("   Most frequent end station:\n    ", names(sort(table(df$End.Station), decreasing = TRUE)[1]), "\n\n")
  cat("   Most frequent route:\n    ", names(sort(table(df$route), decreasing = TRUE)[1]), "\n\n")
  
  # ----- 3. TRIP DURATION -----
  cat("3. TRIP DURATION METRICS (seconds):\n")
  cat("   Total travel time: ", sum(df$Trip.Duration), "\n")
  cat("   Average trip time:", round(mean(df$Trip.Duration), 2), "\n\n")
  
  # ----- 4. USER DEMOGRAPHICS & SEGMENTATION -----
  cat("4. USER INFORMATION:\n")
  cat("   User Types Breakdown:\n")
  print(table(df$User.Type))
  
  # Check for Gender and Birth.Year columns (missing in Washington dataset)
  if ("Gender" %in% colnames(df) & "Birth.Year" %in% colnames(df)) {
    cat("\n   Gender Breakdown:\n")
    print(table(df$Gender))
    cat("\n   Birth Year Statistics:\n")
    cat("   Earliest birth year: ", min(df$Birth.Year, na.rm = TRUE), "\n")
    cat("   Latest birth year:   ", max(df$Birth.Year, na.rm = TRUE), "\n")
    cat("   Most common year:    ", names(sort(table(df$Birth.Year), decreasing = TRUE)[1]), "\n")
  } else {
    cat("\n   (Gender and Birth Year data are unavailable for this city)\n")
  }
  
  cat("\n")
}

# ============================================
# DATA PIPELINE: LOAD AND ANALYZE ALL CITIES
# ============================================

# Batch load datasets for all targeted cities
chi <- load_and_clean("chicago")
nyc <- load_and_clean("new-york-city")
was <- load_and_clean("washington")

# Execute exploratory analysis and print summaries
explore_city(chi, "CHICAGO")
explore_city(nyc, "NEW YORK CITY")
explore_city(was, "WASHINGTON")

# ============================================
# PLOT 1: Hourly Ride Distribution (New York)
# ============================================

ggplot(nyc, aes(x = hour)) +
  geom_bar(fill = "#2E86AB", color = "white", alpha = 0.8) +
  labs(title = "Hourly Ride Distribution (New York City)",
       subtitle = "Data source: H1 2017 US Bikeshare Records",
       x = "Hour of Day (0 = Midnight, 23 = 11 PM)",
       y = "Number of Rides") +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  theme_minimal(base_size = 12)

# ============================================
# PLOT 2: User Segment Comparison Across Cities
# ============================================

# Concatenate datasets for comparative cross-city analysis
all_cities <- bind_rows(
  chi %>% mutate(City = "Chicago"),
  nyc %>% mutate(City = "New York"),
  was %>% mutate(City = "Washington")
)

ggplot(all_cities, aes(x = City, fill = User.Type)) +
  geom_bar(position = "dodge", color = "white") +
  labs(title = "User Type Distribution Across Cities",
       x = "City",
       y = "Number of Rides",
       fill = "User Type") +
  scale_fill_manual(values = c("Subscriber" = "#F18F01", "Customer" = "#006E90")) +
  theme_minimal(base_size = 12)

# ============================================
# PLOT 3: Trip Duration Distribution Analysis
# ============================================

# Convert trip duration from seconds to minutes for business readability
all_cities$duration_min <- all_cities$Trip.Duration / 60

ggplot(all_cities, aes(x = City, y = duration_min, fill = City)) +
  geom_boxplot(alpha = 0.7, outlier.color = "gray60", outlier.size = 0.5) +
  labs(title = "Trip Duration Distribution Across Cities",
       x = "City",
       y = "Trip Duration (Minutes)") +
  scale_fill_manual(values = c("Chicago" = "#C73E1D", 
                               "New York" = "#2E86AB", 
                               "Washington" = "#6A994E")) +
  coord_cartesian(ylim = c(0, 30)) +  # Clip Y-axis to 30 mins to filter extreme outliers for better visualization
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
