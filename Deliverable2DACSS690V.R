# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the pre-processed dataset
linkCovid <- "https://github.com/DACSS-Visual/tabular_bivar_numnum/raw/refs/heads/main/data/covid-19-cases-deaths-per-million-people.csv"
covidWorld <- read.csv(linkCovid)

# Inspect the dataset
str(covidWorld)
head(covidWorld)

# Download country-continent mapping (assuming you have the CSV file)
link_continent <- "https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/raw/master/all/all.csv"
continent_data <- read.csv(link_continent)

# Keep relevant columns (e.g., Country Name and Continent)
continent_mapping <- continent_data %>%
  select(name, region) %>%
  rename(Entity = name, Continent = region)

# Join with your COVID dataset
covidWorld <- covidWorld %>%
  left_join(continent_mapping, by = "Entity")

# Convert 'date' column to Date type for proper plotting
covidWorld$date <- as.Date(covidWorld$date, format = "%m/%d/%y")

# Check for NA values in deaths
summary(covidWorld$daily_deaths_mean)

# Group data by continent and date, and calculate mean daily cases
covidWorld_continent <- covidWorld %>%
  group_by(Continent, date) %>%
  summarise(mean_cases = mean(daily_cases_mean, na.rm = TRUE)) %>%
  arrange(date)

# Convert 'date' to Date format (if not already done)
covidWorld_continent$date <- as.Date(covidWorld_continent$date)

# Plot the trends for mean daily case counts by continent
line_plot <- ggplot(covidWorld_continent, aes(x = date, y = mean_cases, color = Continent)) +
  geom_line(linewidth = 0.4) +
  theme_minimal() +
  labs(
    title = "COVID-19 Mean Daily Cases by Continent",
    subtitle = "Trends Over Time",
    x = "Date",
    y = "Mean Daily Cases",
    color = "Continent",
    caption = "Source: COVID-19 Data Repository"
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

line_plot

saveRDS(line_plot, file = "covidWorld_continent_line_plot.rds")
