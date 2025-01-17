library(ggplot2)
library(dplyr)

#Loading the pre-processed dataset
linkCovid <- "https://github.com/DACSS-Visual/tabular_bivar_numnum/raw/refs/heads/main/data/covid-19-cases-deaths-per-million-people.csv"
covidWorld <- read.csv(linkCovid)

#Inspect data
str(covidWorld)
head(covidWorld)

#Download country-continent mapping 
link_continent <- "https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/raw/master/all/all.csv"
continent_data <- read.csv(link_continent)

#Only keeping relevant columns (Country Name and Continent)
continent_mapping <- continent_data %>%
  select(name, region) %>%
  rename(Entity = name, Continent = region)

#Left join continent column with your COVID dataset
covidWorld <- covidWorld %>%
  left_join(continent_mapping, by = "Entity")

covidWorld$date <- as.Date(covidWorld$date, format = "%m/%d/%y")

#Grouping data by continent and date, and calculate mean daily cases
covidWorld_continent <- covidWorld %>%
  group_by(Continent, date) %>%
  summarise(mean_cases = mean(daily_cases_mean, na.rm = TRUE)) %>%
  arrange(date)

#Convert 'date' to Date format
covidWorld_continent$date <- as.Date(covidWorld_continent$date)

#Plotting trend for mean daily case counts grouped by continent
line_plot <- ggplot(covidWorld_continent, aes(x = date, y = mean_cases, color = Continent)) +
  geom_line(linewidth = 0.4) +
  theme_minimal() +
  labs(
    title = "COVID-19 Mean Daily Cases by Continent",
    subtitle = "Trends Over Time Between Apr 2020 to Oct 2024",
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
