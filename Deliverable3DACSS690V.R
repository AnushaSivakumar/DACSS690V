library(sf)
library(ggplot2)
library(dplyr)
library(rio)
library(tidyr)

linkBoston <- "https://github.com/DACSS-Visual/SpatialData/raw/refs/heads/main/data/BostonContrib.xlsx"
bostonCont <- rio::import(linkBoston)
linkZips <- "https://raw.githubusercontent.com/DACSS-Visual/SpatialData/refs/heads/main/data/zip_codes.json"
bostonZips <- sf::read_sf(linkZips)

#Filtering for 2 tender types Credit Card and Check
selected_tenders <- c("Credit Card", "Check")
bostonCont_filtered <- bostonCont %>%
  filter(`Tender Type Description` %in% selected_tenders)

#Aggregate by tender type and zipcode
agg_data <- bostonCont_filtered %>%
  group_by(Zip, `Tender Type Description`) %>%
  summarise(TotalAmount = sum(Amount, na.rm = TRUE)) %>%
  group_by(Zip) %>%
  mutate(NormalizedAmount = TotalAmount / sum(TotalAmount)) %>%  # Normalize
  ungroup()

#Merge with map
agg_data_wide <- agg_data %>%
  select(Zip, `Tender Type Description`, NormalizedAmount) %>%
  pivot_wider(names_from = `Tender Type Description`, values_from = NormalizedAmount, values_fill = 0)

bostonZips_merged <- bostonZips %>%
  left_join(agg_data_wide, by = c("ZIP5" = "Zip"))

head(bostonZips_merged)

bostonZips_long <- bostonZips_merged %>%
  pivot_longer(cols = c(`Credit Card`, `Check`), 
               names_to = "TenderType", 
               values_to = "NormalizedAmount")

#Faceted choropleths
choropleth_plot <- ggplot(bostonZips_long) +
  geom_sf(aes(fill = NormalizedAmount), color = "white") +
  scale_fill_gradient(low = "#e5f5e0", high = "#006d2c", na.value = "grey90") + # Shades of green
  labs(
    title = "Normalized Contributions by Tender Type and Zip Code",
    fill = "Proportion of Total Contributions",
    x = "Longitude",
    y = "Latitude",
    caption = "Source: Boston Contributions Data"
  ) +
  facet_wrap(~TenderType) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

choropleth_plot

saveRDS(choropleth_plot, file = "choropleth_plot.rds")
