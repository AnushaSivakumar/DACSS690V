rm(list = ls()) # clean memory

location='https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file='eduwa.rda'
link=paste0(location,file)


#getting the data TABLE from the file in the cloud:
load(file=url(link))

dim(eduwa)
names(eduwa)

# this 'width = 70,strict.width='cut' means
# you do not want to see more than 70 characters per row.

str(eduwa,width = 70,strict.width='cut')

library(ggplot2)

# Subset data for Suburb type and drop unused levels
suburbEduwa <- eduwa[eduwa$LocaleType == "Suburb", ]
suburbEduwa$LocaleSub <- droplevels(suburbEduwa$LocaleSub)

# Frequency table as data frame
suburbFreq <- as.data.frame(prop.table(table(suburbEduwa$LocaleSub)))
names(suburbFreq) <- c("LocaleSub", "Percent")

# Horizontal dot plot
dot_plot <- ggplot(suburbFreq, aes(x = Percent, y = reorder(LocaleSub, Percent))) +
  geom_point(size = 5, color = "orange") +
  labs(
    title = "Distribution of Schools Classified by Suburb Size",
    subtitle = "Detailed breakdown for schools in suburban areas",
    caption = "Source: US Department of Education",
    x = "Percent",
    y = "Locale Subtype"
  ) +
  theme_minimal()

#Let's look at the plot
dot_plot

#Save dot plot as rds file
saveRDS(dot_plot, file = "suburb_dot_plot.rds")
