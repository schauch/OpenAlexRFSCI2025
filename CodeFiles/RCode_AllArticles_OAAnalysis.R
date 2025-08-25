# This code file will walk you through doing some specific analysis on the OA output of authors from your institution
# You will need to have first worked your way through the code file labeled RCode_OpenAlexR.

library(tidyverse)
library(janitor)
library(here)

# Read in your file that you made at the end of the code file labeled RCode_OpenAlexR.
Articles_InstCorresponding <- read_csv(here("DataOutput/Inst_Articles_InstCorresponding.csv"))

# First we're going to create a table by publisher showing totals for all the articles where someone at your institution served as corresponding authors
PublishersAll <- Articles_InstCorresponding %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

# Sometimes this creates a column, valid_percent, that we don't need, so this removes it
# If there is no such column, you'll get a warning, but you can just ignore it.
PublishersAll <- PublishersAll %>% 
  select(-c(valid_percent))

# Rename the coun (n) column to All
PublishersAll <- PublishersAll %>%
  rename(All = n)

# Now we'll make a dataframe that's a pivot table showing the breakdown of articles by OA status and the publisher
PublishersOA <- tabyl(Articles_InstCorresponding, host_organization_name, oa_status)

# Rename our columns so they're in Title Case and will display better in our visual
PublishersOA <- PublishersOA %>%
  rename(Diamond = diamond, Closed = closed, Gold = gold, Green = green, Bronze = bronze, Hybrid = hybrid)

# Next bring the two tables together
Publishers <- PublishersAll %>% 
  full_join(PublishersOA)

# Let's set any NAs to 0s
Publishers <- Publishers %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Green = 0, Diamond = 0, Closed = 0, Bronze = 0))

# And then rename the host column to Publisher
Publishers <- Publishers %>%
  rename(Publisher = host_organization_name)

# Now we'll create another DF, PublishersPercent that shows our table but by percentage instead of count
PublishersPercent <- Articles_InstCorresponding %>% 
  tabyl(host_organization_name, oa_status) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 0)

##### Visuals ####

# First, it can help to narrow down how many publishers are included in your chart if there are a lot. 
# The code below will select the top 10 rows from our Publisher table (the one showing the total counts)
# If you want more or less than 10, just change the number to what you want
PublishersVisual <- head(Publishers, 10)

# Now we have to "melt" the PublishersVisual DF to the appropriate format
long_Publishers <- PublishersVisual %>%
  select(Publisher, Gold, Hybrid, Green, Diamond, Closed, Bronze) %>%
  pivot_longer(cols = c(Gold, Hybrid, Green, Diamond, Closed, Bronze), 
               names_to = "Type", 
               values_to = "Value")

# Run the below code to specify what color you want to use for each OA type in the grid
# The code comes with an accessible color scheme, but you can change them out just putting in a different HTML color code for each one
# Go to https://r-charts.com/color-palettes/ to find example color palettes
# Hover over a color to find its HEX number
# Can also exlore https://r-graph-gallery.com/color-palette-finder
custom_colors <- c("Closed" = "#36638E", "Hybrid" = "#8B8E82", "Gold" = "#057BE7", "Diamond" = "#E95CCA", "Green" = "#B2C4DB")


# And here we Create the stacked bar chart
# This code involves three chunks that will also save the chart as a PNG in your Visuals folder in this project
# The first chunk sets the parameters for the saved image - you can change the width and height to whatever you prefer - just play around
# The second chunk creates the chart - if you don't want to save the image and just see it in RStudio, run just the second check.
# The third chunk, starting with dev.off, closes the command and saves the actual file.
png(filename = "Visuals/PublishedArticlesAll.png", 
    width = 6160, height = 3000, res = 600)
ggplot(long_Publishers, aes(x = Publisher, y = Value, fill = Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal()
dev.off()
