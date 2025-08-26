# This code file will walk you through doing some specific analysis on the OA output of authors from your institution
# Specifically on journal articles that were identified by OpenAlex as being grant funded and where an author from your institution served as corresponding author.
# You will need to have first worked your way through the code file labeled RCode_OpenAlexR.

library(tidyverse)
library(janitor)
library(here)

# Read in your first file to analyze. You made this in the previous code file labeled RCode_OpenAlexR.
Inst_Articles_InstCorresponding_Grants <- read_csv(here("DataOutput/Inst_Articles_InstCorresponding.csv"))

# This creates a subset of our article data that includes only those that have been identified as grant funded
Inst_Articles_InstCorresponding_Grants <- subset(Inst_Articles_InstCorresponding_Grants, has_grant == "Y")

# This creates a table showing article counts by publisher and OA status for grant-funded articles 
# where someone at your institution served as corresponding authors
PublishersOAGrants <- tabyl(Inst_Articles_InstCorresponding_Grants, host_organization_name, oa_status)

# If you're working with a small dataset, there's a chance at least one of the OA types will not have any matching values
# In that case, the above code would not create a column for it, which will affect the rest of the code
# To fix this, first run a list of all the column names in the table and see if any are missing
# There should be columns for host_organization_name, closed, gold, hybrid, green, diamond, and bronze
# If all seven columns are present, skip the next chunk of code.If one or more are missing, move to the next chunk of code
names(PublishersOAGrants)

# If one of the OA types is missing, use the below code to add a new column that will fill in 0 for all the values in that column
# Replace oatype in the second line with whichever OA type is missing: closed, diamond, gold, green, bronze, or hybrid
# If more than one OA type is missing, just run the code again with the next OA type in place of where oatype is 
PublishersOAGrants <- PublishersOAGrants %>% 
  add_column(oatype = 0)

# Once the table has all seven columns, we'll rename the columns 
# so they're in Title Case and will display better in our visual
PublishersOAGrants <- PublishersOAGrants %>%
  rename(Publisher = host_organization_name,
         Closed = closed,
         Diamond = diamond, 
         Gold = gold, 
         Green = green, 
         Bronze = bronze, 
         Hybrid = hybrid)

# Add a new column, Total, that shows the total article counts for all articles for each publisher
# and then sort the table in descending order by the Total column
# If you want to sort by a different column, just change Total in the third line to the name of the preferred column
PublishersOAGrants <- PublishersOAGrants %>% 
  adorn_totals("col") %>% 
  arrange(desc(Total))

# Now we'll create another dataframe, PublishersGrantsPercent, which shows our table but by percentage instead of count
Percent_PublishersOAGrants <- Inst_Articles_InstCorresponding_Grants %>% 
  tabyl(host_organization_name, oa_status) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting()

# Any column missing from the first table will also be missing in this table as well, so again if needed
# rerun the below code, replacing oatype with whichever OA type you did above
Percent_PublishersOAGrants <- Percent_PublishersOAGrants %>% 
  add_column(oatype = "0.0%")

### Visual ###

# First, it can help to narrow down how many publishers are included in your chart if there are a lot. 
# The code below will select the top 10 rows from our Publisher table (the one showing the total counts)
# If you want more or less than 10, just change the number to what you want
PublishersOAGrants <- head(PublishersOAGrants, 10)

# Now we have to "melt" the Publishers DF to the appropriate format
long_PublishersOAGrants <- PublishersOAGrants %>%
  select(Publisher, Gold, Hybrid, Green, Diamond, Closed, Bronze) %>%
  pivot_longer(cols = c(Gold, Hybrid, Green, Diamond, Closed, Bronze), 
               names_to = "Type", 
               values_to = "Value")


# Run the below code to specify what color you want to use for each OA type in the grid
# The code comes with an accessible color scheme, but you can change them out just putting in a different HTML color code for each one
# Go to https://r-charts.com/color-palettes/ to find example color palettes
# Hover over a color to find its HEX number
# Can also explore https://r-graph-gallery.com/color-palette-finder
custom_colors <- c("Closed" = "#36638E", "Hybrid" = "#8B8E82", "Gold" = "#057BE7", "Diamond" = "#E95CCA", "Green" = "#B2C4DB", "Bronze" = "#A97F86")


# And here we Create the stacked bar chart
# This code involves three chunks that will also save the chart as a PNG in your Visuals folder in this project
# The first chunk sets the parameters for the saved image - you can change the width and height to whatever you prefer - just play around
# The second chunk creates the chart - if you don't want to save the image and just see it in RStudio, run just the second check.
# The third chunk, starting with dev.off, closes the command and saves the actual file.
png(filename = "Visuals/PublishedArticlesGrants.png", 
    width = 6160, height = 3000, res = 600)
ggplot(long_PublishersOAGrants, aes(x = Publisher, y = Value, fill = Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal()
dev.off()