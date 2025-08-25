#This code file will walk you through doing some specific analysis on the OA output of authors from your institution
#Specifically on journal articles that were identified by OpenAlex as being grant funded and where an author from your institution served as corresponding author.
#You will need to have first worked your way through the code file labeled RCode_OpenAlexR.

library(tidyverse)
library(janitor)
library(here)

#Read in your first file to analyze. You made this in the previous code file labeled RCode_OpenAlexR.
Inst_Articles_InstCorresponding_Grants <- read_csv(here("DataOutput/Inst_Articles_InstCorresponding.csv"))

Inst_Articles_InstCorresponding_Grants <- subset(Inst_Articles_InstCorresponding_Grants, has_grant == "Y")

#First we're going to create a table by publisher showing totals for grant-funded articles where someone at your institution served as corresponding authors
##And then we'll turn that table into a dataframe (DF) that will be used to make a bar chart.
PublishersAllGrants <- Inst_Articles_InstCorresponding_Grants %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

# Sometimes this creates a column, valid_percent, that we don't need, so this removes it
# If there is no such column, you'll get a warning, but you can just ignore it.
PublishersAllGrants <- PublishersAllGrants %>% 
  select(-c(valid_percent))

# Rename the coun (n) column to All
PublishersAllGrants <- PublishersAllGrants %>%
  rename(All = n)

# Now we'll make a dataframe that's a pivot table showing the breakdown of articles by OA status and the publisher
PublishersOAGrants <- tabyl(Inst_Articles_InstCorresponding_Grants, host_organization_name, oa_status)

# Rename our columns so they're in Title Case and will display better in our visual
PublishersOAGrants <- PublishersOAGrants %>%
  rename(Diamond = diamond, Closed = closed, Gold = gold, Green = green, Bronze = bronze, Hybrid = hybrid)

# Next bring the two tables together
Publishers <- PublishersAllGrants %>% 
  full_join(PublishersOAGrants)

#You should see a larger table if you open Publishers that includes columns for Var1, All, Gold, Hybrid, Green, Diamond and Closed

#Let's set any NAs to 0s
PublishersGrants <- PublishersGrants %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Green = 0, Diamond = 0, Closed = 0, Bronze = 0))


#And then rename the Var1 column to Publisher
PublishersGrants <- PublishersGrants %>%
  rename(Publisher = host_organization_name)

#Reorder by the All column and is descending. If you wish to reorder by a different column, just replace All below with the name you want.
PublishersGrants[order(-PublishersGrants$All), ]

#Save our work
write.csv(PublishersGrants, "DataOUtput/PublishersGrants.csv")

#Now we'll create another DF, PublishersPercent that shows our table but by percentage instead of count
PublishersGrants <- PublishersGrants %>% 
  mutate(GoldPercent = (Gold / All) * 100,
         GoldPercent = paste0(round(GoldPercent, 2), "%"),
         HybridPercent = (Hybrid / All) * 100,
         HybridPercent = paste0(round(HybridPercent, 2), "%"),
         GreenPercent = (Green / All) * 100,
         GreenPercent = paste0(round(GreenPercent, 2), "%"),
         DiamondPercent = (Diamond / All) * 100,
         DiamondPercent = paste0(round(DiamondPercent, 2), "%"),
         ClosedPercent = (Closed / All) * 100,
         ClosedPercent = paste0(round(ClosedPercent, 2), "%"),
         BronzePercent = (Bronze / All) * 100,
         BronzePercent = paste0(round(BronzePercent, 2), "%")
     )

#It kept our original columns showing counts, so we'll remove those now
PublishersGrantsPercent <- PublishersGrants %>% select(-one_of('Hybrid', 'Gold', 'Green', 'Diamond', 'Closed', 'All', 'Bronze'))


#And rename the columns to just the OA type
PublishersGrantsPercent <- PublishersGrantsPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Green = GreenPercent, Diamond = DiamondPercent, Closed = ClosedPercent, Bronze = BronzePercent)



#Save our work
write.csv(PublishersGrantsPercent, "DataOutput/PublishersGrantsPercent.csv")

###Making the visual

#First, it can help to narrow down how many publishers are included in your chart if there are a lot. 
#The code below will select the top 10 rows from our Publisher table (the one showing the total counts)
#If you want more or less than 10, just change the number to what you want
PublishersGrants <- head(PublishersGrants, 10)

#Now we have to "melt" the Publishers DF to the appropriate format
long_PublishersGrants <- PublishersGrants %>%
  select(Publisher, Gold, Hybrid, Green, Diamond, Closed, Bronze) %>%
  pivot_longer(cols = c(Gold, Hybrid, Green, Diamond, Closed, Bronze), 
               names_to = "Type", 
               values_to = "Value")


#Run the below code to specify what color you want to use for each OA type in the grid
#The code comes with an accessible color scheme, but you can change them out just putting in a different HTML color code for each one
#Go to https://r-charts.com/color-palettes/ to find example color palettes
#Hover over a color to find its HEX number
#Cn also exlore https://r-graph-gallery.com/color-palette-finder
custom_colors <- c("Closed" = "#36638E", "Hybrid" = "#8B8E82", "Gold" = "#057BE7", "Diamond" = "#E95CCA", "Green" = "#B2C4DB", "Bronze" = "#A97F86")


# And here we Create the stacked bar chart
# This code involves three chunks that will also save the chart as a PNG in your Visuals folder in this project
# The first chunk sets the parameters for the saved image - you can change the width and height to whatever you prefer - just play around
#The second chunk creates the chart - if you don't want to save the image and just see it in RStudio, run just the second check.
#The third chunk, starting with dev.off, closes the command and saves the actual file.
png(filename = "Visuals/PublishedArticlesGrants.png", 
    width = 6160, height = 3000, res = 600)
ggplot(long_PublishersGrants, aes(x = Publisher, y = Value, fill = Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal()
dev.off()