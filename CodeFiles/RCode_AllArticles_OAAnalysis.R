#This code file will walk you through doing some specific analysis on the OA output of authors from your institution
#You will need to have first worked your way through the code file labeled RCode_OpenAlexR.

library(tidyverse)
library(janitor)

### NOTICE! This file uses the following code multiple times:
# PublishersAllGrants <- PublishersAllGrants %>% 
# select(-c(valid_percent))
# This removes a column that's not always there; so if you get an error, don't worry about it!

#Read in your file that you made at the end of the code file labeled RCode_OpenAlexR.
Inst_Articles_InstCorresponding <- read.csv("DataOutput/Inst_Articles_InstCorresponding.csv")

#First we're going to create a table by publisher showing totals for all the articles where someone at your institution served as corresponding authors
##And then we'll turn that table into a dataframe that will be used to make a bar chart.
PublishersAll <- Inst_Articles_InstCorresponding %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersAll <- PublishersAll %>% 
  select(-c(valid_percent))


##In this next section, we're going to create subsets of these articles based on whether they published closed, hybrid, gold, diamond, or green OA
##And then we'll proceed along the same lines as above to make tables and then turn those tables into dataframes 
##So tht we can pull them all together into one visual.


#Hybrid OA articles
Inst_Articles_InstCorresponding_Hybrid <- subset(Inst_Articles_InstCorresponding, oa_status == "hybrid")

PublishersHybrid <- Inst_Articles_InstCorresponding_Hybrid %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersHybrid <- PublishersHybrid %>% 
  select(-c(valid_percent))

#Gold OA articles
Inst_Articles_InstCorresponding_Gold <- subset(Inst_Articles_InstCorresponding, oa_status == "gold")

PublishersGold <- Inst_Articles_InstCorresponding_Gold %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersGold <- PublishersGold %>% 
  select(-c(valid_percent))

#Closed (i.e. paywalled) articles
Inst_Articles_InstCorresponding_Closed <- subset(Inst_Articles_InstCorresponding, oa_status == "closed")

PublishersClosed <- Inst_Articles_InstCorresponding_Closed %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersClosed <- PublishersClosed %>% 
  select(-c(valid_percent))

#Green OA
Inst_Articles_InstCorresponding_Green <- subset(Inst_Articles_InstCorresponding, oa_status == "green")

PublishersGreen <- Inst_Articles_InstCorresponding_Green %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersGreen <- PublishersGreen %>% 
  select(-c(valid_percent))

#Diamond OA
Inst_Articles_InstCorresponding_Diamond <- subset(Inst_Articles_InstCorresponding, oa_status == "diamond")

PublishersDiamond <- Inst_Articles_InstCorresponding_Diamond %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersDiamond <- PublishersDiamond %>% 
  select(-c(valid_percent))

#Bronze OA
Inst_Articles_InstCorresponding_Bronze <- subset(Inst_Articles_InstCorresponding, oa_status == "bronze")

PublishersBronze <- Inst_Articles_InstCorresponding_Bronze %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersBronze <- PublishersBronze %>% 
  select(-c(valid_percent))

#####Visuals####
#We can now use the dataframes (DF) we made above and bring them together to help us create a bar chart. But first, we need to do a little cleaning
#Rename the Freq variable in each DF to the OA type so the totals for each have their own column when brought together.
PublishersAll <- PublishersAll %>% rename(All = n)
PublishersGold <- PublishersGold %>% rename(Gold = n)
PublishersHybrid <- PublishersHybrid %>% rename(Hybrid = n)
PublishersClosed <- PublishersClosed %>% rename(Closed = n)
PublishersGreen <- PublishersGreen %>% rename(Green = n)
PublishersDiamond <- PublishersDiamond %>% rename(Diamond = n)
PublishersBronze <- PublishersBronze %>% rename(Bronze = n)

#Now we can join the DFs together in a new DF called Publishers based on the common variable of Var1, which is the publisher name
Publishers <- PublishersAll %>%
  full_join(PublishersGold, by = "host_organization_name") %>%
  full_join(PublishersHybrid, by = "host_organization_name") %>% 
  full_join(PublishersGreen, by = "host_organization_name") %>%
  full_join(PublishersDiamond, by = "host_organization_name") %>%
  full_join(PublishersClosed, by = "host_organization_name") %>% 
  full_join(PublishersBronze, by = "host_organization_name")

#You should see a larger table if you open Publishers that includes columns for Var1, All, Gold, Hybrid, Green, Diamond and Closed

#Let's set any NAs to 0s
Publishers <- Publishers %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Green = 0, Diamond = 0, Closed = 0, Bronze = 0))

#And then rename the Var1 column to Publisher
Publishers <- Publishers %>%
  rename(Publisher = host_organization_name)

#Reorder by the All column and is descending. If you wish to reorder by a different column, just replace All below with the name you want.
Publishers[order(-Publishers$All), ]

#Save our work
write.csv(Publishers, "DataOUtput/Publishers.csv")

#Now we'll create another DF, PublishersPercent that shows our table but by percentage instead of count
PublishersPercent <- Publishers %>% 
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
PublishersPercent <- PublishersPercent %>% select(-one_of('Hybrid', 'Gold', 'Green', 'Diamond', 'Closed', 'All', 'Bronze'))

#And rename the columns to just the OA type
PublishersPercent <- PublishersPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Green = GreenPercent, Diamond = DiamondPercent, Closed = ClosedPercent, Bronze = BronzePercent)

#Again, save our work
write.csv(PublishersPercent, "DataOUtput/PublishersPercent.csv")


###Making the visual

#First, it can help to narrow down how many publishers are included in your chart if there are a lot. 
#The code below will select the top 10 rows from our Publisher table (the one showing the total counts)
#If you want more or less than 10, just change the number to what you want
Publishers <- head(Publishers, 10)

#Now we have to "melt" the Publishers DF to the appropriate format
long_Publishers <- Publishers %>%
  select(Publisher, Gold, Hybrid, Green, Diamond, Closed, Bronze) %>%
  pivot_longer(cols = c(Gold, Hybrid, Green, Diamond, Closed, Bronze), 
               names_to = "Type", 
               values_to = "Value")

#Run the below code to specify what color you want to use for each OA type in the grid
#The code comes with an accessible color scheme, but you can change them out just putting in a different HTML color code for each one
#Go to https://r-charts.com/color-palettes/ to find example color palettes
#Hover over a color to find its HEX number
#Cn also exlore https://r-graph-gallery.com/color-palette-finder
custom_colors <- c("Closed" = "#36638E", "Hybrid" = "#8B8E82", "Gold" = "#057BE7", "Diamond" = "#E95CCA", "Green" = "#B2C4DB")


# And here we Create the stacked bar chart
# This code involves three chunks that will also save the chart as a PNG in your Visuals folder in this project
# The first chunk sets the parameters for the saved image - you can change the width and height to whatever you prefer - just play around
#The second chunk creates the chart - if you don't want to save the image and just see it in RStudio, run just the second check.
#The third chunk, starting with dev.off, closes the command and saves the actual file.
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
