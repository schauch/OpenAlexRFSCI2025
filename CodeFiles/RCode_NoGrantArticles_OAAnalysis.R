#This code file will walk you through doing some specific analysis on the OA output of authors from your institution
#Specifically on journal articles that were identified by OpenAlex as being NOT grant funded and where an author from your institution served as corresponding author.
#You will need to have first worked your way through the code file labeled RCode_OpenAlexR.

install.packages("janitor")

library(tidyverse)
library(janitor)

### NOTICE! This file uses the following code multiple times:
# PublishersAllGrants <- PublishersAllGrants %>% 
# select(-c(valid_percent))
# This removes a column that's not always there; so if you get an error, don't worry about it!

#Read in your file that you made at the end of the code file labeled RCode_OpenAlexR.
Inst_Articles_InstCorresponding <- read.csv("DataOutput/Inst_Articles_InstCorresponding.csv")

#First we're going to create a table by publisher showing totals for all articles with NO GRANT where someone at your institution served as corresponding authors
##And then we'll turn that table into a dataframe (DF) that will be used to make a bar chart.
Inst_Articles_InstCorresponding_NoGrants <- subset(Inst_Articles_InstCorresponding, has_grant == "N")

PublishersAllNoGrants <- Inst_Articles_InstCorresponding_NoGrants %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersAllNoGrants <- PublishersAllNoGrants %>% 
  select(-c(valid_percent))

##In this next section, we're going to create subsets of these articles based on whether they published closed, hybrid, gold, diamond, or green OA
##And then we'll proceed along the same lines as above to make tables and then turn those tables into DFs 
##so that we can pull them all together into one visual.

#Hybrid OA
Inst_Articles_InstCorresponding_Hybrid_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "hybrid")

PublishersHybridNoGrants <- Inst_Articles_InstCorresponding_Hybrid_NoGrants %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersHybridNoGrants <- PublishersHybridNoGrants %>% 
  select(-c(valid_percent))

#Gold OA
Inst_Articles_InstCorresponding_Gold_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "gold")

PublishersGoldNoGrants <- Inst_Articles_InstCorresponding_Gold_NoGrants %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersGoldNoGrants <- PublishersGoldNoGrants %>% 
  select(-c(valid_percent))

#Closed
Inst_Articles_InstCorresponding_Closed_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "closed")

PublishersClosedNoGrants <- Inst_Articles_InstCorresponding_Closed_NoGrants %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersClosedNoGrants <- PublishersClosedNoGrants %>% 
  select(-c(valid_percent))

#Green OA
Inst_Articles_InstCorresponding_Green_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "green")

PublishersGreenNoGrants <- Inst_Articles_InstCorresponding_Green_NoGrants %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersGreenNoGrants <- PublishersGreenNoGrants %>% 
  select(-c(valid_percent))

#Diamond OA
Inst_Articles_InstCorresponding_Diamond_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants,oa_status == "diamond")

PublishersDiamondNoGrants <- Inst_Articles_InstCorresponding_Diamond_NoGrants %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersDiamondNoGrants <- PublishersDiamondNoGrants %>% 
  select(-c(valid_percent))

#Bronze OA
Inst_Articles_InstCorresponding_Bronze_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants,oa_status == "bronze")

PublishersBronzeNoGrants <- Inst_Articles_InstCorresponding_Bronze_NoGrants %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n)) %>% 
  select(-c(percent))

PublishersBronzeNoGrants <- PublishersBronzeNoGrants %>% 
  select(-c(valid_percent))


#####Visuals####
#We can now use the DFs we made above and bring them together to help us create a bar chart. But first, we need to do a little cleaning
#Rename the Freq variable in each DF to the OA type so the totals for each have their own column when brought together.
PublishersAllNoGrants <- PublishersAllNoGrants %>% rename(All = n)
PublishersGoldNoGrants <- PublishersGoldNoGrants %>% rename(Gold = n)
PublishersHybridNoGrants <- PublishersHybridNoGrants %>% rename(Hybrid = n)
PublishersClosedNoGrants <- PublishersClosedNoGrants %>% rename(Closed = n)
PublishersGreenNoGrants <- PublishersGreenNoGrants %>% rename(Green = n)
PublishersDiamondNoGrants <- PublishersDiamondNoGrants %>% rename(Diamond = n)
PublishersBronzeNoGrants <- PublishersBronzeNoGrants %>% rename(Bronze = n)

#Now we can join the DFs together in a new DF called Publishers based on the common variable of Var1, which is the publisher name
PublishersNoGrants <- PublishersAllNoGrants %>%
  full_join(PublishersGoldNoGrants, by = "host_organization_name") %>%
  full_join(PublishersHybridNoGrants, by = "host_organization_name") %>% 
  full_join(PublishersGreenNoGrants, by = "host_organization_name") %>%
  full_join(PublishersDiamondNoGrants, by = "host_organization_name") %>%
  full_join(PublishersClosedNoGrants, by = "host_organization_name") %>% 
  full_join(PublishersBronzeNoGrants, by = "host_organization_name")

#You should see a larger table if you open Publishers that includes columns for Var1, All, Gold, Hybrid, Green, Diamond and Closed

#Let's set any NAs to 0s
PublishersNoGrants <- PublishersNoGrants %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Bronze = 0, Green = 0, Diamond = 0, Closed = 0))

#And then rename the Var1 column to Publisher
PublishersNoGrants <- PublishersNoGrants %>%
  rename(Publisher = host_organization_name)

#Reorder by the All column and is descending. If you wish to reorder by a different column, just replace All below with the name you want.
PublishersNoGrants[order(-PublishersNoGrants$All), ]

#Save your work
write.csv(PublishersNoGrants, "DataOUtput/PublishersNoGrants.csv")

#Now we'll create another DF, PublishersPercent, that shows our table but by percentage instead of count
PublishersNoGrants <- PublishersNoGrants %>% 
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
PublishersNoGrantsPercent <- PublishersNoGrants %>% select(-one_of('Hybrid', 'Gold', 'Diamond', 'Closed', 'All', 'Bronze', 'Green'))

#And rename the columns to just the OA type
PublishersNoGrantsPercent <- PublishersNoGrantsPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Diamond = DiamondPercent, Closed = ClosedPercent, Green = GreenPercent, Bronze = BronzePercent)

#Save your work
write.csv(PublishersNoGrantsPercent, "DataOutput/PublishersNoGrantsPercent.csv")

###Making the visual

#First, it can help to narrow down how many publishers are included in your chart if there are a lot. 
#The code below will select the top 10 rows from our Publisher table (the one showing the total counts)
#If you want more or less than 10, just change the number to what you want
PublishersNoGrants <- head(PublishersNoGrants, 10)

#Now we have to "melt" the Publishers DF to the appropriate format
long_PublishersNoGrants <- PublishersNoGrants %>%
  select(Publisher, Gold, Hybrid, Diamond, Closed, Green, Bronze) %>%
  pivot_longer(cols = c(Gold, Hybrid, Diamond, Closed, Green, Bronze), 
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
png(filename = "Visuals/PublishedArticlesNoGrants.png", 
    width = 6160, height = 3000, res = 600)
ggplot(long_PublishersNoGrants, aes(x = Publisher, y = Value, fill = Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal()
dev.off()