#This code file will walk you through doing some specific analysis on the OA output of authors from your institution
#Specifically on journal articles that were identified by OpenAlex as being NOT grant funded and where an author from your institution served as corresponding author.
#You will need to have first worked your way through the code file labeled RCode_OpenAlexR.

library(tidyverse)

#Read in your file that you made at the end of the code file labeled RCode_OpenAlexR.
Inst_Articles_InstCorresponding <- read.csv("DataOutput/Inst_Articles_InstCorresponding.csv")

#First we're going to create a table by publisher showing totals for all articles with NO GRANT where someone at your institution served as corresponding authors
##And then we'll turn that table into a dataframe (DF) that will be used to make a bar chart.
Inst_Articles_InstCorresponding_NoGrants <- subset(Inst_Articles_InstCorresponding, has_grant == "N")
PublishersAllNoGrants <- table(Inst_Articles_InstCorresponding_NoGrants$host_organization_name)
PublishersAllNoGrants <- sort(PublishersAllNoGrants, decreasing = TRUE)
PublishersAllNoGrants <- as.data.frame(PublishersAllNoGrants)

##In this next section, we're going to create subsets of these articles based on whether they published closed, hybrid, gold, diamond, or green OA
##And then we'll proceed along the same lines as above to make tables and then turn those tables into DFs 
##so that we can pull them all together into one visual.

#Hybrid OA
Inst_Articles_InstCorresponding_Hybrid_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "hybrid")
PublishersHybridNoGrants <- table(Inst_Articles_InstCorresponding_Hybrid_NoGrants$host_organization_name)
PublishersHybridNoGrants <- sort(PublishersHybridNoGrants, decreasing = TRUE)
PublishersHybridNoGrants <- as.data.frame(PublishersHybridNoGrants)


#Gold OA
Inst_Articles_InstCorresponding_Gold_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "gold")
PublishersGoldNoGrants <- table(Inst_Articles_InstCorresponding_Gold_NoGrants$host_organization_name)
PublishersGoldNoGrants <- sort(PublishersGoldNoGrants, decreasing = TRUE)
PublishersGoldNoGrants <- as.data.frame(PublishersGoldNoGrants)

#Closed
Inst_Articles_InstCorresponding_Closed_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "closed")
PublishersClosedNoGrants <- table(Inst_Articles_InstCorresponding_Closed_NoGrants$host_organization_name)
PublishersClosedNoGrants <- sort(PublishersClosedNoGrants, decreasing = TRUE)
PublishersClosedNoGrants <- as.data.frame(PublishersClosedNoGrants)

#Green OA
Inst_Articles_InstCorresponding_Green_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "green")
PublishersGreenNoGrants <- table(Inst_Articles_InstCorresponding_Green_NoGrants$host_organization_name)
PublishersGreenNoGrants <- sort(PublishersGreenNoGrants, decreasing = TRUE)
PublishersGreenNoGrants <- as.data.frame(PublishersGreenNoGrants)

#Diamond OA
Inst_Articles_InstCorresponding_Diamond_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants,oa_status == "diamond")
PublishersDiamondNoGrants <- table(Inst_Articles_InstCorresponding_Diamond_NoGrants$host_organization_name)
PublishersDiamondNoGrants <- sort(PublishersDiamondNoGrants, decreasing = TRUE)
PublishersDiamondNoGrants <- as.data.frame(PublishersDiamondNoGrants)

#####Visuals####
#We can now use the DFs we made above and bring them together to help us create a bar chart. But first, we need to do a little cleaning
#Rename the Freq variable in each DF to the OA type so the totals for each have their own column when brought together.
PublishersAllNoGrants <- PublishersAllNoGrants %>% rename(All = Freq)
PublishersGoldNoGrants <- PublishersGoldNoGrants %>% rename(Gold = Freq)
PublishersHybridNoGrants <- PublishersHybridNoGrants %>% rename(Hybrid = Freq)
PublishersClosedNoGrants <- PublishersClosedNoGrants %>% rename(Closed = Freq)
#PublishersGreenNoGrants <- PublishersGreenNoGrants %>% rename(Green = Freq)
PublishersDiamondNoGrants <- PublishersDiamondNoGrants %>% rename(Diamond = Freq)

#Now we can join the DFs together in a new DF called Publishers based on the common variable of Var1, which is the publisher name
PublishersNoGrants <- PublishersAllNoGrants %>%
  full_join(PublishersGoldNoGrants, by = "Var1") %>%
  full_join(PublishersHybridNoGrants, by = "Var1") %>% 
#  full_join(PublishersBronzeNoGrants, by = "Var1") %>% 
#  full_join(PublishersGreenNoGrants, by = "Var1") %>%
  full_join(PublishersDiamondNoGrants, by = "Var1") %>%
  full_join(PublishersClosedNoGrants, by = "Var1")
#You should see a larger table if you open Publishers that includes columns for Var1, All, Gold, Hybrid, Green, Diamond and Closed

#Let's set any NAs to 0s
PublishersNoGrants <- PublishersNoGrants %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Bronze = 0, Green = 0, Diamond = 0, Closed = 0))

#And then rename the Var1 column to Publisher
PublishersNoGrants <- PublishersNoGrants %>%
  rename(Publisher = Var1)

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
         ClosedPercent = (Closed / All) * 100,
         ClosedPercent = paste0(round(ClosedPercent, 2), "%"),
#         GreenPercent = (Green / All) * 100,
#         GreenPercent = paste0(round(GreenPercent, 2), "%"),
         DiamondPercent = (Diamond / All) * 100,
         DiamondPercent = paste0(round(DiamondPercent, 2), "%"))

#It kept our original columns showing counts, so we'll remove those now
PublishersNoGrantsPercent <- PublishersNoGrants %>% select(-one_of('Hybrid', 'Gold', 'Diamond', 'Closed', 'All'))

#And rename the columns to just the OA type
PublishersNoGrantsPercent <- PublishersNoGrantsPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Diamond = DiamondPercent, Closed = ClosedPercent)

#Save your work
write.csv(PublishersNoGrantsPercent, "DataOutput/PublishersNoGrantsPercent.csv")

###Making the visual

#First, it can help to narrow down how many publishers are included in your chart if there are a lot. 
#The code below will select the top 10 rows from our Publisher table (the one showing the total counts)
#If you want more or less than 10, just change the number to what you want
PublishersNoGrants <- head(PublishersNoGrants, 10)

#Now we have to "melt" the Publishers DF to the appropriate format
long_PublishersNoGrants <- PublishersNoGrants %>%
  select(Publisher, Gold, Hybrid, Diamond, Closed) %>%
  pivot_longer(cols = c(Gold, Hybrid, Diamond, Closed), 
               names_to = "Type", 
               values_to = "Value")

#Run the below code to specify what color you want to use for each OA type in the grid
#The code comes with an accessible color scheme, but you can change them out just putting in a different HTML color code for each one
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