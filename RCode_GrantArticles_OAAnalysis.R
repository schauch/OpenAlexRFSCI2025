#This code file will walk you through doing some specific analysis on the OA output of authors from your institution
#Specifically on journal articles that were identified by OpenAlex as being grant funded and where an author from your institution served as corresponding author.
#You will need to have first worked your way through the code file labeled RCode_OpenAlexR.

library(tidyverse)

#Read in your first file to analyze. You made this in the previous code file labeled RCode_OpenAlexR.
Inst_Articles_InstCorresponding_Grants <- read.csv("DataOutput/Inst_Articles_InstCorresponding.csv")

#First we're going to create a table by publisher showing totals for grant-funded articles where someone at your institution served as corresponding authors
##And then we'll turn that table into a dataframe (DF) that will be used to make a bar chart.
PublishersAllGrants <- table(Inst_Articles_InstCorresponding_Grants$host_organization_name)
PublishersAllGrants <- sort(PublishersAllGrants, decreasing = TRUE)
PublishersAllGrants <- as.data.frame(PublishersAllGrants)

##In this next section, we're going to create subsets of these articles based on whether they published closed, hybrid, gold, diamond, or green OA
##And then we'll proceed along the same lines as above to make tables and then turn those tables into DFs 
##so that we can pull them all together into one visual.

#Hybrid OA
Inst_Articles_InstCorresponding_Hybrid_Grants <- subset(Inst_Articles_InstCorresponding_Grants, oa_status == "hybrid")
PublishersHybridGrants <- table(Inst_Articles_InstCorresponding_Hybrid_Grants$host_organization_name)
PublishersHybridGrants <- sort(PublishersHybridGrants, decreasing = TRUE)
PublishersHybridGrants <- as.data.frame(PublishersHybridGrants)

#Gold OA
Inst_Articles_InstCorresponding_Gold_Grants <- subset(Inst_Articles_InstCorresponding_Grants, oa_status == "gold")
PublishersGoldGrants <- table(Inst_Articles_InstCorresponding_Gold_Grants$host_organization_name)
PublishersGoldGrants <- sort(PublishersGoldGrants, decreasing = TRUE)
PublishersGoldGrants <- as.data.frame(PublishersGoldGrants)

#Closed
Inst_Articles_InstCorresponding_Closed_Grants <- subset(Inst_Articles_InstCorresponding_Grants, oa_status == "closed")
PublishersClosedGrants <- table(Inst_Articles_InstCorresponding_Closed_Grants$host_organization_name)
PublishersClosedGrants <- sort(PublishersClosedGrants, decreasing = TRUE)
PublishersClosedGrants <- as.data.frame(PublishersClosedGrants)

#Green OA
Inst_Articles_InstCorresponding_Green_Grants <- subset(Inst_Articles_InstCorresponding_Grants, oa_status == "green")
PublishersGreenGrants <- table(Inst_Articles_InstCorresponding_Green_Grants$host_organization_name)
PublishersGreenGrants <- sort(PublishersGreenGrants, decreasing = TRUE)
PublishersGreenGrants <- as.data.frame(PublishersGreenGrants)

#Diamond OA
Inst_Articles_InstCorresponding_Diamond_Grants <- subset(Inst_Articles_InstCorresponding_Grants, oa_status == "diamond")
PublishersDiamondGrants <- table(Inst_Articles_InstCorresponding_Diamond_Grants$host_organization_name)
PublishersDiamondGrants <- sort(PublishersDiamondGrants, decreasing = TRUE)
PublishersDiamondGrants <- as.data.frame(PublishersDiamondGrants)

#####Visuals####
#We can now use the DFs we made above and bring them together to help us create a bar chart. But first, we need to do a little cleaning
#Rename the Freq variable in each DF to the OA type so the totals for each have their own column when brought together.
PublishersAllGrants <- PublishersAllGrants %>% rename(All = Freq)
PublishersGoldGrants <- PublishersGoldGrants %>% rename(Gold = Freq)
PublishersHybridGrants <- PublishersHybridGrants %>% rename(Hybrid = Freq)
PublishersClosedGrants <- PublishersClosedGrants %>% rename(Closed = Freq)
PublishersGreenGrants <- PublishersGreenGrants %>% rename(Green = Freq)
PublishersDiamondGrants <- PublishersDiamondGrants %>% rename(Diamond = Freq)

#Now we can join the DFs together in a new DF called Publishers based on the common variable of Var1, which is the publisher name
PublishersGrants <- PublishersAllGrants %>%
  full_join(PublishersGoldGrants, by = "Var1") %>%
  full_join(PublishersHybridGrants, by = "Var1") %>% 
  full_join(PublishersGreenGrants, by = "Var1") %>%
  full_join(PublishersDiamondGrants, by = "Var1") %>%
  full_join(PublishersClosedGrants, by = "Var1")
#You should see a larger table if you open Publishers that includes columns for Var1, All, Gold, Hybrid, Green, Diamond and Closed

#Let's set any NAs to 0s
PublishersGrants <- PublishersGrants %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Green = 0, Diamond = 0, Closed = 0))

#And then rename the Var1 column to Publisher
PublishersGrants <- PublishersGrants %>%
  rename(Publisher = Var1)

#Reorder by the All column and is descending. If you wish to reorder by a different column, just replace All below with the name you want.
Publishers[order(-Publishers$All), ]

#Save our work
write.csv(PublishersGrants, "DataOUtput/PublishersGrants.csv")

#Now we'll create another DF, PublishersPercent that shows our table but by percentage instead of count
PublishersGrants <- PublishersGrants %>% 
  mutate(GoldPercent = (Gold / All) * 100,
         GoldPercent = paste0(round(GoldPercent, 2), "%"),
         HybridPercent = (Hybrid / All) * 100,
         HybridPercent = paste0(round(HybridPercent, 2), "%"),
         ClosedPercent = (Closed / All) * 100,
         ClosedPercent = paste0(round(ClosedPercent, 2), "%"),
         GreenPercent = (Green / All) * 100,
         GreenPercent = paste0(round(GreenPercent, 2), "%"),
         DiamondPercent = (Diamond / All) * 100,
         DiamondPercent = paste0(round(DiamondPercent, 2), "%"))

#It kept our original columns showing counts, so we'll remove those now
PublishersGrantsPercent <- PublishersGrants %>% select(-one_of('Hybrid', 'Gold', 'Green', 'Diamond', 'Closed', 'All'))

#And rename the columns to just the OA type
PublishersGrantsPercent <- PublishersGrantsPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Green = GreenPercent, Diamond = DiamondPercent, Closed = ClosedPercent)

#Save our work
write.csv(PublishersGrantsPercent, "DataOutput/PublishersGrantsPercent.csv")

###Making the visual

#First, it can help to narrow down how many publishers are included in your chart if there are a lot. 
#The code below will select the top 10 rows from our Publisher table (the one showing the total counts)
#If you want more or less than 10, just change the number to what you want
PublishersGrants <- head(PublishersGrants, 10)

#Now we have to "melt" the Publishers DF to the appropriate format
long_PublishersGrants <- PublishersGrants %>%
  select(Publisher, Gold, Hybrid, Green, Diamond, Closed) %>%
  pivot_longer(cols = c(Gold, Hybrid, Green, Diamond, Closed), 
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