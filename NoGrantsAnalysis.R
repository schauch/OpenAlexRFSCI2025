#This code file will walk you through doing some specific analysis on the OA output of authors from your institution
#You will need to have first worked your way through the code file labeled XXX.

#Read in your first file to analyze. You made this in the previous code file.
Inst_Articles_InstCorresponding <- read.csv("DataOutput/Inst_Articles_InstCorresponding.csv")

#Subset of all articles with your institution as corresponding author and no grant listed
Inst_Articles_InstCorresponding_NoGrants <- subset(Inst_Articles_InstCorresponding, has_grant == "N")

#See which were the most common publishers for non-grant funded OA articles with your institution's corresponding author
PublishersAllNoGrants <- table(Inst_Articles_InstCorresponding_NoGrants$host_organization_name)
PublishersAllNoGrants <- sort(PublishersAllNoGrants, decreasing = TRUE)
PublishersAllNoGrants <- as.data.frame(PublishersAllNoGrants)

#Those without grants
Inst_Articles_InstCorresponding_Hybrid_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "hybrid")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersHybridNoGrants <- table(Inst_Articles_InstCorresponding_Hybrid_NoGrants$host_organization_name)
PublishersHybridNoGrants <- sort(PublishersHybridNoGrants, decreasing = TRUE)
PublishersHybridNoGrants <- as.data.frame(PublishersHybridNoGrants)


#Subset to gold OA non-grant funded articles with your institution as corresponding articles
Inst_Articles_InstCorresponding_Gold_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "gold")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution corresponding author
PublishersGoldNoGrants <- table(Inst_Articles_InstCorresponding_Gold_NoGrants$host_organization_name)
PublishersGoldNoGrants <- sort(PublishersGoldNoGrants, decreasing = TRUE)
PublishersGoldNoGrants <- as.data.frame(PublishersGoldNoGrants)

#Subset closed articles with your institution's corresponding author with no grant funding
Inst_Articles_InstCorresponding_Closed_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "closed")

#See which were most common publishers for all closed articles with Inst corresponding author
PublishersClosedNoGrants <- table(Inst_Articles_InstCorresponding_Closed_NoGrants$host_organization_name)
PublishersClosedNoGrants <- sort(PublishersClosedNoGrants, decreasing = TRUE)
PublishersClosedNoGrants <- as.data.frame(PublishersClosedNoGrants)

#Bronze OA with your institution's corresopnding author with no grant funding
Inst_Articles_InstCorresponding_Bronze_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "bronze")

PublishersBronzeNoGrants <- table(Inst_Articles_InstCorresponding_Bronze_NoGrants$host_organization_name)
PublishersBronzeNoGrants <- sort(PublishersBronzeNoGrants, decreasing = TRUE)
PublishersBronzeNoGrants <- as.data.frame(PublishersBronzeNoGrants)




df <- data.frame(rbind(PublishersBronzeNoGrants))



#Green OA articles with your institution's corresponding author with no grant funding
Inst_Articles_InstCorresponding_Green_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants, oa_status == "green")

PublishersGreenNoGrants <- table(Inst_Articles_InstCorresponding_Green_NoGrants$host_organization_name)
PublishersGreenNoGrants <- sort(PublishersGreenNoGrants, decreasing = TRUE)
PublishersGreenNoGrants <- as.data.frame(PublishersGreenNoGrants)


#Diamond OA articles with your institution's corresponding author with no grant funding
Inst_Articles_InstCorresponding_Diamond_NoGrants <- subset(Inst_Articles_InstCorresponding_NoGrants,oa_status == "diamond")

PublishersDiamondNoGrants <- table(Inst_Articles_InstCorresponding_Diamond_NoGrants$host_organization_name)
PublishersDiamondNoGrants <- sort(PublishersDiamondNoGrants, decreasing = TRUE)
PublishersDiamondNoGrants <- as.data.frame(PublishersDiamondNoGrants)

#####Visuals####
#Combine all the publisher data into one
PublishersAllNoGrants <- PublishersAllNoGrants %>% rename(All = Freq)
PublishersGoldNoGrants <- PublishersGoldNoGrants %>% rename(Gold = Freq)
PublishersHybridNoGrants <- PublishersHybridNoGrants %>% rename(Hybrid = Freq)
PublishersClosedNoGrants <- PublishersClosedNoGrants %>% rename(Closed = Freq)
#PublishersGreenNoGrants <- PublishersGreenNoGrants %>% rename(Green = Freq)
#PublishersBronzeNoGrants <- PublishersBronzeNoGrants %>% rename(Bronze = Freq)
PublishersDiamondNoGrants <- PublishersDiamondNoGrants %>% rename(Diamond = Freq)

###All non-granted funded articles with a your institution's corresponding author###
PublishersNoGrants <- PublishersAllNoGrants %>%
  full_join(PublishersGoldNoGrants, by = "Var1") %>%
  full_join(PublishersHybridNoGrants, by = "Var1") %>% 
#  full_join(PublishersBronzeNoGrants, by = "Var1") %>% 
#  full_join(PublishersGreenNoGrants, by = "Var1") %>%
  full_join(PublishersDiamondNoGrants, by = "Var1") %>%
  full_join(PublishersClosedNoGrants, by = "Var1")

PublishersNoGrants <- PublishersNoGrants %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Bronze = 0, Green = 0, Diamond = 0, Closed = 0))

PublishersNoGrants <- PublishersNoGrants %>%
  rename(Publisher = Var1)

write.csv(PublishersNoGrants, "DataOUtput/PublishersNoGrants.csv")

PublishersNoGrants <- PublishersNoGrants %>% 
  mutate(GoldPercent = (Gold / All) * 100,
         GoldPercent = paste0(round(GoldPercent, 2), "%"),
         HybridPercent = (Hybrid / All) * 100,
         HybridPercent = paste0(round(HybridPercent, 2), "%"),
         ClosedPercent = (Closed / All) * 100,
         ClosedPercent = paste0(round(ClosedPercent, 2), "%"),
#         BronzePercent = (Bronze / All) * 100,
#         BronzePercent = paste0(round(BronzePercent, 2), "%"),
#         GreenPercent = (Green / All) * 100,
#         GreenPercent = paste0(round(GreenPercent, 2), "%"),
         DiamondPercent = (Diamond / All) * 100,
         DiamondPercent = paste0(round(DiamondPercent, 2), "%"))

#PublishersNoGrantsPercent <- PublishersNoGrants %>% select(-one_of('Hybrid', 'Gold', 'Bronze', 'Green', 'Diamond', 'Closed', 'All'))
PublishersNoGrantsPercent <- PublishersNoGrants %>% select(-one_of('Hybrid', 'Gold', 'Diamond', 'Closed', 'All'))

#PublishersNoGrantsPercent <- PublishersNoGrantsPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Brone = BronzePercent, Green = GreenPercent, Diamond = DiamondPercent, Closed = ClosedPercent)
PublishersNoGrantsPercent <- PublishersNoGrantsPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Diamond = DiamondPercent, Closed = ClosedPercent)


write.csv(PublishersNoGrantsPercent, "DataOutput/PublishersNoGrantsPercent.csv")

PublishersNoGrants <- head(PublishersNoGrants, 10)

# Melt the dataframe to long format
#long_PublishersNoGrants <- PublishersNoGrants %>%
#  select(Publisher, Gold, Hybrid, Bronze, Green, Diamond, Closed) %>%
#  pivot_longer(cols = c(Gold, Hybrid, Bronze, Green, Diamond, Closed), 
#               names_to = "Type", 
#               values_to = "Value")
long_PublishersNoGrants <- PublishersNoGrants %>%
  select(Publisher, Gold, Hybrid, Diamond, Closed) %>%
  pivot_longer(cols = c(Gold, Hybrid, Diamond, Closed), 
               names_to = "Type", 
               values_to = "Value")

custom_colors <- c("Closed" = "#36638E", "Hybrid" = "#8B8E82", "Gold" = "#057BE7", "Bronze" = "#E95CCA", "Green" = "#B2C4DB", "Diamond" = "#A97F86")


# Create the stacked bar chart
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