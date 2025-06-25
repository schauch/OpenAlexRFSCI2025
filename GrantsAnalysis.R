#This code file will walk you through doing some specific analysis on the OA output of authors from your institution
#You will need to have first worked your way through the code file labeled XXX.

#Read in your first file to analyze. You made this in the previous code file.
Inst_Articles_InstCorresponding_Grants <- read.csv("DataOutput/Inst_Articles_InstCorresponding.csv")

#See which were the most common publishers for grant funded articles with your institution's corresponding author
PublishersAllGrants <- table(Inst_Articles_InstCorresponding_Grants$host_organization_name)
PublishersAllGrants <- sort(PublishersAllGrants, decreasing = TRUE)
PublishersAllGrants <- as.data.frame(PublishersAllGrants)

#See which hybrid OA articles with a your institution's corresponding author had a grant
Inst_Articles_InstCorresponding_Hybrid_Grants <- subset(Inst_Articles_InstCorresponding_Grants, oa_status == "hybrid")

PublishersHybridGrants <- table(Inst_Articles_InstCorresponding_Hybrid_Grants$host_organization_name)
PublishersHybridGrants <- sort(PublishersHybridGrants, decreasing = TRUE)
PublishersHybridGrants <- as.data.frame(PublishersHybridGrants)

#See which gold OA grant-funded articles with a your institution's corresponding author 
Inst_Articles_InstCorresponding_Gold_Grants <- subset(Inst_Articles_InstCorresponding_Grants, oa_status == "gold")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersGoldGrants <- table(Inst_Articles_InstCorresponding_Gold_Grants$host_organization_name)
PublishersGoldGrants <- sort(PublishersGoldGrants, decreasing = TRUE)
PublishersGoldGrants <- as.data.frame(PublishersGoldGrants)

#See which closed articles with a your institution's corresponding author had a grant
Inst_Articles_InstCorresponding_Closed_Grants <- subset(Inst_Articles_InstCorresponding_Grants, oa_status == "closed")

#See which were the most common publishers for non-grant funded closed articles with your institution's corresponding author
PublishersClosedGrants <- table(Inst_Articles_InstCorresponding_Closed_Grants$host_organization_name)
PublishersClosedGrants <- sort(PublishersClosedGrants, decreasing = TRUE)
PublishersClosedGrants <- as.data.frame(PublishersClosedGrants)


#Bronze OA with your institution's corresponding author with grant funding
Inst_Articles_InstCorresponding_Bronze_Grants <- subset(Inst_Articles_InstCorresponding_Grants, oa_status == "bronze")

PublishersBronzeGrants <- table(Inst_Articles_InstCorresponding_Bronze_Grants$host_organization_name)
PublishersBronzeGrants <- sort(PublishersBronzeGrants, decreasing = TRUE)
PublishersBronzeGrants <- as.data.frame(PublishersBronzeGrants)


#Green OA articles with your institution's corresponding author with grant funding
Inst_Articles_InstCorresponding_Green_Grants <- subset(Inst_Articles_InstCorresponding_Grants, oa_status == "green")

PublishersGreenGrants <- table(Inst_Articles_InstCorresponding_Green_Grants$host_organization_name)
PublishersGreenGrants <- sort(PublishersGreenGrants, decreasing = TRUE)
PublishersGreenGrants <- as.data.frame(PublishersGreenGrants)


#Diamond OA articles with your institution's corresponding author with grant funding
Inst_Articles_InstCorresponding_Diamond_Grants <- subset(Inst_Articles_InstCorresponding_Grants, oa_status == "diamond")

PublishersDiamondGrants <- table(Inst_Articles_InstCorresponding_Diamond_Grants$host_organization_name)
PublishersDiamondGrants <- sort(PublishersDiamondGrants, decreasing = TRUE)
PublishersDiamondGrants <- as.data.frame(PublishersDiamondGrants)

#####Visuals####
#Combine all the publisher data into one
PublishersAllGrants <- PublishersAllGrants %>% rename(All = Freq)
PublishersGoldGrants <- PublishersGoldGrants %>% rename(Gold = Freq)
PublishersHybridGrants <- PublishersHybridGrants %>% rename(Hybrid = Freq)
PublishersClosedGrants <- PublishersClosedGrants %>% rename(Closed = Freq)
PublishersGreenGrants <- PublishersGreenGrants %>% rename(Green = Freq)
#PublishersBronzeGrants <- PublishersBronzeGrants %>% rename(Bronze = Freq)
PublishersDiamondGrants <- PublishersDiamondGrants %>% rename(Diamond = Freq)

###For all grant-funded articles with your institution's corresponding authors
PublishersGrants <- PublishersAllGrants %>%
  full_join(PublishersGoldGrants, by = "Var1") %>%
  full_join(PublishersHybridGrants, by = "Var1") %>% 
#  full_join(PublishersBronzeGrants, by = "Var1") %>% 
  full_join(PublishersGreenGrants, by = "Var1") %>%
  full_join(PublishersDiamondGrants, by = "Var1") %>%
  full_join(PublishersClosedGrants, by = "Var1")

#PublishersGrants <- PublishersGrants %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Bronze = 0, Green = 0, Diamond = 0, Closed = 0))
PublishersGrants <- PublishersGrants %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Green = 0, Diamond = 0, Closed = 0))


PublishersGrants <- PublishersGrants %>%
  rename(Publisher = Var1)

write.csv(PublishersGrants, "DataOUtput/PublishersGrants.csv")


#PublishersGrants <- PublishersGrants %>% 
#  mutate(GoldNonGrant = (Gold - GoldGrant),
#         HybridNonGrant = (Hybrid - HybridGrant),
#         AllPaidOA = (Gold + Hybrid),
#         GoldNonGrantPercent = (GoldNonGrant / Gold) * 100,
#         GoldNonGrantPercent = paste0(round(GoldNonGrant, 2), "%"),
#         HybridNonGrantPercent = (HybridNonGrant / Hybrid) * 100,
#         HybridNonGrantPercent = paste0(round(HybridNonGrantPercent, 2), "%"))

PublishersGrants <- PublishersGrants %>% 
  mutate(GoldPercent = (Gold / All) * 100,
         GoldPercent = paste0(round(GoldPercent, 2), "%"),
         HybridPercent = (Hybrid / All) * 100,
         HybridPercent = paste0(round(HybridPercent, 2), "%"),
         ClosedPercent = (Closed / All) * 100,
         ClosedPercent = paste0(round(ClosedPercent, 2), "%"),
#         BronzePercent = (Bronze / All) * 100,
#         BronzePercent = paste0(round(BronzePercent, 2), "%"),
         GreenPercent = (Green / All) * 100,
         GreenPercent = paste0(round(GreenPercent, 2), "%"),
         DiamondPercent = (Diamond / All) * 100,
         DiamondPercent = paste0(round(DiamondPercent, 2), "%"))

#PublishersGrantsPercent <- PublishersGrants %>% select(-one_of('Hybrid', 'Gold', 'Bronze', 'Green', 'Diamond', 'Closed', 'All'))
PublishersGrantsPercent <- PublishersGrants %>% select(-one_of('Hybrid', 'Gold', 'Green', 'Diamond', 'Closed', 'All'))

#PublishersGrantsPercent <- PublishersGrantsPercent %>%
#  rename(Gold = GoldPercent, Hybrid = HybridPercent, Brone = BronzePercent, Green = GreenPercent, Diamond = DiamondPercent, Closed = ClosedPercent)
PublishersGrantsPercent <- PublishersGrantsPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Green = GreenPercent, Diamond = DiamondPercent, Closed = ClosedPercent)


write.csv(PublishersGrantsPercent, "DataOutput/PublishersGrantsPercent.csv")


PublishersGrants <- head(PublishersGrants, 10)

# Melt the dataframe to long format
#long_PublishersGrants <- PublishersGrants %>%
#  select(Publisher, Gold, Hybrid, Bronze, Green, Diamond, Closed) %>%
#  pivot_longer(cols = c(Gold, Hybrid, Bronze, Green, Diamond, Closed), 
#               names_to = "Type", 
#               values_to = "Value")
long_PublishersGrants <- PublishersGrants %>%
  select(Publisher, Gold, Hybrid, Green, Diamond, Closed) %>%
  pivot_longer(cols = c(Gold, Hybrid, Green, Diamond, Closed), 
               names_to = "Type", 
               values_to = "Value")

custom_colors <- c("Closed" = "#36638E", "Hybrid" = "#8B8E82", "Gold" = "#057BE7", "Bronze" = "#E95CCA", "Green" = "#B2C4DB", "Diamond" = "#A97F86")


# Create the stacked bar chart
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