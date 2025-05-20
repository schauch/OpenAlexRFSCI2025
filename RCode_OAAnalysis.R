#This code file will walk you through doing some specific analysis on the OA output of authors from your institution
#You will need to have first worked your way through the code file labeled XXX.

#Read in your first file to analyze. You made this in the previous code file.
Inst_Articles_InstCorresponding <- read.csv("DataOutput/Inst_Articles_InstCorresponding.csv")

#See where your institution's authors published in total as corresponding authors
PublishersAll <- table(Inst_Articles_InstCorresponding$host_organization_name)
PublishersAll <- sort(PublishersAll, decreasing = TRUE)
PublishersAll <- as.data.frame(PublishersAll)

#Subset of all articles with your institution as corresponding author and no grant listed
Inst_Articles_InstCorresponding_NoGrants <- subset(Inst_Articles_InstCorresponding, has_grant == "N")

#See which were the most common publishers for non-grant funded OA articles with your institution's corresponding author
PublishersAllNoGrants <- table(Inst_Articles_InstCorresponding_NoGrants$host_organization_name)
PublishersAllNoGrants <- sort(PublishersAllNoGrants, decreasing = TRUE)
PublishersAllNoGrants <- as.data.frame(PublishersAllNoGrants)

#Subset of all articles with your institution as corresponding and grant listed
Inst_Articles_InstCorresponding_Grants <- subset(Inst_Articles_InstCorresponding, has_grant == "Y")

#See which were the most common publishers for grant funded articles with your institution's corresponding author
PublishersAllGrants <- table(Inst_Articles_InstCorresponding_Grants$host_organization_name)
PublishersAllGrants <- sort(PublishersAllGrants, decreasing = TRUE)
PublishersAllGrants <- as.data.frame(PublishersAllGrants)

###Create subset to see which authors have paid for hybrid OA
Inst_Articles_InstCorresponding_Hybrid <- subset(Inst_Articles_InstCorresponding, oa_status == "hybrid")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersHybrid <- table(Inst_Articles_InstCorresponding_Hybrid$host_organization_name)
PublishersHybrid <- sort(PublishersHybrid, decreasing = TRUE)

PublishersHybrid <- as.data.frame(PublishersHybrid)

#Those without grants
Inst_Articles_InstCorresponding_Hybrid_NoGrants <- subset(Inst_Articles_InstCorresponding_Hybrid, has_grant == "N")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersHybridNoGrants <- table(Inst_Articles_InstCorresponding_Hybrid_NoGrants$host_organization_name)
PublishersHybridNoGrants <- sort(PublishersHybridNoGrants, decreasing = TRUE)
PublishersHybridNoGrants <- as.data.frame(PublishersHybridNoGrants)

#See which hybrid OA articles with a your institution's corresponding author had a grant
Inst_Articles_InstCorresponding_Hybrid_Grants <- subset(Inst_Articles_InstCorresponding, has_grant == "Y" & oa_status == "hybrid")

PublishersHybridGrants <- table(Inst_Articles_InstCorresponding_Hybrid_Grants$host_organization_name)
PublishersHybridGrants <- sort(PublishersHybridGrants, decreasing = TRUE)
PublishersHybridGrants <- as.data.frame(PublishersHybridGrants)

####See which authors have paid for gold OA
Inst_Articles_InstCorresponding_Gold <- subset(Inst_Articles_InstCorresponding, oa_status == "gold")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersGold <- table(Inst_Articles_InstCorresponding_Gold$host_organization_name)
PublishersGold <- sort(PublishersGold, decreasing = TRUE)
PublishersGold <- as.data.frame(PublishersGold)

#Subset to gold OA non-grant funded articles with your institution as corresponding articles
Inst_Articles_InstCorresponding_Gold_NoGrants <- subset(Inst_Articles_InstCorresponding_Gold, has_grant == "N")

#Combine and export lists of gold & hybrid OA articles with your institution corresponding so I can manually check for grants
Inst_Articles_InstCorresponding_HybridGold_NoGrants <- bind_rows(Inst_Articles_InstCorresponding_Hybrid_NoGrants, Inst_Articles_InstCorresponding_Gold_NoGrants)

write.csv(Inst_Articles_InstCorresponding_HybridGold_NoGrants, "DataOUtput/Inst_Articles_InstCorresponding_HybridGold_NoGrants.csv" )


#See which were the most common publishers for non-grant funded hybrid OA articles with your institution corresponding author
PublishersGoldNoGrants <- table(Inst_Articles_InstCorresponding_Gold_NoGrants$host_organization_name)
PublishersGoldNoGrants <- sort(PublishersGoldNoGrants, decreasing = TRUE)
PublishersGoldNoGrants <- as.data.frame(PublishersGoldNoGrants)

#See which gold OA grant-funded articles with a your institution's corresponding author 
Inst_Articles_InstCorresponding_Gold_Grants <- subset(Inst_Articles_InstCorresponding_Gold, has_grant == "Y")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersGoldGrants <- table(Inst_Articles_InstCorresponding_Gold_Grants$host_organization_name)
PublishersGoldGrants <- sort(PublishersGoldGrants, decreasing = TRUE)
PublishersGoldGrants <- as.data.frame(PublishersGoldGrants)

###See which ones were closed with a your institution's corresponding author
Inst_Articles_InstCorresponding_Closed <- subset(Inst_Articles_InstCorresponding, oa_status == "closed")

PublishersClosed <- table(Inst_Articles_InstCorresponding_Closed$host_organization_name)
PublishersClosed <- sort(PublishersClosed, decreasing = TRUE)
PublishersClosed <- as.data.frame(PublishersClosed)

#Subset closed articles with your institution's corresponding author with no grant funding
Inst_Articles_InstCorresponding_Closed_NoGrants <- subset(Inst_Articles_InstCorresponding, has_grant == "N" & oa_status == "closed")

#See which were most common publishers for all closed articles with Inst corresponding author
PublishersClosedNoGrants <- table(Inst_Articles_InstCorresponding_Closed_NoGrants$host_organization_name)
PublishersClosedNoGrants <- sort(PublishersClosedNoGrants, decreasing = TRUE)
PublishersClosedNoGrants <- as.data.frame(PublishersClosedNoGrants)

#See which closed articles with a your institution's corresponding author had a grant
Inst_Articles_InstCorresponding_Closed_Grants <- subset(Inst_Articles_InstCorresponding, has_grant == "Y" & oa_status == "closed")

#See which were the most common publishers for non-grant funded closed articles with your institution's corresponding author
PublishersClosedGrants <- table(Inst_Articles_InstCorresponding_Closed_Grants$host_organization_name)
PublishersClosedGrants <- sort(PublishersClosedGrants, decreasing = TRUE)
PublishersClosedGrants <- as.data.frame(PublishersClosedGrants)

###Bronze OA articles
Inst_Articles_InstCorresponding_Bronze <- subset(Inst_Articles_InstCorresponding, oa_status == "bronze")

PublishersBronze <- table(Inst_Articles_InstCorresponding_Bronze$host_organization_name)
PublishersBronze <- sort(PublishersBronze, decreasing = TRUE)
PublishersBronze <- as.data.frame(PublishersBronze)

#Bronze OA with your institution's corresopnding author with no grant funding
Inst_Articles_InstCorresponding_Bronze_NoGrants <- subset(Inst_Articles_InstCorresponding_Bronze, has_grant == "N")

PublishersBronzeNoGrants <- table(Inst_Articles_InstCorresponding_Bronze_NoGrants$host_organization_name)
PublishersBronzeNoGrants <- sort(PublishersBronzeNoGrants, decreasing = TRUE)
PublishersBronzeNoGrants <- as.data.frame(PublishersBronzeNoGrants)

#Bronze OA with your institution's corresopnding author with grant funding
Inst_Articles_InstCorresponding_Bronze_Grants <- subset(Inst_Articles_InstCorresponding_Bronze, has_grant == "Y")

PublishersBronzeGrants <- table(Inst_Articles_InstCorresponding_Bronze_Grants$host_organization_name)
PublishersBronzeGrants <- sort(PublishersBronzeGrants, decreasing = TRUE)
PublishersBronzeGrants <- as.data.frame(PublishersBronzeGrants)

###Green OA
Inst_Articles_InstCorresponding_Green <- subset(Inst_Articles_InstCorresponding, oa_status == "green")

PublishersGreen <- table(Inst_Articles_InstCorresponding_Green$host_organization_name)
PublishersGreen <- sort(PublishersGreen, decreasing = TRUE)
PublishersGreen <- as.data.frame(PublishersGreen)

#Green OA articles with your institution's corresponding author with no grant funding
Inst_Articles_InstCorresponding_Green_NoGrants <- subset(Inst_Articles_InstCorresponding_Green, has_grant == "N")

PublishersGreenNoGrants <- table(Inst_Articles_InstCorresponding_Green_NoGrants$host_organization_name)
PublishersGreenNoGrants <- sort(PublishersGreenNoGrants, decreasing = TRUE)
PublishersGreenNoGrants <- as.data.frame(PublishersGreenNoGrants)

#Green OA articles with your institution's corresponding author with grant funding
Inst_Articles_InstCorresponding_Green_Grants <- subset(Inst_Articles_InstCorresponding_Green, has_grant == "Y")

PublishersGreenGrants <- table(Inst_Articles_InstCorresponding_Green_Grants$host_organization_name)
PublishersGreenGrants <- sort(PublishersGreenGrants, decreasing = TRUE)
PublishersGreenGrants <- as.data.frame(PublishersGreenGrants)


###Diamond OA
Inst_Articles_InstCorresponding_Diamond <- subset(Inst_Articles_InstCorresponding, oa_status == "diamond")

PublishersDiamond <- table(Inst_Articles_InstCorresponding_Diamond$host_organization_name)
PublishersDiamond <- sort(PublishersDiamond, decreasing = TRUE)
PublishersDiamond <- as.data.frame(PublishersDiamond)

#Diamond OA articles with your institution's corresponding author with no grant funding
Inst_Articles_InstCorresponding_Diamond_NoGrants <- subset(Inst_Articles_InstCorresponding_Diamond, has_grant == "N")

PublishersDiamondNoGrants <- table(Inst_Articles_InstCorresponding_Diamond_NoGrants$host_organization_name)
PublishersDiamondNoGrants <- sort(PublishersDiamondNoGrants, decreasing = TRUE)
PublishersDiamondNoGrants <- as.data.frame(PublishersDiamondNoGrants)

#Diamond OA articles with your institution's corresponding author with grant funding
Inst_Articles_InstCorresponding_Diamond_Grants <- subset(Inst_Articles_InstCorresponding_Diamond, has_grant == "Y")

PublishersDiamondGrants <- table(Inst_Articles_InstCorresponding_Diamond_Grants$host_organization_name)
PublishersDiamondGrants <- sort(PublishersDiamondGrants, decreasing = TRUE)
PublishersDiamondGrants <- as.data.frame(PublishersDiamondGrants)

#####Visuals####
#Combine all the publisher data into one
PublishersAll <- PublishersAll %>% rename(All = Freq)
PublishersAllGrants <- PublishersAllGrants %>% rename(All = Freq)
PublishersAllNoGrants <- PublishersAllNoGrants %>% rename(All = Freq)
PublishersGold <- PublishersGold %>% rename(Gold = Freq)
PublishersGoldGrants <- PublishersGoldGrants %>% rename(Gold = Freq)
PublishersGoldNoGrants <- PublishersGoldNoGrants %>% rename(Gold = Freq)
PublishersHybrid <- PublishersHybrid %>% rename(Hybrid = Freq)
PublishersHybridGrants <- PublishersHybridGrants %>% rename(Hybrid = Freq)
PublishersHybridNoGrants <- PublishersHybridNoGrants %>% rename(Hybrid = Freq)
PublishersClosed <- PublishersClosed %>% rename(Closed = Freq)
PublishersClosedGrants <- PublishersClosedGrants %>% rename(Closed = Freq)
PublishersClosedNoGrants <- PublishersClosedNoGrants %>% rename(Closed = Freq)
PublishersGreen <- PublishersGreen %>% rename(Green = Freq)
PublishersGreenGrants <- PublishersGreenGrants %>% rename(Green = Freq)
PublishersGreenNoGrants <- PublishersGreenNoGrants %>% rename(Green = Freq)
PublishersBronze <- PublishersBronze %>% rename(Bronze = Freq)
PublishersBronzeGrants <- PublishersBronzeGrants %>% rename(Bronze = Freq)
PublishersBronzeNoGrants <- PublishersBronzeNoGrants %>% rename(Bronze = Freq)
PublishersDiamond <- PublishersDiamond %>% rename(Diamond = Freq)
PublishersDiamondGrants <- PublishersDiamondGrants %>% rename(Diamond = Freq)
PublishersDiamondNoGrants <- PublishersDiamondNoGrants %>% rename(Diamond = Freq)


Publishers <- PublishersAll %>%
  full_join(PublishersGold, by = "Var1") %>%
  full_join(PublishersHybrid, by = "Var1") %>% 
#  full_join(PublishersBronze, by = "Var1") %>% 
  full_join(PublishersGreen, by = "Var1") %>%
  full_join(PublishersDiamond, by = "Var1") %>%
  full_join(PublishersClosed, by = "Var1")

Publishers <- Publishers %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Green = 0, Diamond = 0, Closed = 0))

Publishers <- Publishers %>%
  rename(Publisher = Var1)

Publishers[order(-Publishers$All), ]

write.csv(Publishers, "DataOUtput/Publishers.csv")

#Publishers <- Publishers %>% 
#  mutate(TotalPaidOA = (Gold + Hybrid))


PublishersPercent <- Publishers %>% 
  mutate(GoldPercent = (Gold / All) * 100,
         GoldPercent = paste0(round(GoldPercent, 2), "%"),
         HybridPercent = (Hybrid / All) * 100,
         HybridPercent = paste0(round(HybridPercent, 2), "%"),
         ClosedPercent = (Closed / All) * 100,
         ClosedPercent = paste0(round(ClosedPercent, 2), "%"),
        # BronzePercent = (Bronze / All) * 100,
         #BronzePercent = paste0(round(BronzePercent, 2), "%"),
         GreenPercent = (Green / All) * 100,
         GreenPercent = paste0(round(GreenPercent, 2), "%"),
         DiamondPercent = (Diamond / All) * 100,
         DiamondPercent = paste0(round(DiamondPercent, 2), "%"))

# Publishers <- Publishers %>% 
#  mutate(TotalPaidOAPercent = (TotalPaidOA / All) * 100,
#         TotalPaidOAPercent = paste0(round(TotalPaidOAPercent, 2), "%"))

PublishersPercent <- PublishersPercent %>% select(-one_of('Hybrid', 'Gold', 'Green', 'Diamond', 'Closed', 'All'))

PublishersPercent <- PublishersPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Green = GreenPercent, Diamond = DiamondPercent, Closed = ClosedPercent)

write.csv(PublishersPercent, "DataOUtput/PublishersPercent.csv")


#Publishers <- Publishers %>% 
#  mutate(NonPaidOA = All - Gold - Hybrid)

Publishers <- head(Publishers, 10)

# Melt the dataframe to long format
long_Publishers <- Publishers %>%
  select(Publisher, Gold, Hybrid, Green, Diamond, Closed) %>%
  pivot_longer(cols = c(Gold, Hybrid, Green, Diamond, Closed), 
               names_to = "Type", 
               values_to = "Value")

custom_colors <- c("Closed" = "#36638E", "Hybrid" = "#8B8E82", "Gold" = "#057BE7", "Bronze" = "#E95CCA", "Green" = "#B2C4DB")


# Create the stacked bar chart
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

###For all grant-funded articles with your institution's corresponding authors
PublishersGrants <- PublishersAllGrants %>%
  full_join(PublishersGoldGrants, by = "Var1") %>%
  full_join(PublishersHybridGrants, by = "Var1") %>% 
  full_join(PublishersBronzeGrants, by = "Var1") %>% 
  full_join(PublishersGreenGrants, by = "Var1") %>%
  full_join(PublishersDiamondGrants, by = "Var1") %>%
  full_join(PublishersClosedGrants, by = "Var1")

PublishersGrants <- PublishersGrants %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Bronze = 0, Green = 0, Diamond = 0, Closed = 0))

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
         BronzePercent = (Bronze / All) * 100,
         BronzePercent = paste0(round(BronzePercent, 2), "%"),
         GreenPercent = (Green / All) * 100,
         GreenPercent = paste0(round(GreenPercent, 2), "%"),
         DiamondPercent = (Diamond / All) * 100,
         DiamondPercent = paste0(round(DiamondPercent, 2), "%"))

PublishersGrantsPercent <- PublishersGrants %>% select(-one_of('Hybrid', 'Gold', 'Bronze', 'Green', 'Diamond', 'Closed', 'All'))

PublishersGrantsPercent <- PublishersGrantsPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Brone = BronzePercent, Green = GreenPercent, Diamond = DiamondPercent, Closed = ClosedPercent)


write.csv(PublishersGrantsPercent, "DataOutput/PublishersGrantsPercent.csv")


PublishersGrants <- head(PublishersGrants, 10)

# Melt the dataframe to long format
long_PublishersGrants <- PublishersGrants %>%
  select(Publisher, Gold, Hybrid, Bronze, Green, Diamond, Closed) %>%
  pivot_longer(cols = c(Gold, Hybrid, Bronze, Green, Diamond, Closed), 
               names_to = "Type", 
               values_to = "Value")

custom_colors <- c("Closed" = "#36638E", "Hybrid" = "#8B8E82", "Gold" = "#057BE7", "Bronze" = "#E95CCA", "Green" = "#B2C4DB", "Closed" = "#A97F86")


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


###All non-granted funded articles with a your institution's corresponding author###
PublishersNoGrants <- PublishersAllNoGrants %>%
  full_join(PublishersGoldNoGrants, by = "Var1") %>%
  full_join(PublishersHybridNoGrants, by = "Var1") %>% 
  full_join(PublishersBronzeNoGrants, by = "Var1") %>% 
  full_join(PublishersGreenNoGrants, by = "Var1") %>%
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
         BronzePercent = (Bronze / All) * 100,
         BronzePercent = paste0(round(BronzePercent, 2), "%"),
         GreenPercent = (Green / All) * 100,
         GreenPercent = paste0(round(GreenPercent, 2), "%"),
         DiamondPercent = (Diamond / All) * 100,
         DiamondPercent = paste0(round(DiamondPercent, 2), "%"))

PublishersNoGrantsPercent <- PublishersNoGrants %>% select(-one_of('Hybrid', 'Gold', 'Bronze', 'Green', 'Diamond', 'Closed', 'All'))

PublishersNoGrantsPercent <- PublishersNoGrantsPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Brone = BronzePercent, Green = GreenPercent, Diamond = DiamondPercent, Closed = ClosedPercent)


write.csv(PublishersNoGrantsPercent, "DataOutput/PublishersNoGrantsPercent.csv")

PublishersNoGrants <- head(PublishersNoGrants, 10)

# Melt the dataframe to long format
long_PublishersNoGrants <- PublishersNoGrants %>%
  select(Publisher, Gold, Hybrid, Bronze, Green, Diamond, Closed) %>%
  pivot_longer(cols = c(Gold, Hybrid, Bronze, Green, Diamond, Closed), 
               names_to = "Type", 
               values_to = "Value")

custom_colors <- c("Closed" = "#36638E", "Hybrid" = "#8B8E82", "Gold" = "#057BE7", "Bronze" = "#E95CCA", "Green" = "#B2C4DB", "Closed" = "#A97F86")


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





####Extra stuff####
Publishers <- Publishers %>%
  mutate(GoldPercent = (Gold / All) * 100,
         GoldPercent = paste0(round(GoldPercent, 2), "%"))

PublishersGrants <- subset(PublishersGrants, AllPaidOA > 9)

PublishersGrants <- PublishersGrants[ , c('Var1', 'GoldGrant', 'GoldNonGrant', 'HybridGrant', 'HybridNonGrant')]

# Melt the dataframe to long format
long_PublishersGrants <- PublishersGrants %>%
  select(Var1, GoldGrant, GoldNonGrant, HybridGrant, HybridNonGrant) %>%
  pivot_longer(cols = c(GoldGrant, GoldNonGrant, HybridGrant, HybridNonGrant), 
               names_to = "Type", 
               values_to = "Value")

custom_colors <- c("Gold" = "#5B8FF9", "Hybrid" = "#5AD8A6", "NonPaidOA" = "#5D7092")


# Create the stacked bar chart
png(filename = "Visuals/PublishedGrantsArticles.png", 
    width = 6160, height = 4000, res = 600)
ggplot(long_PublishersGrants, aes(x = Var1, y = Value, fill = Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal()
dev.off()


#Select just author and institution related information
Inst_Works_201823_Authors <- Inst_Works_201823_Authors %>% select(one_of('id', 'au_id', 'au_display_name', 'author_position', 'is_corresponding', 'institution_id', 'institution_display_name', 'institution_ror', 'institution_country_code', 'institution_type'))

#Select just rows with AAU comps as author
#AAUComps_Works_2018_23 <- subset(Inst_Works_201823_Authors, institution_ror == "https://ror.org/03nawhv43"|institution_ror == "https://ror.org/03s65by71"|institution_ror == "https://ror.org/03r0ha626"|institution_ror == "https://ror.org/03efmqc40")

table(AAUComps_Works_2018_23$institution_display_name)
table(Inst_Works_2018_23$institution_display_name)

#Count how many total works by fractional authorship
AAUComps_Works_2018_23 %>%
  group_by(institution_display_name) %>%
  summarise(sum_numeric_A = sum(fract_author))
Inst_Works_2018_23 %>%
  group_by(institution_display_name) %>%
  summarise(sum_numeric_A = sum(fract_author))
