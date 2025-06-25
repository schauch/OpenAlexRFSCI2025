#This code file will walk you through doing some specific analysis on the OA output of authors from your institution
#You will need to have first worked your way through the code file labeled XXX.

#Read in your first file to analyze. You made this in the previous code file.
Inst_Articles_InstCorresponding <- read.csv("DataOutput/Inst_Articles_InstCorresponding.csv")

#See where your institution's authors published in total as corresponding authors
PublishersAll <- table(Inst_Articles_InstCorresponding$host_organization_name)
PublishersAll <- sort(PublishersAll, decreasing = TRUE)
PublishersAll <- as.data.frame(PublishersAll)



###Create subset to see which authors have paid for hybrid OA
Inst_Articles_InstCorresponding_Hybrid <- subset(Inst_Articles_InstCorresponding, oa_status == "hybrid")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersHybrid <- table(Inst_Articles_InstCorresponding_Hybrid$host_organization_name)
PublishersHybrid <- sort(PublishersHybrid, decreasing = TRUE)
PublishersHybrid <- as.data.frame(PublishersHybrid)


####See which authors have paid for gold OA
Inst_Articles_InstCorresponding_Gold <- subset(Inst_Articles_InstCorresponding, oa_status == "gold")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersGold <- table(Inst_Articles_InstCorresponding_Gold$host_organization_name)
PublishersGold <- sort(PublishersGold, decreasing = TRUE)
PublishersGold <- as.data.frame(PublishersGold)


###See which ones were closed with a your institution's corresponding author
Inst_Articles_InstCorresponding_Closed <- subset(Inst_Articles_InstCorresponding, oa_status == "closed")

PublishersClosed <- table(Inst_Articles_InstCorresponding_Closed$host_organization_name)
PublishersClosed <- sort(PublishersClosed, decreasing = TRUE)
PublishersClosed <- as.data.frame(PublishersClosed)

###Bronze OA articles
Inst_Articles_InstCorresponding_Bronze <- subset(Inst_Articles_InstCorresponding, oa_status == "bronze")

PublishersBronze <- table(Inst_Articles_InstCorresponding_Bronze$host_organization_name)
PublishersBronze <- sort(PublishersBronze, decreasing = TRUE)
PublishersBronze <- as.data.frame(PublishersBronze)

###Green OA
Inst_Articles_InstCorresponding_Green <- subset(Inst_Articles_InstCorresponding, oa_status == "green")

PublishersGreen <- table(Inst_Articles_InstCorresponding_Green$host_organization_name)
PublishersGreen <- sort(PublishersGreen, decreasing = TRUE)
PublishersGreen <- as.data.frame(PublishersGreen)


###Diamond OA
Inst_Articles_InstCorresponding_Diamond <- subset(Inst_Articles_InstCorresponding, oa_status == "diamond")

PublishersDiamond <- table(Inst_Articles_InstCorresponding_Diamond$host_organization_name)
PublishersDiamond <- sort(PublishersDiamond, decreasing = TRUE)
PublishersDiamond <- as.data.frame(PublishersDiamond)




#####Visuals####
#Combine all the publisher data into one
PublishersAll <- PublishersAll %>% rename(All = Freq)
PublishersGold <- PublishersGold %>% rename(Gold = Freq)
PublishersHybrid <- PublishersHybrid %>% rename(Hybrid = Freq)
PublishersClosed <- PublishersClosed %>% rename(Closed = Freq)
PublishersGreen <- PublishersGreen %>% rename(Green = Freq)
#PublishersBronze <- PublishersBronze %>% rename(Bronze = Freq)
PublishersDiamond <- PublishersDiamond %>% rename(Diamond = Freq)


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

custom_colors <- c("Closed" = "#36638E", "Hybrid" = "#8B8E82", "Gold" = "#057BE7", "Diamond" = "#E95CCA", "Green" = "#B2C4DB")


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
