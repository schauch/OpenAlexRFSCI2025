#hello

#To download and interact with the data, we need certain instructions that don't come by default with R
#So we have to first install two packages. The first, Tidyverse, is very well known and widely used. It offers
#a lot of functionality that will be useful in just about anything you do in R.
#The second, openalexR, is specific to OpenAlex and allows us to download data through the OpenAlex API
#and then transform that data from a JSON format into something we can begin to manipulate in a more traditional
#spreadsheet format.
#Run the two lines of code below this comment section. You should only need to do this once, although periodically rerunning them
#is good practice as updates will be made to the packages. It can be especially important for the 
#openalexr package as the data in OpenAlex can change in major ways that mean old code no longer works.
#If you're getting error messages when pulling data from the API, try reinstalling openalexR.

install.packages("openalexR")
install.packages("tidyverse")

#Opens up the profile so I can tell OpenAlex I'm a trusted party with contact info so they'll do my stuff faster
#file.edit("~/.Rprofile")

#Once you install the packages, you then need to call them to get them to actually work. You can do so by running
#the next two lines of code. You'll need to do this each time you reopen RStudio.

library(openalexR)
library(tidyverse)




####Pull and combine all works by people affiliated with a specific institution####
#This uses the openalexr package to pull data from the OAX API and turn it into what's called in R as a dataframe. 
#The first line should not change. 
#If you want to pull another format type - say, you want a list of authors from an institution - change the 
#"works" in the second line to the type you want. See the list of OAX entities for options: https://docs.openalex.org/api-entities/entities-overview
#The third line, authorships.institutions.ror, specifies what institution you want by using the ROR ID
#Use the ROR website, https://ror.org/, to find the ROR ID for the institution(s) you're interested in
#and then replace the 01keh0577 in the quotation marks with the ID for your institution(s)
#If want to pull other years, just change the date range in the fourth and fifth lines of the code.
Inst_Works <- oa_fetch(
  entity = "works",
  authorships.institutions.ror = c("01keh0577"),
  from_publication_date = "2024-08-01",
  to_publication_date = "2024-10-31",
  mailto = oa_email(),
  per_page = 25,
  verbose = TRUE
)
#The more data you try to pull from the API, the more problems can pop up, so it can help to break your pull into chunks
#Doing this by smaller date ranges can help.
#After pulling one batch, save as an RDS file (see next), then change dates and pull again as needed
#Make sure to save after each pull or change the name for the dataframe so you don't erase/lose a prior pull

#The dataframes are not flat spreadsheets, so we can't save them as a CSV file yet. We have to first save them as
#a special R file format called RDS. It's not great for the long term because it will only work with R, 
#but it's fine until we get them flat
#The below code will create saved file in the DataOutput subfolder of your project. Change the file name to anything you wish 
#but make sure it begins with DataOutput/ and ends in .rds.
write_rds(Inst_Works, "DataOutput/Inst_Works_2024_Summer_FSCI.rds")
write_rds(Inst_Works, "DataOutput/Inst_Works_2024_Fall_FSCI.rds")

#Now read the file back in. If you had to pull data in chunks, read in each file with a different name
#and then follow the next step.
Inst_Works_2024_Summer_FSCI <- read_rds("DataOutput/Inst_Works_2024_Summer_FSCI.rds")
Inst_Works_2024_Fall_FSCI <- read_rds("DataOutput/Inst_Works_2024_Fall_FSCI.rds")


#If pulled in by multiple batches, use this to bring them into one dataframe
Inst_Works <- bind_rows(Inst_Works_2024_Summer_FSCI, Inst_Works_2024_Fall_FSCI)



####Data cleanup####
#We need to clean up our data to beging working with it

#We'll first delete unneeded columns. If you wish to include any, just dete it's name from the list in the next chunk of code
#Remember to also delete the single quotation marks around it and the comma that comes after it as well.
Inst_Works <- Inst_Works %>% select(-one_of('abstract', 'pdf_url', 'first_page', 'last_page', 'volume', 'issue', 'any_repository_has_fulltext', 'cited_by_api_url','ids', 'referenced_works', 'related_works', 'concepts', 'counts_by_year')) 

#It can be good practice to check to see if you have duplicate records
#Run the below chunk. If you do not have any duplictes, you should see a 0 in returned in the console.
#If so, skip the next chunk of code.
sum(duplicated(Inst_Works$id))

#If you get a number returned that's greater than 0, run the next code chunk to remove all duplicate rows. 
#This will leave one row for each duplicate.
Inst_Works <- Inst_Works %>%
  distinct(id, .keep_all = TRUE)

#See how many works there are in your dataset by work type (i.e. journal article, book, etc.)
table(Inst_Works$type)

#Now create a subset of data that includes only journal articles
Inst_Articles <- subset(Inst_Works, type == "article")

#We're going to analyze articles by publisher, but the display names for publishers in OAX is not always standardized
#So use the below code to bring them all under one standard name. This also combines subsidiaries with their parent publishers
#If you wish to keep the subsidiaries separate, simply delete that entire line from the code.
Inst_Articles <- Inst_Articles %>%
  mutate(host_organization_name = recode(host_organization_name, 
                              "Wiley-Blackwell" = "Wiley", 
                              "Springer Science+Business Media" = "Springer",
                              "Routledge" = "Taylor & Francis",
                              "Nature Portfolio" = "Springer",
                              "RELX Group (Netherlands)" = "Elsevier",
                              "Elsevier BV" = "Elsevier",
                              "BioMed Central" = "Springer",
                              "Springer Nature" = "Springer",
                              "John Wiley & Sons Ltd" = "Wiley",
                              "Palgrave Macmillan" = "Palgrave",
                              "Springer International Publishing" = "Springer",
                              "WileyOpen" = "Wiley",
                              "Multidisciplinary Digital Publishing Institute" = "MDPI"))


#Now we'll display in the console how many articles were published by publisher in descending order
InstPublishers <- table(Inst_Articles$host_organization_name)
InstPublishers <- sort(InstPublishers, decreasing = TRUE)
InstPublishers

#As explained above, the current dataset is not in a simple flat spreadsheet. Instead, it has spreadsheets (or dataframes)
#nested in the main dataframe. We need to start separating these out.
#We'll first unnest the topics variable, which will create one row for each topic assigned to an article
#meaning most if not all articles will now have multiple rows.
#We'll then create a subset that removes the other nested variables
#so we can save it as its own CSV file in the DataOutput folder that we can explore separately
Inst_Articles_Topics <- unnest(Inst_Articles, topics, names_sep = "_")
Inst_Articles_Topics <- subset(Inst_Articles_Topics, select = -c(authorships, keywords, apc, grants))
write.csv(Inst_Articles_Topics, "DataOutput/Inst_Articles_Topics.csv")

#Honestly not sure what I was doing here
Inst_Articles_Topics <- Inst_Articles_Topics %>%
  mutate(largest_topics_i = max(topics_i, na.rm = TRUE))

table(Inst_Articles_Topics$largest_topics_i)


#Now remove the topics variable from the main dataframe
Inst_Articles <- Inst_Articles %>% select(-one_of('topics')) 

#We're going to do the exact same thing but for the keywords variable this time
Inst_Articles_Keywords <- unnest(Inst_Articles, keywords, names_sep = "_")
Inst_Articles_Keywords <- subset(Inst_Articles_Keywords, select = -c(authorships, apc, grants))
write.csv(Inst_Articles_Keywords, "DataOutput/Inst_Articles_Keywords.csv")
Inst_Articles <- Inst_Articles %>% select(-one_of('keywords'))

#This will let us see how many works are OA and by which work type
table(Inst_Articles$oa_status)

#See OA type by publication year
pivot_table <- Inst_Articles %>%
  group_by(publication_year, oa_status) %>%
  summarise(count = n()) %>%
  spread(key = oa_status, value = count, fill = 0)
# Display the pivot table
print(pivot_table)

#Same idea, but shows percentages
pivot_table <- Inst_Articles %>%
  group_by(publication_year, oa_status) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  select(-count) %>%
  spread(key = oa_status, value = percentage, fill = 0)
# Display the pivot table
print(pivot_table)

#Unnest author variable so that now we'll have a each author for a single work, meaning we'll have multiple rows per work
Inst_Articles_Authors <- unnest(Inst_Articles, authorships, names_sep = "_")

#Create column called total_authors showing the total number of authors per work.
Inst_Articles_Authors <- Inst_Articles_Authors %>%
  group_by(id) %>%
  mutate(total_authors = n())

#Create another column called fract_authors showing the fractional author count per work 
Inst_Articles_Authors <- Inst_Articles_Authors %>%
  mutate(fract_author = 1 / total_authors)

#There's actually multiple levels of nesting going on, as the nested dataframe of authorship contains another
#nested variable for authorship affiliation as authors can have multiple affiliations. 
#So we need to unnest this as well. 
Inst_Articles_Authors <- unnest(Inst_Articles_Authors, authorships_affiliations, names_sep = "_")

#Now let's create a subset that includes only the rows with collaborators as authors, i.e. those NOT associated with your institution 
#To do this, you'll again need to change out the ROR ID - as the full URL - in the below code chunk - 
#to tell it which articles to exclude
#Because some of your institution's authors now likely have multiple rows, or entries, with some that are not for your institution
#We have to first create a list of authors where any row attached to them includes your institution's ROR ID.
ids_to_remove <- Inst_Articles_Authors %>%
  filter(grepl("https://ror.org/01keh0577", authorships_affiliations_ror, ignore.case = TRUE)) %>%
  pull(authorships_id) %>%
  unique()

#Now we can use the list we made to identify and remove all rows containing those authors affiliated with your institution.
Inst_Articles_Collabs <- Inst_Articles_Authors %>%
  filter(!authorships_id %in% ids_to_remove)

#Now see which institutions are most commonly our collaborators
#There's a lot so it's limited to the top 50 institutions with the most collaborating authors
#If you want to see more or less, just change the 50 in the third line to the number you want printed.
InstCollabInst <- table(Inst_Articles_Collabs$authorships_affiliations_display_name)
InstCollabInst <- sort(InstCollabInst, decreasing = TRUE)
InstCollabInst <- head(InstCollabInst, 50)
InstCollabInst

#Now do the Same but by the collaborators' country
#It will return it by a two-letter code, so you might need to look up what each country the codes correspond to.
InstCollabCountry <- table(Inst_Articles_Collabs$authorships_affiliations_country_code)
InstCollabCountry <- sort(InstCollabCountry, decreasing = TRUE)
InstCollabCountry

#How many works had at least one collaborating author not affiliating with your institution
sum(duplicated(Inst_Articles_Collabs$id))

#Now that we've analyzed collaborating authors, let's create 
#a subset that removes all duplicate rows for each work so we have only one row per work
Inst_Articles_CollabDistinctWorks <- Inst_Articles_Collabs %>%
  distinct(id, .keep_all = TRUE)

#Removing grants variable so can then save as csv file
Inst_Articles_CollabAuthors <- Inst_Articles_CollabAuthors %>% select(-one_of('grants')) 
write.csv(Inst_Articles_CollabAuthors, "DataOutput/Inst_Articles_CollabAuthors.csv")

#Subest just rows with Inst person as author
Inst_Articles_InstAuthors <- subset(Inst_Articles_Authors, institution_ror == "https://ror.org/01keh0577")
#UNR averaged 3 authors per article

#Create new variable that shows how many works each individual your institution author had
Inst_Articles_InstAuthors <- Inst_Articles_InstAuthors %>%
  group_by(au_id) %>%
  mutate(au_totalworks = n()) %>%
  ungroup()

#checking to make sure got it right. https://openalex.org/A5000257708
#Inst_Articles_InstAuthors_Test <- subset(Inst_Articles_InstAuthors, au_id == "https://openalex.org/A5000257708")
#Interesting - accidentally tested it first on wrong DF, Inst_Articles_Author
#Was supposed to be 8, but when did the first test, got 12
#Which means there's four times this author's record was not affiliated with your institution. 
#Checked his LinkedIn - he left and went to UNLV. Cool.
#Otherwise it worked.

#Remove grants so can save list of all your institution's authors (even duplicates) as csv file
Inst_Articles_InstAuthors_CSV <- Inst_Articles_InstAuthors %>% select(-one_of('grants')) 
write.csv(Inst_Articles_InstAuthors_CSV, "DataOutput/Inst_Articles_InstAuthors_CSV.csv")

#Making sure we don't have duplicates because someone has multiple current affiliations
table(Inst_Articles_InstAuthors$institution_display_name)
#We do not - OAX must choose; should look to see how they do this.

#Now create subset that gets rid of repeats in authors - i.e, see how many actual people published in this time

Inst_Articles_InstDistinctAuthors <- Inst_Articles_InstAuthors %>%
  distinct(au_id, .keep_all = TRUE)

#Slightly more complicated way of removing duplicates but seems to have worked
Inst_Articles_InstDistinctAuthors <- Inst_Articles_InstAuthors %>%
  group_by(au_id) %>%
  slice(1) %>%
  ungroup()
#We had 4,609 people serve as an author from 2020 through 2024
#Looks like people averaged 1.84 articles then? Is that right?

#Get a feel for how many works each author was publishing
table(Inst_Articles_InstDistinctAuthors$au_totalworks)

#Remove grant so can then save as csv
Inst_Articles_InstDistinctAuthors <- Inst_Articles_InstDistinctAuthors %>% select(-one_of('grants')) 
write.csv(Inst_Articles_InstDistinctAuthors, "DataOutput/Inst_Articles_InstDistinctAuthors.csv")

#Just to ensure the deduplication worked
#sum(duplicated(Inst_Articles_InstDistinctAuthors$au_id))

#Subset to articles where your institution's author has served as corresponding author
Inst_Articles_InstCorresponding <- subset(Inst_Articles_InstAuthors, is_corresponding == TRUE)
#Served as corresponding author on 34.4% of the articles

#Checking to see if have duplicates. 
sum(duplicated(Inst_Articles_InstCorresponding$id))

Inst_Articles_InstCorresponding_Dupes <- Inst_Articles_InstCorresponding %>% 
  group_by(id) %>%
  filter(n() > 1) %>%
  ungroup()
#We do, 241. Would assume would not have dupes, but for whatever reason this grabbed multiple for some

#Deduplicating so each article is repped just once
sum(duplicated(Inst_Articles_InstCorresponding$id))
Inst_Articles_InstCorresponding <- Inst_Articles_InstCorresponding %>%
  distinct(id, .keep_all = TRUE)

#So now see how many are OA and what status when it's your institution's author as corresponding
table(Inst_Articles_InstCorresponding$oa_status)
  #For 2020-2024, closed = 1,617;, gold = 1,093; bronze = 511; green = 499; hybrid = 314; Diamond = 78
#Hybrid probably overstated as OAX doesn't count gold OA journals if not in DOAJ.

#Unnest the grants list variable so I can see which articles did and did not have a grant
Inst_Articles_InstCorresponding <- unnest(Inst_Articles_InstCorresponding, grants)

#Unnesting is messy, so this cleans up to just the OAX grant funder ID and any NAs
Inst_Articles_InstCorresponding <- Inst_Articles_InstCorresponding[grepl("^https:", Inst_Articles_InstCorresponding$grants) | is.na(Inst_Articles_InstCorresponding$grants), ]

#Deduplicating again because of articles with multiple grants
sum(duplicated(Inst_Articles_InstCorresponding$id))
Inst_Articles_InstCorresponding <- Inst_Articles_InstCorresponding %>%
  distinct(id, .keep_all = TRUE)

#Create new variable to say whether it has a grant at all or not.
Inst_Articles_InstCorresponding <- Inst_Articles_InstCorresponding %>%
  mutate(has_grant = ifelse(is.na(grants), "N", "Y"))

#Create csv file of deduplicated articles with a your institution's corresponding article, topics removed and grants unnested
write.csv(Inst_Articles_InstCorresponding, "DataOutput/Inst_Articles_InstCorresponding.csv")


####Digging into OA satus of articles with your institution's as corresponding author####
Inst_Articles_InstCorresponding <- read.csv("DataOutput/Inst_Articles_InstCorresponding.csv")

#See where your institution's authors published in total as corresponding authors
PublishersAll <- table(Inst_Articles_InstCorresponding$host_organization)
PublishersAll <- sort(PublishersAll, decreasing = TRUE)
PublishersAll <- as.data.frame(PublishersAll)
#Elsevier = 686; Wiley = 379; T&F = 310; SpringerNature = 284; MDPI = 237; Sage = 211; ACS = 159; Oxford = 128; CSHL = 88; 
#Frontiers = 74; PLOS = 68; Cambridge = 56; Royal Soc of Chem = 58 

#Subset of all articles with your institution as corresponding and no grant listed
Inst_Articles_InstCorresponding_NoGrants <- subset(Inst_Articles_InstCorresponding, has_grant == "N")

#See which were the most common publishers for non-grant funded OA articles with your institution's corresponding author
PublishersAllNoGrants <- table(Inst_Articles_InstCorresponding_NoGrants$host_organization)
PublishersAllNoGrants <- sort(PublishersAllNoGrants, decreasing = TRUE)
PublishersAllNoGrants
PublishersAllNoGrants <- as.data.frame(PublishersAllNoGrants)

#Subset of all articles with your institution as corresponding and grant listed
Inst_Articles_InstCorresponding_Grants <- subset(Inst_Articles_InstCorresponding, has_grant == "Y")

#See which were the most common publishers for grant funded articles with your institution's corresponding author
PublishersAllGrants <- table(Inst_Articles_InstCorresponding_Grants$host_organization)
PublishersAllGrants <- sort(PublishersAllGrants, decreasing = TRUE)
PublishersAllGrants
PublishersAllGrants <- as.data.frame(PublishersAllGrants)

###Create subset to see which authors have paid for hybrid OA
#No longer need the is_corresponding, but leaving in because it shouldn't matter
Inst_Articles_InstCorresponding_Hybrid <- subset(Inst_Articles_InstCorresponding, oa_status == "hybrid")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersHybrid <- table(Inst_Articles_InstCorresponding_Hybrid$host_organization)
PublishersHybrid <- sort(PublishersHybrid, decreasing = TRUE)
PublishersHybrid
PublishersHybrid <- as.data.frame(PublishersHybrid)
#Bit messy - some duplication with different versions of publisher names.
#But looks like Elsevier = 55; Wiley = 12; Cambridge = 7 (which probably was covered by our TA); American Society of Civ Eng = 2
#Oxford Uni Press = 5; Springer = 9; all others were 1 article

#No longer need the is_corresponding, but leaving in because it shouldn't matter
Inst_Articles_InstCorresponding_Hybrid_NoGrants <- subset(Inst_Articles_InstCorresponding_Hybrid, has_grant == "N")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersHybridNoGrants <- table(Inst_Articles_InstCorresponding_Hybrid_NoGrants$host_organization)
PublishersHybridNoGrants <- sort(PublishersHybridNoGrants, decreasing = TRUE)
PublishersHybridNoGrants
PublishersHybridNoGrants <- as.data.frame(PublishersHybridNoGrants)

#See which hybrid OA articles with a your institution's corresponding author had a grant
Inst_Articles_InstCorresponding_Hybrid_Grants <- subset(Inst_Articles_InstCorresponding, has_grant == "Y" & oa_status == "hybrid")

PublishersHybridGrants <- table(Inst_Articles_InstCorresponding_Hybrid_Grants$host_organization)
PublishersHybridGrants <- sort(PublishersHybridGrants, decreasing = TRUE)
PublishersHybridGrants
PublishersHybridGrants <- as.data.frame(PublishersHybridGrants)

####See which authors have paid for gold OA
Inst_Articles_InstCorresponding_Gold <- subset(Inst_Articles_InstCorresponding, oa_status == "gold")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersGold <- table(Inst_Articles_InstCorresponding_Gold$host_organization)
PublishersGold <- sort(PublishersGold, decreasing = TRUE)
PublishersGold
PublishersGold <- as.data.frame(PublishersGold)

#Subset to gold OA non-grant funded articles with your institution as corresponding articles
Inst_Articles_InstCorresponding_Gold_NoGrants <- subset(Inst_Articles_InstCorresponding_Gold, has_grant == "N")

#Combine and export lists of gold & hybrid OA aritcles with your institution corresponding so I can manually check for grants
Inst_Articles_InstCorresponding_HybridGold_NoGrants <- bind_rows(Inst_Articles_InstCorresponding_Hybrid_NoGrants, Inst_Articles_InstCorresponding_Gold_NoGrants)

write.csv(Inst_Articles_InstCorresponding_HybridGold_NoGrants, "DataOUtput/Inst_Articles_InstCorresponding_HybridGold_NoGrants.csv" )


#See which were the most common publishers for non-grant funded hybrid OA articles with your institution corresponding author
PublishersGoldNoGrants <- table(Inst_Articles_InstCorresponding_Gold_NoGrants$host_organization)
PublishersGoldNoGrants <- sort(PublishersGoldNoGrants, decreasing = TRUE)
PublishersGoldNoGrants
PublishersGoldNoGrants <- as.data.frame(PublishersGoldNoGrants)

#See which gold OA grant-funded articles with a your institution's corresponding author 
Inst_Articles_InstCorresponding_Gold_Grants <- subset(Inst_Articles_InstCorresponding_Gold, has_grant == "Y")

#See which were the most common publishers for non-grant funded hybrid OA articles with your institution's corresponding author
PublishersGoldGrants <- table(Inst_Articles_InstCorresponding_Gold_Grants$host_organization)
PublishersGoldGrants <- sort(PublishersGoldGrants, decreasing = TRUE)
PublishersGoldGrants
PublishersGoldGrants <- as.data.frame(PublishersGoldGrants)

###See which ones were closed with a your institution's corresponding author
Inst_Articles_InstCorresponding_Closed <- subset(Inst_Articles_InstCorresponding, oa_status == "closed")

PublishersClosed <- table(Inst_Articles_InstCorresponding_Closed$host_organization)
PublishersClosed <- sort(PublishersClosed, decreasing = TRUE)
PublishersClosed
PublishersClosed <- as.data.frame(PublishersClosed)

#Subset closed articles with your institution's corresponding author with no grant funding
Inst_Articles_InstCorresponding_Closed_NoGrants <- subset(Inst_Articles_InstCorresponding, has_grant == "N" & oa_status == "closed")

#See which were most common publishers for all closed articles with Inst corresponding author
PublishersClosedNoGrants <- table(Inst_Articles_InstCorresponding_Closed_NoGrants$host_organization)
PublishersClosedNoGrants <- sort(PublishersClosedNoGrants, decreasing = TRUE)
PublishersClosedNoGrants
PublishersClosedNoGrants <- as.data.frame(PublishersClosedNoGrants)

#See which closed articles with a your institution's corresponding author had a grant
Inst_Articles_InstCorresponding_Closed_Grants <- subset(Inst_Articles_InstCorresponding, has_grant == "Y" & oa_status == "closed")

#See which were the most common publishers for non-grant funded closed articles with your institution's corresponding author
PublishersClosedGrants <- table(Inst_Articles_InstCorresponding_Closed_Grants$host_organization)
PublishersClosedGrants <- sort(PublishersClosedGrants, decreasing = TRUE)
PublishersClosedGrants
PublishersClosedGrants <- as.data.frame(PublishersClosedGrants)

###Bronze OA articles
Inst_Articles_InstCorresponding_Bronze <- subset(Inst_Articles_InstCorresponding, oa_status == "bronze")

PublishersBronze <- table(Inst_Articles_InstCorresponding_Bronze$host_organization)
PublishersBronze <- sort(PublishersBronze, decreasing = TRUE)
PublishersBronze
PublishersBronze <- as.data.frame(PublishersBronze)

#Bronze OA with your institution's corresopnding author with no grant funding
Inst_Articles_InstCorresponding_Bronze_NoGrants <- subset(Inst_Articles_InstCorresponding_Bronze, has_grant == "N")

PublishersBronzeNoGrants <- table(Inst_Articles_InstCorresponding_Bronze_NoGrants$host_organization)
PublishersBronzeNoGrants <- sort(PublishersBronzeNoGrants, decreasing = TRUE)
PublishersBronzeNoGrants
PublishersBronzeNoGrants <- as.data.frame(PublishersBronzeNoGrants)

#Bronze OA with your institution's corresopnding author with grant funding
Inst_Articles_InstCorresponding_Bronze_Grants <- subset(Inst_Articles_InstCorresponding_Bronze, has_grant == "Y")

PublishersBronzeGrants <- table(Inst_Articles_InstCorresponding_Bronze_Grants$host_organization)
PublishersBronzeGrants <- sort(PublishersBronzeGrants, decreasing = TRUE)
PublishersBronzeGrants
PublishersBronzeGrants <- as.data.frame(PublishersBronzeGrants)

###Green OA
Inst_Articles_InstCorresponding_Green <- subset(Inst_Articles_InstCorresponding, oa_status == "green")

PublishersGreen <- table(Inst_Articles_InstCorresponding_Green$host_organization)
PublishersGreen <- sort(PublishersGreen, decreasing = TRUE)
PublishersGreen
PublishersGreen <- as.data.frame(PublishersGreen)

#Green OA articles with your institution's corresponding author with no grant funding
Inst_Articles_InstCorresponding_Green_NoGrants <- subset(Inst_Articles_InstCorresponding_Green, has_grant == "N")

PublishersGreenNoGrants <- table(Inst_Articles_InstCorresponding_Green_NoGrants$host_organization)
PublishersGreenNoGrants <- sort(PublishersGreenNoGrants, decreasing = TRUE)
PublishersGreenNoGrants
PublishersGreenNoGrants <- as.data.frame(PublishersGreenNoGrants)

#Green OA articles with your institution's corresponding author with grant funding
Inst_Articles_InstCorresponding_Green_Grants <- subset(Inst_Articles_InstCorresponding_Green, has_grant == "Y")

PublishersGreenGrants <- table(Inst_Articles_InstCorresponding_Green_Grants$host_organization)
PublishersGreenGrants <- sort(PublishersGreenGrants, decreasing = TRUE)
PublishersGreenGrants
PublishersGreenGrants <- as.data.frame(PublishersGreenGrants)


###Diamond OA
Inst_Articles_InstCorresponding_Diamond <- subset(Inst_Articles_InstCorresponding, oa_status == "diamond")

PublishersDiamond <- table(Inst_Articles_InstCorresponding_Diamond$host_organization)
PublishersDiamond <- sort(PublishersDiamond, decreasing = TRUE)
PublishersDiamond
PublishersDiamond <- as.data.frame(PublishersDiamond)

#Diamond OA articles with your institution's corresponding author with no grant funding
Inst_Articles_InstCorresponding_Diamond_NoGrants <- subset(Inst_Articles_InstCorresponding_Diamond, has_grant == "N")

PublishersDiamondNoGrants <- table(Inst_Articles_InstCorresponding_Diamond_NoGrants$host_organization)
PublishersDiamondNoGrants <- sort(PublishersDiamondNoGrants, decreasing = TRUE)
PublishersDiamondNoGrants
PublishersDiamondNoGrants <- as.data.frame(PublishersDiamondNoGrants)

#Diamond OA articles with your institution's corresponding author with grant funding
Inst_Articles_InstCorresponding_Diamond_Grants <- subset(Inst_Articles_InstCorresponding_Diamond, has_grant == "Y")

PublishersDiamondGrants <- table(Inst_Articles_InstCorresponding_Diamond_Grants$host_organization)
PublishersDiamondGrants <- sort(PublishersDiamondGrants, decreasing = TRUE)
PublishersDiamondGrants
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
  full_join(PublishersBronze, by = "Var1") %>% 
  full_join(PublishersGreen, by = "Var1") %>%
  full_join(PublishersDiamond, by = "Var1") %>%
  full_join(PublishersClosed, by = "Var1")

Publishers <- Publishers %>% replace_na(list(All = 0, Gold = 0, Hybrid = 0, Bronze = 0, Green = 0, Diamond = 0, Closed = 0))

Publishers <- Publishers %>%
  rename(Publisher = Var1)


write.csv(Publishers, "DataOUtput/Publishers.csv")

#Publishers <- Publishers %>% 
#  mutate(TotalPaidOA = (Gold + Hybrid))

Publishers <- Publishers %>%
  mutate(GoldPercent = (Gold / All) * 100,
         GoldPercent = paste0(round(GoldPercent, 2), "%"))

Publishers <- Publishers %>%
  mutate(HybridPercent = (Hybrid / All) * 100,
         HybridPercent = paste0(round(HybridPercent, 2), "%"))

Publishers <- Publishers %>%
  mutate(BronzePercent = (Bronze / All) * 100,
         BronzePercent = paste0(round(BronzePercent, 2), "%"))

Publishers <- Publishers %>%
  mutate(GreenPercent = (Green / All) * 100,
         GreenPercent = paste0(round(GreenPercent, 2), "%"))

Publishers <- Publishers %>%
  mutate(DiamondPercent = (Diamond / All) * 100,
         DiamondPercent = paste0(round(DiamondPercent, 2), "%"))

Publishers <- Publishers %>%
  mutate(ClosedPercent = (Closed / All) * 100,
         ClosedPercent = paste0(round(ClosedPercent, 2), "%"))

# Publishers <- Publishers %>% 
#  mutate(TotalPaidOAPercent = (TotalPaidOA / All) * 100,
#         TotalPaidOAPercent = paste0(round(TotalPaidOAPercent, 2), "%"))

PublishersPercent <- Publishers %>% select(-one_of('Hybrid', 'Gold', 'Bronze', 'Green', 'Diamond', 'Closed', 'All'))

PublishersPercent <- PublishersPercent %>%
  rename(Gold = GoldPercent, Hybrid = HybridPercent, Brone = BronzePercent, Green = GreenPercent, Diamond = DiamondPercent, Closed = ClosedPercent)

write.csv(PublishersPercent, "DataOUtput/PublishersPercent.csv")


#Publishers <- Publishers %>% 
#  mutate(NonPaidOA = All - Gold - Hybrid)

Publishers <- head(Publishers, 10)

# Melt the dataframe to long format
long_Publishers <- Publishers %>%
  select(Publisher, Gold, Hybrid, Bronze, Green, Diamond, Closed) %>%
  pivot_longer(cols = c(Gold, Hybrid, Bronze, Green, Diamond, Closed), 
               names_to = "Type", 
               values_to = "Value")

custom_colors <- c("Closed" = "#36638E", "Hybrid" = "#8B8E82", "Gold" = "#057BE7", "Bronze" = "#E95CCA", "Green" = "#B2C4DB", "Closed" = "#A97F86")


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
