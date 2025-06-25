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
#This uses the openalexr package to pull data from the OAX API and turn it into what's known in R as a dataframe. 
#The first line of code should not change. 
#If you want to pull another format type - say, you want a list of authors from an institution - change the 
#"works" in the second line to the type you want. See the list of OAX entities for options: https://docs.openalex.org/api-entities/entities-overview
#The third line, authorships.institutions.ror, specifies what institution you want by using the institution's ROR ID.
#Use the ROR website, https://ror.org/, to find the ROR ID for the institution(s) you're interested in
#and then replace the 01keh0577 in the quotation marks with the ID for your institution(s) - make sure to keep the quotation marks, though!
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
#such as by using smaller date ranges.
#After pulling one batch, save as an RDS file (see next), then change dates and pull again as needed
#Make sure to save after each pull or change the name for the dataframe so you don't erase/lose a prior pull

#The dataframes are not flat spreadsheets, so we can't save them as a CSV file yet. We have to first save them as
#a special R file format called RDS. It's not great for the long term because it will only work with R, 
#but it's fine until we get them flat
#The below code will create a saved file in the DataOutput subfolder of your project. Change the file name to anything you wish 
#but make sure it begins with DataOutput/ and ends in .rds.
write_rds(Inst_Works, "DataOutput/Inst_Works_2024_Summer_FSCI.rds")
write_rds(Inst_Works, "DataOutput/Inst_Works_2024_Fall_FSCI.rds")

#Now read the file back in. If you had to pull data in chunks, read in each file with a different name
#and then follow the next step.
Inst_Works_2024_Summer_FSCI <- read_rds("DataOutput/Inst_Works_2024_Summer_FSCI.rds")
Inst_Works_2024_Fall_FSCI <- read_rds("DataOutput/Inst_Works_2024_Fall_FSCI.rds")


#If pulled you pulled the data in by multiple batches, use this to combine (or bind) them into one dataframe
Inst_Works <- bind_rows(Inst_Works_2024_Summer_FSCI, Inst_Works_2024_Fall_FSCI)



####Data cleanup####
#We need to clean up our data to begin working with it.

#We'll first delete unneeded columns. If you wish to include any, just delete its name from the list in the next chunk of code
#Remember to also delete the single quotation marks around it and the comma that comes after it as well.
Inst_Works <- Inst_Works %>% select(-one_of('abstract', 'pdf_url', 'first_page', 'last_page', 'volume', 'issue', 'any_repository_has_fulltext', 'cited_by_api_url','ids', 'referenced_works', 'related_works', 'concepts', 'counts_by_year')) 

#It can be good practice to check to see if you have duplicate records
#Run the below chunk to check for duplicates based on OAX's id for the work. 
#If you do not have any duplicates, you should see a 0 returned in the console, which is what we want.
#If so, skip the next chunk of code.
sum(duplicated(Inst_Works$id))

#If you get a number returned that's greater than 0, run the next code chunk to remove all duplicate rows. 
#This will leave just one row for each duplicate.
Inst_Works <- Inst_Works %>%
  distinct(id, .keep_all = TRUE)

#We're going to focus on journal articles, so let's see how many are in our dataset, compared to other work types, by running below code.
table(Inst_Works$type)

#Now create a subset of data that includes only journal articles
Inst_Articles <- subset(Inst_Works, type == "article")

#We're going to analyze articles by publisher, but the display names for publishers in OAX is not always standardized
#Use the below code to bring them all under one standard name. This also combines subsidiaries with their parent publishers
#If you wish to keep the subsidiaries separate, simply delete that entire line from the code.
#If you have additional publisher name changes you wish to make, just copy one line and paste it below and then change the two names to what you want.
#Current name used in OAX data goes to the left of the =, new name goes to the right.
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

#Now let's see how many works are OA and by which work type
table(Inst_Articles$oa_status)

#And let's create a pivot table to see OA type by publication year
pivot_table <- Inst_Articles %>%
  group_by(publication_year, oa_status) %>%
  summarise(count = n()) %>%
  spread(key = oa_status, value = count, fill = 0)
# Display the pivot table (we only have one year, so it's a short table)
print(pivot_table)

#Let's do the same thing, but show it in percentages
pivot_table <- Inst_Articles %>%
  group_by(publication_year, oa_status) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  select(-count) %>%
  spread(key = oa_status, value = percentage, fill = 0)
# Display the pivot table
print(pivot_table)

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

#It's time to unnest the author variable so we'll have a row for each author for a single work, meaning we'll have multiple rows per work
Inst_Articles_Authors <- unnest(Inst_Articles, authorships, names_sep = "_")

#Create column called total_authors that shows the total number of authors per work.
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

#Now do the same but by the collaborators' country
#It will return it by a two-letter code, so you might need to look up what each country the codes correspond to.
InstCollabCountry <- table(Inst_Articles_Collabs$authorships_affiliations_country_code)
InstCollabCountry <- sort(InstCollabCountry, decreasing = TRUE)
InstCollabCountry

#Create a new dataframe to see how many works had at least one collaborating author not affiliated with your institution
Inst_Articles_CollabDistinctWorks <- Inst_Articles_Collabs %>%
  distinct(id, .keep_all = TRUE)

#Save the new dataframe for later analysis, which means removing the remaining nested variables
Inst_Articles_Collabs <- Inst_Articles_Collabs %>% select(-one_of('grants', 'apc')) 
write.csv(Inst_Articles_Collabs, "DataOutput/Inst_Articles_Collabs.csv")

#To focus on authors from your institution, create a subset with just rows featuring your authors
#Again, change out the ROR ID URL for the one matching your organization.
Inst_Articles_InstAuthors <- subset(Inst_Articles_Authors, authorships_affiliations_ror == "https://ror.org/01keh0577")

#Create a new variable called author_totalworks that shows how many works each individual author has in the dataset
Inst_Articles_InstAuthors <- Inst_Articles_InstAuthors %>%
  group_by(authorships_id) %>%
  mutate(author_totalworks = n()) %>%
  ungroup()

#Remove the remaining nested variables to save list of all your institution's authors (even duplicates) as a csv file
Inst_Articles_InstAuthors_CSV <- Inst_Articles_InstAuthors %>% select(-one_of('grants', 'apc')) 
write.csv(Inst_Articles_InstAuthors_CSV, "DataOutput/Inst_Articles_InstAuthors_CSV.csv")

#Making sure we don't have duplicates because someone has multiple current affiliations
#Should only see one value - your institution's
table(Inst_Articles_InstAuthors$authorships_affiliations_display_name)

#Now create a subset dataframe that gets rid of repeats in authors - i.e, see how many actual people published in this time
#Note: This can be imperfect if OAX has multiple records for an author.
Inst_Articles_InstDistinctAuthors <- Inst_Articles_InstAuthors %>%
  distinct(authorships_id, .keep_all = TRUE)

#Get a feel for how many works each author was publishing by seeing how many had 1 article, 2, etc.
table(Inst_Articles_InstDistinctAuthors$author_totalworks)

#Remove remaining nested variables so can then save as csv file.
Inst_Articles_InstDistinctAuthors <- Inst_Articles_InstDistinctAuthors %>% select(-one_of('grants','apc')) 
write.csv(Inst_Articles_InstDistinctAuthors, "DataOutput/Inst_Articles_InstDistinctAuthors.csv")

#Now create a subset of articles where your institution's author has served as corresponding author
Inst_Articles_InstCorresponding <- subset(Inst_Articles_InstAuthors, authorships_is_corresponding == TRUE)

#Because some articles can have multiple corresponding articles, we need to
#deduplicate so that each article has just one row
Inst_Articles_InstCorresponding <- Inst_Articles_InstCorresponding %>%
  distinct(id, .keep_all = TRUE)

#Now see how many are OA and what status when it's your institution's author as corresponding authors
#You can compare this to the OA status for all articles in this dataset, no matter who was the corresponding author (see line 123)
table(Inst_Articles_InstCorresponding$oa_status)
  

#Unnest the grants list variable so you can see which articles did and did not have a grant
Inst_Articles_InstCorresponding <- unnest(Inst_Articles_InstCorresponding, grants)

#Unnesting grants is messy, so this cleans up to just the OAX grant funder ID and any NAs
Inst_Articles_InstCorresponding <- Inst_Articles_InstCorresponding[grepl("^https:", Inst_Articles_InstCorresponding$grants) | is.na(Inst_Articles_InstCorresponding$grants), ]

#Deduplicating again because of articles with multiple grants
sum(duplicated(Inst_Articles_InstCorresponding$id))
Inst_Articles_InstCorresponding <- Inst_Articles_InstCorresponding %>%
  distinct(id, .keep_all = TRUE)

#Create new variable to say whether it has a grant at all or not.
Inst_Articles_InstCorresponding <- Inst_Articles_InstCorresponding %>%
  mutate(has_grant = ifelse(is.na(grants), "N", "Y"))

#We now have our final flat spreadsheet that we can save as a CSV file. 
Inst_Articles_InstCorresponding <- Inst_Articles_InstCorresponding %>% select(-one_of('apc')) 
write.csv(Inst_Articles_InstCorresponding, "DataOutput/Inst_Articles_InstCorresponding.csv")
