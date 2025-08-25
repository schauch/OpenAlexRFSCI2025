# hello

# To download and interact with the data, we need certain instructions that don't come by default with R
# So we have to first install two packages. The first, Tidyverse, is very well known and widely used. It offers
# a lot of functionality that will be useful in just about anything you do in R.
# The second, openalexR, is specific to OpenAlex and allows us to download data through the OpenAlex API
# and then transform that data from a JSON format into something we can begin to manipulate in a more traditional
# spreadsheet format.
# Run the two lines of code below this comment section. You should only need to do this once, although periodically rerunning them
# is good practice as updates will be made to the packages. It can be especially important for the 
# openalexr package as the data in OpenAlex can change in major ways that mean old code no longer works.
# If you're getting error messages when pulling data from the API, try reinstalling openalexR.

install.packages("openalexR")
install.packages("tidyverse")
install.packages("janitor")

# We also need to create some directory subfolders that we'll save our work to. 
# Again, you just need to run the next two lines of code once.

dir.create("DataOutput")
dir.create("Visuals")

# Once you install the packages, you then need to call them to get them to actually work. You can do so by running
# the next two lines of code. You'll need to do this each time you reopen RStudio.

library(openalexR)
library(tidyverse)
library(janitor)

# Opens up the profile so you can tell OpenAlex you're a trusted party and it will handle your requests faster
# Again, just run this line of code once.
file.edit("~/.Rprofile")


#### Pull and combine all works by people affiliated with a specific institution ####
# This uses the openalexr package to pull data from the OAX API and turn it into what's known in R as a dataframe. 
Inst_Works <- oa_fetch(    # This line of code should not change. 
  entity = "works",    # Type of record you want - replace "works" with another type if you want (leave the quotation marks). See the list of OAX entities for options: https://docs.openalex.org/api-entities/entities-overview
  authorships.institutions.ror = c("01keh0577"),   # Replace the ID in the quotation marks with the ROR ID you want (see https://ror.org/). If you want multiple, separate with a comma after the closing quotation mark.
  from_publication_date = "2024-06-01", # If want to pull other years, just change the date range in this and the next lines of code.
  to_publication_date = "2024-07-31",
  mailto = oa_email(),  # Do not change this or the rest of the lines
  per_page = 25,
  verbose = TRUE
)
# The more data you try to pull from the API, the more problems can pop up, so it can help to break your pull into chunks
# such as by using smaller date ranges.
# After pulling one batch, save as an RDS file (see next), then change dates and pull again as needed
# Make sure to save after each pull or change the name for the dataframe (InstWorks) so you don't erase/lose a prior pull

# The dataframes are not flat spreadsheets, so we can't save them as a CSV file yet. We have to first save them as
# a special R file format called RDS. It's not great for the long term because it will only work with R, 
# but it's fine until we get them flat.
# The below code will create a saved file in the DataOutput subfolder of your project. Change the file name to anything you wish 
# but make sure it begins with DataOutput/ and ends in .rds.

write_rds(Inst_Works, "DataOutput/Inst_Works_2024_Summer_FSCI.rds")
write_rds(Inst_Works, "DataOutput/Inst_Works_2024_Fall_FSCI.rds")

# Now read the file back in. If you had to pull data in chunks, read in each file with a different name
# and then follow the next step.
Inst_Works_2024_Summer_FSCI <- read_rds("DataOutput/Inst_Works_2024_Summer_FSCI.rds")
Inst_Works_2024_Fall_FSCI <- read_rds("DataOutput/Inst_Works_2024_Fall_FSCI.rds")


# If pulled you pulled the data in by multiple batches, use this to combine (or bind) them into one dataframe
# You can add as many dataframes as you need, just separate with a comma after each one until the last one.
Inst_Works <- bind_rows(Inst_Works_2024_Summer_FSCI, Inst_Works_2024_Fall_FSCI)

# Save this new dataframe so we do not have to repeat the above steps

write_rds(Inst_Works, "DataOutput/Inst_Works.rds")



#### Data cleanup ####
# We need to clean up our data to begin working with it.
# Call in the saved file from above

Inst_Works <- read_rds("DataOutput/Inst_Works.rds")

# First let's get an overview of the data. The command glimpse will return a list of all our variable names 
# along with data type (i.e. <chr>, <list>, or <dbl> (dbl is numeric)), and then the first few values for each variable
# We can also use this to decide which columns are not needed and that we can remove in the next step
glimpse(Inst_Works)

# We'll first delete unneeded columns. If you wish to include any, just delete its name from the list in the next chunk of code
# Remember to also delete the single quotation marks around it and the comma that comes after it as well.
Inst_Works <- Inst_Works %>% select(-one_of('abstract', 'pdf_url', 'first_page', 'last_page', 'volume', 'issue', 'any_repository_has_fulltext', 'cited_by_api_url','ids', 'referenced_works', 'related_works', 'concepts', 'counts_by_year')) 

# It can be good practice to check to see if you have duplicate records
# Run the below chunk to check for duplicates based on OAX's id for the work. 
# If you do not have any duplicates, you should see a 0 returned in the console, which is what we want.
# If so, skip the next chunk of code.
sum(duplicated(Inst_Works$id))

# If you get a number returned that's greater than 0, run the next code chunk to remove all duplicate rows. 
# This will leave just one row for each duplicate.
Inst_Works <- Inst_Works %>%
  distinct(id, .keep_all = TRUE)

# We're going to focus on journal articles, so let's see how many are in our dataset, compared to other work types, by running below code.
tabyl(Inst_Works$type)

# Now create a subset of data that includes only journal articles
Inst_Articles <- filter(Inst_Works, type == "article")

# We're going to analyze articles by publisher, but the display names for publishers in OAX is not always standardized
# Use the below code to bring them all under one standard name. This also combines subsidiaries with their parent publishers
# If you wish to keep the subsidiaries separate, simply delete that entire line from the code.
# If you have additional publisher name changes you wish to make, just copy one line and paste it below and then change the two names to what you want.
# Current name used in OAX data goes to the left of the =, new name goes to the right.
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


# Now we'll display in the console how many articles were published by publisher in descending order
Inst_Articles %>% 
  tabyl(host_organization_name) %>% 
  arrange(desc(n))

# Now let's see how many works are OA and by which work type
Inst_Articles %>% 
  tabyl(oa_status) %>% 
  arrange(desc(n))

# And let's create a pivot table to see OA type by publication year
Inst_Articles %>% 
  tabyl(publication_year, oa_status) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>% 
  adorn_ns()


# As explained above, the current dataset is not in a simple flat spreadsheet. Instead, it has spreadsheets (or dataframes)
# nested in the main dataframe (shown as data type list). We need to start separating these out.
# We'll first unnest the topics variable, which will create one row for each topic assigned to an article
# meaning most if not all articles will now have multiple rows.
# We'll then create a subset that removes the other nested variables
# so we can save it as its own CSV file in the DataOutput folder that we can explore separately
Topics <- unnest(Inst_Articles, topics, names_sep = "_")
Topics <- subset(Topics, select = -c(authorships, keywords, apc, grants))
write.csv(Topics, "DataOutput/Topics.csv")

# Now remove the topics variable from the main dataframe
Inst_Articles <- Inst_Articles %>% select(-one_of('topics')) 

# We're going to do the exact same thing but for the keywords variable this time
Keywords <- unnest(Inst_Articles, keywords, names_sep = "_")
Keywords <- subset(Keywords, select = -c(authorships, apc, grants))
write.csv(Keywords, "DataOutput/Keywords.csv")
Inst_Articles <- Inst_Articles %>% select(-one_of('keywords'))

# It's time to unnest the author variable so we'll have a row for each author for a single work, meaning we'll have multiple rows per work
Articles_Authors <- unnest(Inst_Articles, authorships, names_sep = "_")

# Create column called total_authors that shows the total number of authors per work.
Articles_Authors <- Articles_Authors %>%
  group_by(id) %>%
  mutate(total_authors = n())

# Create another column called fract_authors showing the fractional author count per work 
Articles_Authors <- Articles_Authors %>%
  mutate(fract_author = 1 / total_authors)

# There's actually multiple levels of nesting going on, as the nested dataframe of authorship contains another
# nested variable for authorship affiliation as authors can have multiple affiliations. 
# So we need to unnest this as well. 
Articles_Authors <- unnest(Articles_Authors, authorships_affiliations, names_sep = "_")

# Now let's create a subset that includes only the rows with collaborators as authors, i.e. those NOT associated with your institution 
# To do this, you'll again need to change out the ROR ID - as the full URL - in the below code chunk - 
# to tell it which articles to exclude
# Because some of your institution's authors now likely have multiple rows, or entries, with some that are not for your institution,
# we have to first create a list of authors where any row attached to them includes your institution's ROR ID.
ids_to_remove <- Articles_Authors %>%
  filter(grepl("https://ror.org/01keh0577", authorships_affiliations_ror, ignore.case = TRUE)) %>%  #Replace with ROR ID for your institution
  pull(authorships_id) %>%
  unique()

# Now we can use the list we made to identify and remove all rows containing those authors affiliated with your institution.
Articles_Collabs <- Articles_Authors %>%
  filter(!authorships_id %in% ids_to_remove)

# Now see which institutions are most commonly our collaborators
# If you want to see more or less, just change the 50 in the third line to the number you want printed.
Articles_Collabs %>% 
  tabyl(authorships_affiliations_display_name) %>% 
  arrange(desc(n)) %>% 
  head(50)   #There can be a lot of other institutions, so this limits to the top 50; you can change the number to what you want

# Now do the same but by the collaborators' country
# It will return it by a two-letter code, so you might need to look up what each country the codes correspond to.
Articles_Collabs %>% 
  tabyl(authorships_affiliations_country_code) %>% 
  arrange(desc(n))

# Create a new dataframe to see how many works had at least one collaborating author not affiliated with your institution
CollabDistinctWorks <- Articles_Collabs %>%
  distinct(id, .keep_all = TRUE)

# Save the new dataframe for later analysis, which means removing the remaining nested variables
Articles_Collabs <- Articles_Collabs %>% select(-one_of('grants', 'apc')) 
write.csv(Articles_Collabs, "DataOutput/Articles_Collabs.csv")

# To focus on authors from your institution, create a subset with just rows featuring your authors
# Again, change out the ROR ID URL for the one matching your organization.
Articles_InstAuthors <- subset(Articles_Authors, authorships_affiliations_ror == "https://ror.org/01keh0577")

# Create a new variable called author_totalworks that shows how many works each individual author has in the dataset
Articles_InstAuthors <- Articles_InstAuthors %>%
  group_by(authorships_id) %>%
  mutate(author_totalworks = n()) %>%
  ungroup()

# Remove the remaining nested variables to save list of all your institution's authors (even duplicates) as a csv file
Articles_InstAuthors_CSV <- Articles_InstAuthors %>% select(-one_of('grants', 'apc')) 
write.csv(Articles_InstAuthors_CSV, "DataOutput/Articles_InstAuthors_CSV.csv")

# Making sure we don't have duplicates because someone has multiple current affiliations
# Should only see one value - your institution's
unique(Articles_InstAuthors$authorships_affiliations_display_name)

# Now create a subset dataframe that gets rid of repeats in authors - i.e, see how many actual people published in this time
# Note: This can be imperfect if OAX has multiple records for an author.
Articles_InstDistinctAuthors <- Articles_InstAuthors %>%
  distinct(authorships_id, .keep_all = TRUE)

# Get a feel for how many works each author was publishing by seeing how many had 1 article, 2, etc.
tabyl(Articles_InstDistinctAuthors$author_totalworks)

# Remove remaining nested variables so can then save as csv file.
Articles_InstDistinctAuthors <- Articles_InstDistinctAuthors %>% select(-one_of('grants','apc')) 
write.csv(Articles_InstDistinctAuthors, "DataOutput/Articles_InstDistinctAuthors.csv")

# Now create a subset of articles where your institution's author has served as corresponding author
Articles_InstCorresponding <- subset(Articles_InstAuthors, authorships_is_corresponding == TRUE)

# Because some articles can have multiple corresponding authors, we need to
# deduplicate so that each article has just one row
Articles_InstCorresponding <- Articles_InstCorresponding %>%
  distinct(id, .keep_all = TRUE)


# Now see how many are OA and what status when it's your institution's author as corresponding authors
# You can compare this to the OA status for all articles in this dataset, no matter who was the corresponding author (see line 123)
tabyl(Articles_InstCorresponding$oa_status)
  

# Unnest the grants list variable so you can see which articles did and did not have a grant
Articles_InstCorresponding <- unnest(Articles_InstCorresponding, grants)

# Unnesting grants is messy, so this cleans up to just the OAX grant funder ID and any NAs
Articles_InstCorresponding <- Articles_InstCorresponding[grepl("^https:", Articles_InstCorresponding$grants) | is.na(Articles_InstCorresponding$grants), ]

# Deduplicating again because of articles with multiple grants
sum(duplicated(Articles_InstCorresponding$id))
Articles_InstCorresponding <- Articles_InstCorresponding %>%
  distinct(id, .keep_all = TRUE)

# Create new variable to say whether it has a grant at all or not.
Articles_InstCorresponding <- Articles_InstCorresponding %>%
  mutate(has_grant = ifelse(is.na(grants), "N", "Y"))

# We now have our final flat spreadsheet that we can save as a CSV file. 
Articles_InstCorresponding <- Articles_InstCorresponding %>% select(-one_of('apc')) 
write.csv(Articles_InstCorresponding, "DataOutput/Articles_InstCorresponding.csv")
