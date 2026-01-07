# This file should be used after you have imported and cleaned up data using the RCode_OpenAlexR script. 
# This will have you analyze the nested variables that were removed from the main dataframe in the prior script.
# These include Topics, Keywords, Sustainability, Funders, and Awards

# First reload the packages at the start of a new R session
library(tidyverse)
library(janitor)
library(here)

### Topics ###

# We'll first work on analyzing the Topics variable by reading in the file we saved
Topics <- read_csv(here("DataOutput/Topics.csv"))

Topics <- Topics %>% 
  filter(topics_i == 1)

# OAX has four levels of topics, with domain being the broadest, then field, then subfield, and then topic as the narrowest
# We'll create four subsets of the Topics dataframe to focus on each one

Topics_domain <- Topics %>% 
  filter(topics_type == "domain")

# View a table showing how many times each domain has been used on an article  
Topics_domain %>% 
  tabyl(topics_display_name) %>% 
  arrange(desc(n))

# Now we'll separate out by the individual domains before going down to subfields and topics

Physical_IDs <- Topics %>% 
  filter(grepl("Physical Sciences", topics_display_name, ignore.case = TRUE)) %>% 
  pull(id) %>% 
  unique()

Physical_Sub <- Topics %>% 
  filter(id  %in% Physical_IDs) %>% 
  filter(topics_type == "field")

Physical_Sub %>% 
  tabyl(topics_display_name) %>% 
  arrange(desc(n))

Social_IDs <- Topics %>% 
  filter(grepl("Social Sciences", topics_display_name, ignore.case = TRUE)) %>% 
  pull(id) %>% 
  unique()

Social_Sub <- Topics %>% 
  filter(id  %in% Social_IDs) %>% 
  filter(topics_type == "field")

Social_Sub %>% 
  tabyl(topics_display_name) %>% 
  arrange(desc(n))

Health_IDs <- Topics %>% 
  filter(grepl("Health Sciences", topics_display_name, ignore.case = TRUE)) %>% 
  pull(id) %>% 
  unique()

Health_Sub <- Topics %>% 
  filter(id  %in% Health_IDs) %>% 
  filter(topics_type == "field")

Health_Sub %>% 
  tabyl(topics_display_name) %>% 
  arrange(desc(n))

Life_IDs <- Topics %>% 
  filter(grepl("Life Sciences", topics_display_name, ignore.case = TRUE)) %>% 
  pull(id) %>% 
  unique()

Life_Sub <- Topics %>% 
  filter(id  %in% Life_IDs) %>% 
  filter(topics_type == "field")

Life_Sub %>% 
  tabyl(topics_display_name) %>% 
  arrange(desc(n))



### Keywords ###

Keywords <- read_csv(here("DataOutput/Keywords.csv"))

# See a summary of all the keywords in our dataset and which were the most common
Keywords %>% 
  tabyl(keywords_display_name) %>% 
  arrange(desc(n))

# There will likely be a lot too - too many to really go through
# And many of these keywords will likely have low scores - i.e., OAX's AI did not think there was a strong connection
# So we can remove rows where the score is less than a certain number. For now, well go with less than 50%, or .5
Keywords <- Keywords %>% 
  filter(keywords_score > .5)

# If you want to further analyze articles that have a specific keyword, use the below code to create a subset
# Just change out the KeywordHere with whichever keyword you want to analyze.
# If you want to subset based on multiple keywords, you can use the or operator, | , in between
# So filter(keywords_display_name == "Keyword1" | keywords_display_name == "Keyword2")
Keywords_Specific <- Keywords %>% 
  filter(keywords_display_name == "KeywordHere")



### Sustainable Development Goals ###
Sustainable <- read_csv(here("DataOutput/Sustainable.csv"))

# See what are the top sustainability goals identified with articles in our dataframe
Sustainable %>% 
  tabyl(sustainable_development_goals_display_name) %>% 
  arrange(desc(n))

# If you want to further analyze articles related to a specific goal, use the code below to create a subset
# Replace GoalHere with the specific goal you want to investigate
Sustainable_Specific <- Sustainable %>% 
  filter(sustainable_development_goals_display_name == "GoalHere")



### Funders ###
Funders <- read_csv(here("DataOutput/Funders.csv"))

# Analyze to see who the top funders were
# Note that subgroups could be separate from the parent organization 
# - i.e. National Institute of General Medical Sciences has its own line separate from the National Institutes of Health
Funders %>% 
  tabyl(funders_display_name) %>% 
  arrange(desc(n))

# If you want to view all child organizations with a parent organization, you can use the recode command
# But first you need to determine which are the child organizations. 
# You can browse through the tabyl results we just printed to identify them
# Or you can try searching for the parent organization in OpenAlex
# OAX does not appear to currently display child organizations in the record of a parent organization when you search it as a Funder
# However, if you search it as an Institution - https://openalex.org/institutions?page=1 - 
# the record for the organizaiton should display the names of all children organizations as known by OAX
# You can also click on the API button and do a search for "child" to see these listed more neatly
# Once you have a list of the names, then you can use the below code to create a new column that will provide the parent funding group
# Just replace ChildName1Here and ChildName2Here with the official names used by OAX (copy and paste works best)
# And then ParentNameHere with the official name of the parent organization you want to use - just make sure it's always the same!
# You can also add more lines as needed - just make sure to have a comma at the end of each line except for the last line.

# For example, if I wanted to combine child departments of the United States NIH, I could use:
# Funders <- Funders %>%
#   mutate(funders_parent_name = recode(funders_display_name, 
#                                    "National Institute on Drug Abuse" = "National Institutes of Health", 
#                                    "United States National Library of Medicine" = "National Institutes of Health"))

Funders <- Funders %>% 
  mutate(funders_parent_name = recode(funders_display_name,
                                      "ChildName1Here" = "ParentNameHere",
                                      "ChildName2Here" = "ParentNameHere"))

#Now you can analyze based on the new parent column we made
Funders %>% 
  tabyl(funders_parent_name) %>% 
  arrange(desc(n))


### Awards ###

Awards <- read_csv(here("DataOutput/Awards.csv"))

#Need to clean this up.
# Unnest the grants list variable so you can see which articles did and did not have a grant
Articles_InstCorresponding <- unnest(Articles_InstCorresponding, funders, names_sep = "_", keep_empty = TRUE)

# Unnesting grants is messy, so this cleans up to just the OAX grant funder ID and any NAs
Articles_InstCorresponding <- Articles_InstCorresponding[grepl("^https:", Articles_InstCorresponding$grants) | is.na(Articles_InstCorresponding$grants), ]

# Deduplicating again because of articles with multiple grants
sum(duplicated(Articles_InstCorresponding$id))
Articles_InstCorresponding <- Articles_InstCorresponding %>%
  distinct(id, .keep_all = TRUE)
