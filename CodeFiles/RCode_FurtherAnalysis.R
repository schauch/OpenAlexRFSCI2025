# This file should be used after you have imported and cleaned up data using the RCode_OpenAlexR script. 
# This will have you analyze the nested variables that were removed from the main dataframe in the prior script.
# These include Topics, Keywords, Sustainibility, Funders, and Awards

# First reload the packages at the start of a new R session
library(tidyverse)
library(janitor)
library(here)

### Topics ###

# We'll first work on analyzing the Topics variable by reading in the file we saved
Topics <- read_csv(here("DataOutput/Topics.csv"))

# OAX has four levels of topics, with domain being the broadest, then field, then subfield, and then topic as the narrowest
# We'll create four subsets of the Topics dataframe to focus on each one

Topics_domain <- Topics %>% 
  filter(topics_type == "domain")

# View a table showing how many times each domain has been used on an article  
Topics_domain %>% 
  tabyl(topics_display_name) %>% 
  arrange(desc(n))

# Now we'll separate out by the individual domains before going down to subfields and topics

Topics_Physical <- Topics %>% 
  filter(topics_display_name == "Physical Sciences") %>% 
  filter(topics_type)

Topics_Physical %>% 
  tabyl(topics_display_name)