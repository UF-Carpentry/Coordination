#
# Script for choosing candidates for the travel awards.
#

# Settings
cutoff_date <- '2017-08-12' # Should be two years ago, in yyyy-mm-dd format.

# We use tidyverse extensively.
library(tidyverse)

# Download the 'instructors' file from the Google Drive as a CSV.
inst_df <- read.csv("data/instructors.csv", header=TRUE)

# Lowercase e-mail address to avoid case-related issues.
inst_df <- inst_df %>% mutate(email_lc = trimws(tolower(Email)))

print("Have data for these events:")
inst_df %>%
  distinct(event, date) %>%
  arrange(event, date)

print("Of this, we will consider activity for the following events:")
since_cutoff <- inst_df %>% filter(as.POSIXct(date, format="%Y-%m-%d") > as.POSIXct(cutoff_date, format="%Y-%m-%d"))
since_cutoff %>% distinct(event, date) %>% arrange(event, date)  

print("Crosstab of how many times people have done what in 2 years")
whos_done_what <- since_cutoff %>%
  group_by(Name, email_lc, role, event) %>%
  arrange(Name, role)
whos_done_what %>% print(n=1000)
whos_done_what %>% write_csv('data/whos_done_what.csv')

print("Who has contributed (in any role) more than twice since cutoff_date")
selected = whos_done_what %>%
  group_by(Name, email_lc) %>%
  count() %>%
  filter(n>=2)
selected %>% print(n=1000)
selected %>% write_csv('data/travel_award_candidates.csv')
