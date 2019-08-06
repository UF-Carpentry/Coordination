# Settings
cutoff_date <- '2017-05-08' # Should be two years ago, in yyyy-mm-dd format.

# Set up packages
library(tidyverse)

# Download the 'instructors' file from the Google Drive as a CSV.
inst_df <- read.csv("data/instructors.csv", header=TRUE)

print("Have data for these events:")
inst_df %>%
  distinct(event, date) %>%
  arrange(event, date)

print("Crosstab of how many times people have done what in 2 years")
inst_df %>%
  filter(as.POSIXct(date, format="%Y-%m-%d") > as.POSIXct(cutoff_date, format="%Y-%m-%d")) %>%
  group_by(Name, Email, role, date) %>%
  count() %>%
  arrange(Name, role) %>%
  print(n=1000)

print("Who has contributed (in any role) more than twice since cutoff_date")
inst_df %>%
  filter(as.POSIXct(date, format="%Y-%m-%d") > as.POSIXct(cutoff_date, format="%Y-%m-%d")) %>%
  group_by(Name, Email) %>%
  count() %>%
  filter(n>=2) %>% 
  arrange(Name, Email) %>%
  print(n=1000)
