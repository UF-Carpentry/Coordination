library(tidyverse)
inst_df <- read.csv("../workshop-data/instructors.csv", header=TRUE)


print("Have data for these events:")
inst_df %>%
  distinct(event) %>%
  arrange(event)

cutoff_date <- '2016-08-01'
print("Crosstab of how many times people have done what in 2 years")
inst_df %>%
  mutate(date=substring(event, 1, 10)) %>%
  filter(date > cutoff_date) %>%
  group_by(first_name, last_name, email, role) %>%
  count() %>%
  arrange(first_name, last_name, role) %>%
  print(n=1000)
 

print("People's emails")
inst_df %>%
  group_by(first_name, last_name, email) %>%
  count() %>%
  arrange(first_name, last_name) %>%
  print(n=1000)


