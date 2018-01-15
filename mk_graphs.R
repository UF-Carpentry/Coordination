library(tidyverse)

participants <- read.csv("data/MergedParticipants.csv", stringsAsFactors = FALSE)
corrected_participants <- participants %>%
  rename(rawAffiliation = Affiliation) %>%
  mutate(Affiliation = ifelse(rawAffiliation == "Student", "Graduate", rawAffiliation))


affiliation_graph <- function(count_data, title="Participants by Affiliation"){
  # top to bottom ordering of affiliations to draw
  affiliations <- c("Undergraduate", "Graduate", "Post-Doc", "Staff", "Faculty", "Other")
  
  # Y location for text
  text_y <- max(count_data$n) * 0.66
  
  # count everyone up
  everyone <- count_data %>% tally()
  
  # count up any other affiliations...
  others <- count_data %>%
    filter(! Affiliation %in% affiliations) %>%
    tally()

  # and replace those other affiliation rows with an "Other" total
  plot_data <- count_data %>%
    filter(Affiliation %in% affiliations) %>%
    union(data.frame(Affiliation="Other", n=others$nn))

  # set bar order by re-assigning the factor levels, note reversing  
  plot_data$Affiliation = factor(plot_data$Affiliation, levels=rev(affiliations))
    
  p <- ggplot(data=plot_data, aes(x=Affiliation, y=n, fill=Affiliation)) +
    geom_bar(stat="identity") +
    coord_flip() +
    guides(fill=FALSE) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    labs(y = "Number of Participants", x = "UF Affiliation", title=title) + 
    ggplot2::annotate("text", y=text_y, x=1.5, label=paste0("Total Participants: ", everyone$nn))
    # tm library has an "annotate" too
  return(p)
}

# All participants graph
affiliation_counts <- corrected_participants %>%
  group_by(Affiliation) %>%
  tally()
p <- affiliation_graph(affiliation_counts)
ggsave("graphs/participants_all.png", plot=p, height=3, width=4)
#print(p)

# Most recent full year
year <- 2017
affiliation_counts <- corrected_participants %>%
  filter(Year == year) %>%
  group_by(Affiliation) %>%
  tally()
p <- affiliation_graph(affiliation_counts, paste0("Participants by Affiliation in ", year))
ggsave(paste0("graphs/participants_", year, ".png"), plot=p, height=3, width=4)


# Wordcloud!
library(tm)
library(wordcloud)
depts <- Corpus(VectorSource(corrected_participants$Department))


# remove junk words
depts <- tm_map(depts, stripWhitespace)
depts <- tm_map(depts, content_transformer(tolower))
depts <- tm_map(depts, removeWords, stopwords("english"))
depts <- tm_map(depts, removeWords, c("and", "department", "florida", "lab")) 

dtm <- TermDocumentMatrix(depts)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(8)
png(paste0("graphs/wordcloud_", "all", ".png"), height=450, width=600)
wordcloud(words=d$word, freq=d$freq, 
          random.order=TRUE, max.words=50, colors=brewer.pal(8, "Dark2"))
dev.off()


# Cumulative graph
participants_over_time <- corrected_participants %>%
  mutate(Date = as.Date(paste(Year, Month, Day, sep='-'))) %>%
  group_by(Event, Carpentry, Date) %>%
  tally() %>%
  arrange(Date) %>%
  mutate(number = row_number())

year_starts <- as.Date(paste(unique(corrected_participants$Year), 1, 1, sep='-'))

cumulative_graph <- function(cumulative_data, y, title){
  p <- ggplot(cumulative_data, aes(x=Date, y=cumsum(y))) + 
    geom_step(direction="hv", colour="dodgerblue", size=1) + 
    scale_x_date(breaks=cumulative_data$Date,
                 labels=paste0(cumulative_data$Carpentry, "\n", cumulative_data$Date),
                 limits=c(as.Date("2016-03-15"), NA)) +
    scale_y_continuous(limits=c(0, NA)) + 
    labs(y = "Cumulative Number", x = "Workshops", title=title) + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.text.x= element_text(angle=35, hjust = 1))
  
  for (x in year_starts){
    p <- p + geom_vline(aes(xintercept=x), colour="indianred", linetype="dashed", size=0.5)
  }
  
  return(p)
}

# workshops
p <- cumulative_graph(participants_over_time, participants_over_time$number, "Cumulative Workshops Over Time")
#print(p)
ggsave(paste0("graphs/cumulative_wks.png"), plot=p, height=3, width=4)

# participants
p <- cumulative_graph(participants_over_time, participants_over_time$n, "Cumulative Participants Over Time")
#print(p)
ggsave(paste0("graphs/cumulative_part.png"), plot=p, height=3, width=4)


