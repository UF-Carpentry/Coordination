library(tidyverse)
library(stringi) # stri_pad_both()
library(scales) # pretty_breaks()

participants <- read.csv("data/MergedParticipants.csv", stringsAsFactors = FALSE)
# Coding for students has varied over time, merge together some older codes and assume
# that all "Student"s were graduate.
corrected_participants <- participants %>%
  rename(rawAffiliation = Affiliation) %>%
  mutate(Affiliation = ifelse(rawAffiliation == "Student" | rawAffiliation == "PhD"  | rawAffiliation == "Masters",
                              "Graduate", rawAffiliation))

# Subtitle for all graphs so we know when they were generated
as_of = paste0("As of ", Sys.Date())

affiliation_graph <- function(count_data, title="Participants by Affiliation"){
  # top to bottom ordering of affiliations to draw
  affiliations <- c("Undergraduate", "Graduate", "Post-Doc", "Staff", "Faculty", "Outside", "Unknown")
  
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
    union(data.frame(Affiliation="Unknown", n=others$nn))

  # set bar order by re-assigning the factor levels, note reversing  
  plot_data$Affiliation = factor(plot_data$Affiliation, levels=rev(affiliations))
    
  p <- ggplot(data=plot_data, aes(x=Affiliation, y=n, fill=Affiliation)) +
    geom_bar(stat="identity") +
    coord_flip() +
    guides(fill=FALSE) +
    scale_y_continuous(limits=c(0, NA), breaks=pretty_breaks()) +
    theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + 
    labs(y = "Number of Participants", x = "UF Affiliation", title=title,
         subtitle=as_of) + 
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
depts <- tm_map(depts, removeWords, c("and", "department", "florida", "lab", "university", "college", "state", "school")) 

dtm <- TermDocumentMatrix(depts)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(7)
png(paste0("graphs/wordcloud_", "all", ".png"), height=500, width=675)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, cex=2, "Wordcloud of Participant Departments")
text(x=0.5, y=0.1, cex=1.6, as_of)
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

year_starts <- data.frame(intercept=
                            as.Date(paste(unique(corrected_participants$Year), 1, 1, sep='-'))
)

cumulative_graph <- function(cumulative_data, y, title){
  y_data = cumsum(y)
  
  p <- ggplot(cumulative_data, aes(x=Date, y=y_data)) + 
    geom_step(direction="hv", colour="dodgerblue", size=1) + 
    scale_x_date(breaks=cumulative_data$Date,
                 labels=paste0(stri_pad_both(cumulative_data$Carpentry, width=10),
                               "\n", cumulative_data$Date),
                 limits=c(as.Date("2016-01-20"), NA)) +
    scale_y_continuous(limits=c(0, NA), breaks=pretty_breaks()) + 
    labs(y = "Cumulative Number", x = "Workshops", title=title, subtitle=as_of) + 
    theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) +
    theme(axis.text.x=element_text(size=5, angle=90, hjust=0.5, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=5))
  
  p <- p + geom_vline(data=year_starts, aes(xintercept=intercept), colour="indianred", linetype="dashed", size=0.5)

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


