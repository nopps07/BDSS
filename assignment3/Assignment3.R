# Assignment 3
# Gunho Lee, Valentin Duprez, Lennert Vanhaeren

############
# library  #
############

library(rscopus)
library(dplyr)
library(rvest)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(readr)
library(lubridate)
library(plotly)
library(zoo)
library(tidytext)
library(dplyr)
library(textdata)
library(vader)
library(tidytext)
library(syuzhet)


################################
# Data Preparation + Cleaning  #
################################

# the URL of the Wikipedia page (The list is provided over three pages)
url <- "https://en.wikipedia.org/w/index.php?title=Category:Artificial_intelligence_researchers&pageuntil=Krizhevsky%2C+Alex%0AAlex+Krizhevsky#mw-pages"

url2 <- "https://en.wikipedia.org/w/index.php?title=Category:Artificial_intelligence_researchers&pagefrom=Krizhevsky%2C+Alex%0AAlex+Krizhevsky#mw-pages"

url3 <- "https://en.wikipedia.org/w/index.php?title=Category:Artificial_intelligence_researchers&pagefrom=Wolfram%2C+Stephen%0AStephen+Wolfram#mw-pages"

# read the HTML content
page <- read_html(url)
page2 <- read_html(url2)
page3 <- read_html(url3)

# scrape the researcher names
researchers1 <- page %>%
  html_nodes(".mw-category-group li a") %>%
  html_text()

researchers2 <- page2 %>%
  html_nodes(".mw-category-group li a") %>%
  html_text()

researchers3 <- page3 %>%
  html_nodes(".mw-category-group li a") %>%
  html_text()

# Extract only the list of researchers and merge the lists
researchers1 = researchers1[9:207]
researchers2 = researchers2[9:207]
researchers3 = researchers3[9:26]
all_researchers = c(researchers1, researchers2, researchers3)

# Clean names
clean_names <- gsub("\\(.*\\)", "", all_researchers)  # Remove text within brackets
clean_names <- trimws(clean_names)  # Remove leading/trailing white spaces

print(clean_names)

# 1. Open .Renviron
#file.edit("~/.Renviron")
# 2. In the file,  add the following line
#Elsevier_API = "YOUR API KEY"

set.seed(123)
for (i in 1:length(clean_names)) {
  name <- clean_names[i]
  # Extract last name and first name
  name_parts <- strsplit(name, " ")[[1]]
  last <- name_parts[length(name_parts)]
  first <- paste(name_parts[-length(name_parts)], collapse = " ")
  
  if (grepl("\\.", first)) {
    # Handle cases where last name is separated by a space
    split_name <- strsplit(first, "\\. ")[[1]]
    first <- paste(split_name[-length(split_name)], collapse = " ")
    last <- split_name[length(split_name)]
  }
  
  # Iteration
  tryCatch({
    if (have_api_key()) {
      res <- author_df(last_name = last, first_name = first, verbose = FALSE, general = FALSE)
      names(res)
      
      # Extract doi
      doi <- res$doi
      
      # Save the info
      result <- res[, c("title", "journal", "description", "cover_date", "first_name", "last_name")]
      result$doi <- doi
      
      results[[i]] <- result  # Save the result for this author in the list
    }
  }, error = function(e) {
    cat("Error occurred for author:", name, "\n")
  })
}

# Merge all the results into a single data frame
merged_results <- do.call(rbind, results)
merged_results_noNA <- merged_results[complete.cases(merged_results$doi), ]

# Create an empty list to store the abstracts
abstracts <- list()

for (doi in merged_results_noNA$doi) {
  if (!is.null(api_key)) {
    tryCatch({
      # Retrieve the abstract using the DOI
      abstract <- abstract_retrieval(doi, identifier = "doi", view = "FULL", verbose = FALSE)
      
      # Save the abstract in the list
      abstracts[[doi]] <- abstract$content$`abstracts-retrieval-response`$item$bibrecord$head$abstracts
    }, error = function(e) {
      cat("Error occurred for DOI:", doi, "\n")
    })
  }
}

# Merge the individual abstracts into a data frame
merged_abstracts <- data.frame(doi = names(abstracts), abstract = unlist(abstracts))
# Merge the abstracts and results based on DOI
merged_data <- merge(merged_abstracts, merged_results_noNA, by = "doi", all.x = TRUE)

# Select the needed columns
merged_data <- merged_data[, c("doi", "abstract", "title", "journal", "description","cover_date", "first_name", "last_name")]

## Final Filtering 
keywords <- c("artificial intelligence", "AI", "Machine Learning", "ML", "deep learning")
scopus_cleaned <- merged_data[grepl(paste(keywords, collapse = "|"), merged_data$abstract), ]

FILE <- 'data/arxiv-metadata-oai-snapshot.json'

# Read JSON file line by line
data_list <- stream_in(file(FILE))

# Create a dataframe
dataframe <- data.frame(
  authors = sapply(data_list, function(x) x$authors),
  title = sapply(data_list, function(x) x$title),
  update_date = sapply(data_list, function(x) x$update_date),
  abstract = sapply(data_list, function(x) x$abstract),
  stringsAsFactors = FALSE
)

# List of strings to search for in abstracts
strings <- c(' ai ', ' artificial intelligence ', ' machine learning ', ' deep learning ', ' neural network ', ' transformers')

# Convert all abstracts to lowercase
dataframe$abstract <- tolower(dataframe$abstract)

# Keep all the rows where the abstract contains one of the strings
dataframe <- dataframe %>% 
  filter(str_detect(abstract, str_c(strings, collapse = '|')))

# Show the dimensions of the filtered dataframe
print(dim(dataframe))

# Save as csv
write_csv(dataframe, 'data/arxiv.csv')

# Read the arxiv data
arxiv <- read_csv("data/arxiv.csv", col_types = cols(update_date = col_date(format = "%Y-%m-%d")))
head(arxiv)


# Loop through the texts in the abstract column
for(i in 1:nrow(arxiv)){
  abstract <- arxiv$abstract[i]
  # create a text corpus
  corpus <- Corpus(VectorSource(abstract))
  
  # clean the abstracts
  corpus_clean <- corpus %>%
    tm_map(content_transformer(tolower)) %>% # convert to lower case
    tm_map(removePunctuation) %>% # remove punctuation
    tm_map(removeNumbers) %>% # remove numbers
    tm_map(removeWords, stopwords("en")) %>% # remove stopwords
    tm_map(stripWhitespace) # remove extra white spaces
  
  abstract_clean <- as.character(corpus_clean[[1]])
  
  # replace the abstract with the cleaned version
  arxiv$abstract[i] <- abstract_clean
}

write.csv(arxiv, "arxiv_cleaned.csv", row.names = FALSE)

#############################
# Exploratory Data Analysis #
#############################

## Scopus
scopus <- read.csv('data/scopus_cleaned.csv')
scopus <- scopus[, -10]
scopus$cover_date <- as.Date(scopus$cover_date, format = "%Y-%m-%d")
scopus <- scopus[scopus$cover_date >= as.Date("1970-01-01"),]

# Create a new column with the year of the cover date
scopus$year <- format(scopus$cover_date, "%Y")

# Create a histogram of the amount of articles per year
plot_ly(scopus, x = ~year, type = "histogram") %>%
  layout(title = "Amount of Articles per Year", xaxis = list(title = "Year"), yaxis = list(title = "Count"))

# Group the data by author and count the number of articles
author_counts <- scopus %>%
  group_by(last_name, first_name) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  head(30)

# Combine first_name and last_name to a single column for the plot
author_counts <- author_counts %>%
  mutate(author = paste(first_name, last_name)) %>%
  arrange(desc(count))  # Ensure the data frame is sorted by count

# Convert the author column to a factor and specify the levels to match the order in the data frame
author_counts$author <- factor(author_counts$author, levels = author_counts$author)

# Create a barplot of the top 20 authors
plot_ly(author_counts, x = ~author, y = ~count, type = "bar") %>%
  layout(title = "Top 30 Authors by Article Count", xaxis = list(title = "Author"), yaxis = list(title = "Count"))

# Order the data by count
desc_counts <- scopus %>%
  group_by(description) %>%
  summarize(count = n(), .groups = "drop") %>%
  arrange(desc(count))

# Create a factor variable with the ordered descriptions
desc_counts$description <- factor(desc_counts$description, levels = desc_counts$description)

# Create a barplot of the ordered descriptions
plot_ly(desc_counts, x = ~description, y = ~count, type = "bar") %>%
  layout(title = "Description Barplot", xaxis = list(title = "Description"), yaxis = list(title = "Count"))

# create a year column
arxiv$year <- format(arxiv$update_date, "%Y")
# Create a histogram of the amount of articles per year with padding
plot_ly(arxiv, x = ~year, type = "histogram") %>%
  layout(title = "Amount of Articles per Year", xaxis = list(title = "Year", automargin = TRUE), yaxis = list(title = "Count", automargin = TRUE, margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)), bargap = 0.1)


############################
#  3. Sentiment Analysis   #
############################

## 3.1. Visualization

# Add a new column called 'abstract_sen' to the 'scopus' dataframe
scopus$abstract_sen <- NA
# Loop through each row of the dataframe and calculate the sentiment score for the abstract
for (i in 1:nrow(scopus)) {
  sentiment <- get_sentiment(scopus$abstract[i], method="syuzhet")
  scopus$abstract_sen[i] <- sentiment
}

# Calculate the average sentiment score per year
scopus_avg <- aggregate(scopus$abstract_sen, by=list(scopus$date), FUN=mean)
colnames(scopus_avg) <- c("Year", "Avg_Sentiment")

# Create a plotly line chart of the average sentiment score per year
plot_ly(scopus_avg, x = ~Year, y = ~Avg_Sentiment, type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "Average Sentiment Score per Year", xaxis = list(title = "Year"), yaxis = list(title = "Average Sentiment Score"))

# Order the data by cover_date
scopus <- scopus[order(scopus$cover_date),]

# Calculate the rolling average of abstract_sen with a window of 2 months
scopus$rolling_avg <- rollmean(scopus$abstract_sen, k = 60, fill = NA, align = "right")

# Create a plotly line chart of the rolling average sentiment score per cover date
plot_ly(scopus, x = ~cover_date, y = ~rolling_avg, type = 'scatter', mode = 'lines') %>%
  layout(title = "Rolling Average Sentiment Score per Cover Date", xaxis = list(title = "Cover Date"), yaxis = list(title = "Rolling Average Sentiment Score")) 

for(i in 1:nrow(scopus)){
  abstract <- scopus$abstract[i]
  # create a text corpus
  corpus <- Corpus(VectorSource(abstract))
  
  # preprocess text
  corpus_clean <- corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(stripWhitespace)
  
  abstract_clean <- as.character(corpus_clean[[1]])
  
  # replace the abstract with the cleaned version
  scopus$abstract[i] <- abstract_clean
}

scopus$vader_sen <- NA
for (i in 1:nrow(scopus)) {
  vader_sentiment <- get_vader(scopus$abstract[i])[2]
  scopus$vader_sen[i] <- vader_sentiment
}

# Add a new column called 'abstract_sen' to the 'data_test_cleaned' dataframe
scopus$nrc_sen <- NA

# Loop through each row of the dataframe and calculate the sentiment score for the abstract
for (i in 1:nrow(scopus)) {
  nrc_sentiment <- get_sentiment(scopus$abstract[i], method="syuzhet")
  scopus$nrc_sen[i] <- nrc_sentiment
}

scopus$cover_date <- as.Date(scopus$cover_date)

# Define start and end dates
start_date <- as.Date("2021-11-22")
end_date <- as.Date("2023-11-22")

# Filter the data to include only two years of interest
scopus_filtered <- scopus %>%
  filter(cover_date >= start_date & cover_date <= end_date)

# Calculate the average sentiment score per day
scopus_avg_nrc <- aggregate(scopus_filtered$nrc_sen, by=list(scopus_filtered$cover_date), FUN=mean)
colnames(scopus_avg_nrc) <- c("Date", "Avg_Sentiment")

# Calculate the 7-day rolling mean of sentiment score
scopus_avg_nrc$Rolling_Mean <- rollmean(scopus_avg_nrc$Avg_Sentiment, k = 7, fill = NA, align = "right")

# Create a plotly line chart of the average sentiment score per day
p <- plot_ly(scopus_avg_nrc, x = ~Date, y = ~Avg_Sentiment, type = 'scatter', mode = 'lines', name = 'Daily Average_NRC') %>%
  layout(title = "Average Sentiment Score per Day", 
         xaxis = list(title = "Date"), 
         yaxis = list(title = "Average Sentiment Score"))

# Add the 7-day rolling mean to the plot
p <- add_trace(p, x = ~Date, y = ~Rolling_Mean, type = 'scatter', mode = 'lines', name = '7-day Rolling Mean NRC')

# Add a red vertical line at 30 November 2022
marker_date <- as.Date("2022-11-30")
p <- add_segments(p, x = marker_date, xend = marker_date, y = 0, yend = 16, line = list(color = 'red'), name = 'ChatGPT Release')

# Display the plot
p


arxiv <- read_csv("data/arxiv_cleaned.csv", col_types = cols(update_date = col_date(format = "%Y-%m-%d")))

# Add a new column called 'abstract_sen' to the 'data_test_cleaned' dataframe
arxiv$nrc_sen <- NA

# Loop through each row of the dataframe and calculate the sentiment score for the abstract
for (i in 1:nrow(arxiv)) {
  nrc_sentiment <- get_sentiment(arxiv$abstract[i], method="syuzhet")
  arxiv$nrc_sen[i] <- nrc_sentiment
}

# Calculate the average sentiment score per year
arxiv$update_date <- as.Date(arxiv$update_date)
arxiv$year <- year(arxiv$update_date)

arxiv_avg <- aggregate(arxiv$nrc_sen, by=list(arxiv$year), FUN=mean)
colnames(arxiv_avg) <- c("Year", "Avg_Sentiment")

# Create a plotly line chart of the average sentiment score per year
plot_ly(arxiv_avg, x = ~Year, y = ~Avg_Sentiment, type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "Average Sentiment Score per Year", xaxis = list(title = "Year"), yaxis = list(title = "Average Sentiment Score"))

arxiv <- read_csv("data/arxiv_sentiments.csv", col_types = cols(update_date = col_date(format = "%Y-%m-%d")))

# Convert update_date to Date class
arxiv$update_date <- as.Date(arxiv$update_date)

# Define start and end dates
start_date <- as.Date("2021-11-22")
end_date <- as.Date("2023-11-22")

# Filter the data to include only two years of interest
arxiv_filtered <- arxiv %>%
  filter(update_date >= start_date & update_date <= end_date)

# Calculate the average sentiment score per day
arxiv_avg_nrc <- aggregate(arxiv_filtered$nrc_sen, by=list(arxiv_filtered$update_date), FUN=mean)
colnames(arxiv_avg_nrc) <- c("Date", "Avg_Sentiment")

# Calculate the 7-day rolling mean of sentiment score
arxiv_avg_nrc$Rolling_Mean <- rollmean(arxiv_avg_nrc$Avg_Sentiment, k = 14, fill = NA, align = "right")

# Create a plotly line chart of the average sentiment score per day
p <- plot_ly(arxiv_avg_nrc, x = ~Date, y = ~Avg_Sentiment, type = 'scatter', mode = 'lines', name = 'Daily Average_NRC') %>%
  layout(title = "Average Sentiment Score per Day", 
         xaxis = list(title = "Date"), 
         yaxis = list(title = "Average Sentiment Score"))

# Add the 7-day rolling mean to the plot
p <- add_trace(p, x = ~Date, y = ~Rolling_Mean, type = 'scatter', mode = 'lines', name = '14-day Rolling Mean NRC')

# Add a red vertical line at 30 November 2022
marker_date <- as.Date("2022-11-30")
p <- add_segments(p, x = marker_date, xend = marker_date, y = 0, yend = 10, line = list(color = 'red'), name = 'ChatGPT Release')

# Display the plot
p

## 3.2. Deeper Analysis

# Filter the data to include only the article from May 1, 2022
article_may_1 <- arxiv_filtered %>% filter(update_date == as.Date("2022-05-01"))

# Display the article
print(article_may_1$abstract)

# Get sentiment of the abstract
article_may_1$sentiment <- get_sentiment(article_may_1$abstract)
#article_may_1$sentiment <- get_sentiment(article_may_13$abstract) #maybe it's an error?  may_13 => may_1

# Display the sentiment score
print(article_may_1$sentiment)

# Tokenize the abstract into individual words
words <- article_may_1 %>% unnest_tokens(word, abstract)

# Add sentiment scores to each word   
word_sentiment <- words %>%
  inner_join(get_sentiments("nrc"), by = "word")  # join on 'word'

# Display the sentiment of each word
print(word_sentiment)

### 3.2.1. VADER lexicon

arxiv$vader_sen <- NA

for (i in 1:nrow(arxiv)) {
  vader_sentiment <- get_vader(arxiv$abstract[i])[2]
  arxiv$vader_sen[i] <- vader_sentiment
}

write.csv(arxiv, "arxiv_sentiments.csv", row.names = FALSE)

# Calculate the average sentiment score per day
arxiv_avg_vader <- aggregate(arxiv_filtered$vader_sen, by=list(arxiv_filtered$update_date), FUN=mean)
colnames(arxiv_avg_vader) <- c("Date", "Avg_Sentiment")

# Calculate the 7-day rolling mean of sentiment score
arxiv_avg_vader$Rolling_Mean <- rollmean(arxiv_avg_vader$Avg_Sentiment, k = 14, fill = NA, align = "right")

# Create a plotly line chart of the average sentiment score per day
p <- plot_ly(arxiv_avg_vader, x = ~Date, y = ~Avg_Sentiment, type = 'scatter', mode = 'lines', name = 'Daily Average VADER') %>%
  layout(title = "Average Sentiment Score per Day", 
         xaxis = list(title = "Date"), 
         yaxis = list(title = "Average Sentiment Score"))

# Add the 7-day rolling mean to the plot
p <- add_trace(p, x = ~Date, y = ~Rolling_Mean, type = 'scatter', mode = 'lines', name = '14-day Rolling Mean VADER')

# Add a red vertical line at 30 November 2022
marker_date <- as.Date("2022-11-30")
p <- add_segments(p, x = marker_date, xend = marker_date, y = -1, yend = 1, line = list(color = 'red'), name = 'ChatGPT Release')

# Display the plot
p

### 3.2.2. Outlier Removal
# Convert update_date to Date class if it's not
arxiv$update_date <- as.Date(arxiv$update_date)

# Specify the dates you want to remove
dates_to_remove <- as.Date(c("2021-11-26","2021-11-28", "2021-12-27", "2022-05-01", "2022-09-06", "2022-09-25", "2023-05-13"))

# Filter rows to remove specific dates
arxiv_filtered <- arxiv[!(arxiv$update_date %in% dates_to_remove),]

### 3.2.3. NRC lexicon (after outlier removal)
# Convert update_date to Date class
arxiv_filtered$update_date <- as.Date(arxiv_filtered$update_date)

# Define start and end dates
start_date <- as.Date("2021-11-22")
end_date <- as.Date("2023-11-22")

# Filter the data to include only two years of interest
arxiv_filtered <- arxiv_filtered %>%
  filter(update_date >= start_date & update_date <= end_date)

# Calculate the average sentiment score per day
arxiv_avg_nrc <- aggregate(arxiv_filtered$nrc_sen, by=list(arxiv_filtered$update_date), FUN=mean)
colnames(arxiv_avg_nrc) <- c("Date", "Avg_Sentiment")

# Calculate the 7-day rolling mean of sentiment score
arxiv_avg_nrc$Rolling_Mean <- rollmean(arxiv_avg_nrc$Avg_Sentiment, k = 30, fill = NA, align = "right")

# Create a plotly line chart of the average sentiment score per day
p <- plot_ly(arxiv_avg_nrc, x = ~Date, y = ~Avg_Sentiment, type = 'scatter', mode = 'lines', name = 'Daily Average_NRC') %>%
  layout(title = "Average Sentiment Score per Day", 
         xaxis = list(title = "Date"), 
         yaxis = list(title = "Average Sentiment Score"))

# Add the 7-day rolling mean to the plot
p <- add_trace(p, x = ~Date, y = ~Rolling_Mean, type = 'scatter', mode = 'lines', name = '30-day Rolling Mean NRC')

# Add a red vertical line at 30 November 2022
marker_date <- as.Date("2022-11-30")
p <- add_segments(p, x = marker_date, xend = marker_date, y = 0, yend = 10, line = list(color = 'red'), name = 'ChatGPT Release')

# Display the plot
p

### 3.2.4. VADER lexicon (after outlier removal)
# Convert update_date to Date class
arxiv_filtered$update_date <- as.Date(arxiv_filtered$update_date)

# Define start and end dates
start_date <- as.Date("2021-11-22")
end_date <- as.Date("2023-11-22")

# Filter the data to include only two years of interest
arxiv_filtered <- arxiv_filtered %>%
  filter(update_date >= start_date & update_date <= end_date)

# Calculate the average sentiment score per day
arxiv_avg_vader <- aggregate(arxiv_filtered$vader_sen, by=list(arxiv_filtered$update_date), FUN=mean)
colnames(arxiv_avg_vader) <- c("Date", "Avg_Sentiment")

# Calculate the 7-day rolling mean of sentiment score
arxiv_avg_vader$Rolling_Mean <- rollmean(arxiv_avg_vader$Avg_Sentiment, k = 30, fill = NA, align = "right")

# Create a plotly line chart of the average sentiment score per day
p <- plot_ly(arxiv_avg_vader, x = ~Date, y = ~Avg_Sentiment, type = 'scatter', mode = 'lines', name = 'Daily Average_VADER') %>%
  layout(title = "Average Sentiment Score per Day", 
         xaxis = list(title = "Date"), 
         yaxis = list(title = "Average Sentiment Score"))

# Add the 7-day rolling mean to the plot
p <- add_trace(p, x = ~Date, y = ~Rolling_Mean, type = 'scatter', mode = 'lines', name = '30-day Rolling Mean NRC')

# Add a red vertical line at 30 November 2022
marker_date <- as.Date("2022-11-30")
p <- add_segments(p, x = marker_date, xend = marker_date, y = -1, yend = 1, line = list(color = 'red'), name = 'ChatGPT Release')

# Display the plot
p

### 3.2.5. Statistical significance test
# Filter the data to include only the year before and after the release of ChatGPT
arxiv_filtered <- arxiv_filtered %>%
  filter(update_date >= "2021-11-22" & update_date <= "2023-11-22")

# Split the data into two groups
arxiv_filtered_before <- arxiv_filtered %>%
  filter(update_date < "2022-11-30")

arxiv_filtered_after <- arxiv_filtered %>%
  filter(update_date >= "2022-11-30")

set.seed(2)
before = sample(arxiv_filtered_before$nrc_sen,5000)
after = sample(arxiv_filtered_after$nrc_sen,5000)

# plot histogram of sentiment scores
hist(before, breaks = 50, main = "Histogram of Sentiment Scores", xlab = "Sentiment Score")
hist(after,breaks = 50, main = "Histogram of Sentiment Scores", xlab = "Sentiment Score")

# Check the assumption of equal variance
var.test(before, after)

# Perform the Welch Two Sample t-test
t.test(before, after, var.equal = FALSE)

## 3.3. Assumptions
# Generate 100 random numbers between 1 and the number of rows in arxiv_filtered
random_indices <- sample(1:nrow(arxiv_filtered), 100) # forgot the seed  ...

# Create a new dataframe by subsetting arxiv_filtered using the random indices
arxiv_filtered_sampled <- arxiv_filtered[random_indices, ]

# Print the new dataframe
print(arxiv_filtered_sampled)
write.csv(arxiv_filtered_sampled, "arxiv_titles.csv", row.names = FALSE)

arxiv_texts <- read_csv("data/arxiv_text.csv")

for(i in 1:nrow(arxiv_texts)){
  Text <- arxiv_texts$Text[i]
  # create a text corpus
  corpus <- Corpus(VectorSource(Text))
  
  # preprocess text
  corpus_clean <- corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(stripWhitespace)
  
  Text_clean <- as.character(corpus_clean[[1]])
  
  # replace the abstract with the cleaned version
  arxiv_texts$Text[i] <- Text_clean
}

arxiv_texts$nrc_sen <- NA

for (i in 1:nrow(arxiv_texts)) {
  nrc_sentiment <- get_sentiment(arxiv_texts$Text[i], method="syuzhet")
  arxiv_texts$nrc_sen[i] <- nrc_sentiment
}

arxiv_texts$vader_sen <- NA

for (i in 1:nrow(arxiv_texts)) {
  vader_sentiment <- get_vader(arxiv_texts$Text[i])[2]
  arxiv_texts$vader_sen[i] <- vader_sentiment
}
write.csv(arxiv_texts, "arxiv_text.csv", row.names = FALSE)

arxiv_texts <- read_csv("data/arxiv_text.csv")

df_combined <- merge(arxiv_filtered, arxiv_texts, by.x="title", by.y="Title", suffixes=c("_abstract", "_text"))

# Your text string
text = df_combined$Text[1]

# Split the text into words
words <- strsplit(text, "\\W")[[1]]

# Filter words longer than 2 characters and count them
word_count <- sum(nchar(words) > 2)

print(word_count)

# Iterate over each element in 'nrc_sen_text'
for(i in 1:length(df_combined)){
  # Access the element
  text = df_combined$Text[i]
  abstract = df_combined$abstract[i]
  
  words <- strsplit(text, "\\W")[[1]]
  words2 <- strsplit(abstract, "\\W")[[1]]
  
  word_count <- sum(nchar(words) > 2)
  word_count2 <- sum(nchar(words2) > 2)
  
  df_combined$nrc_sen_text = df_combined$nrc_sen_text / word_count
  df_combined$vader_sen_text = df_combined$vader_sen_text / word_count
  
  df_combined$vader_sen_abstract = df_combined$vader_sen_abstract / word_count2
  df_combined$nrc_sen_abstract = df_combined$nrc_sen_abstract / word_count2
}

# Normalize the columns
df_combined$nrc_sen_text <- (df_combined$nrc_sen_text - min(df_combined$nrc_sen_text)) / (max(df_combined$nrc_sen_text) - min(df_combined$nrc_sen_text))

df_combined$vader_sen_text <- (df_combined$vader_sen_text - min(df_combined$vader_sen_text)) / (max(df_combined$vader_sen_text) - min(df_combined$vader_sen_text))

df_combined$vader_sen_abstract <- (df_combined$vader_sen_abstract - min(df_combined$vader_sen_abstract)) / (max(df_combined$vader_sen_abstract) - min(df_combined$vader_sen_abstract))

df_combined$nrc_sen_abstract <- (df_combined$nrc_sen_abstract - min(df_combined$nrc_sen_abstract)) / (max(df_combined$nrc_sen_abstract) - min(df_combined$nrc_sen_abstract))

# Create a new column 'index' which will act as the x-axis
df_combined$index <- 1:nrow(df_combined)

# Convert dataframe to long format
df_long <- reshape2::melt(df_combined, id.vars = "index", measure.vars = c("nrc_sen_abstract", "nrc_sen_text"))

# Create separate data frames for each variable
df_abstract <- df_long[df_long$variable == "nrc_sen_abstract", ]
df_text <- df_long[df_long$variable == "nrc_sen_text", ]

# Calculate distance for each index
df_distance <- df_abstract %>%
  inner_join(df_text, by = "index", suffix = c("_abstract", "_text")) %>%
  mutate(distance = abs(value_abstract - value_text)) %>%
  select(index, distance)

# Create plotly object for 'nrc_sen_abstract'
fig <- plot_ly(df_abstract, x = ~index, y = ~value, type = "scatter", mode = "markers", marker = list(color = 'red'), name = 'nrc_sen_abstract')

# Add 'nrc_sen_text'
fig <- fig %>% add_trace(data = df_text, x = ~index, y = ~value, type = "scatter", mode = "markers",marker = list(color = 'blue'), name = 'nrc_sen_text')

# Add 'distance'
fig <- fig %>% add_trace(data = df_distance, x = ~index, y = ~distance, type = "scatter", mode = "markers", marker = list(color = 'green'), name = 'distance')

# Create list of lines
line_list <- lapply(unique(df_long$index), function(i) {list(type = 'line', line = list(color = 'grey',width=0.5), x0 = i, x1 = i, y0 = 0, y1 = 1)
})

# Add all lines to the layout
fig <- fig %>% layout(shapes = line_list)

# Display the plot
fig

## 3.4. Topic Modelling
### 3.4.1. Loading and Preprocessing the data
df = read_csv('data/arxiv_sentiments.csv')

corpus = Corpus(VectorSource(df$abstract))

processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, stopwords("en"))
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

### 3.4.2. Document-Term Matrix (DTM)

# compute document term matrix with terms >= minimumFrequency
minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
dim(DTM)
  

### 3.4.3. Finding the optimal topic numbers
# create models with different number of topics 
result <- FindTopicsNumber(
  DTM,
  topics = 1:20,
  metrics = c("CaoJuan2009",  "Deveaud2014")
)

FindTopicsNumber_plot(result)

### 3.4.4. LDA model
# number of topics
K <- 20
# set random number generator seed
set.seed(9161)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 1000, verbose = 25))


### 3.4.5. Model Checking
# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)
nTerms(DTM)              # lengthOfVocab
# topics are probability distributions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta)                # K distributions over nTerms(DTM) terms
rowSums(beta)            # rows in beta sum to 1
nDocs(DTM)               # size of collection
# for every document we have a probability distribution of its contained topics
theta <- tmResult$topics 
dim(theta)               # nDocs(DTM) distributions over K topics
rowSums(theta)[1:10]     # rows in theta sum to 1


### 3.4.6. Results
terms(topicModel, 10)
top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
topics <- apply(theta, 1, which.max)
df$topic <- topicNames[topics]

# visualize topics as word cloud
topicToViz <- 3 # change for your own topic of interest
# select top 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

exampleIds <- c(2, 100, 200)

N <- length(exampleIds)

# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)

df$update_date <- as.Date(df$update_date)
specific_date <- as.Date("2021-12-27")
abstracts_on_specific_date <- df[df$update_date == specific_date,]
df$update_date <- as.Date(df$update_date)
dates_to_remove <- as.Date(c("2021-11-26","2021-11-28", "2021-12-27", "2022-05-01", "2022-09-06", "2022-09-25", "2023-05-13"))
df <- df[!(df$update_date %in% dates_to_remove),]

counts <- table(df$topic)
counts_df <- data.frame(A = names(counts), Count = as.numeric(counts))

# Create bar plot
p <- plot_ly(counts_df, x = ~A, y = ~Count, type = 'bar') %>%
  layout(xaxis = list(title = "A"), yaxis = list(title = "Count"), 
         title = "Number of articles per Topic")
# Display the plot
p

generate_topic_plot_nrc <- function(t) {
  filtered_grouped <- df %>%
    filter(topic == t)
  
  filtered_grouped$update_date <- as.Date(filtered_grouped$update_date)
  
  # Define start and end dates
  start_date <- as.Date("2021-11-22")
  end_date <- as.Date("2023-11-22")
  
  # Filter the data to include only two years of interest
  filtered_grouped <- filtered_grouped %>%
    filter(update_date >= start_date & update_date <= end_date)
  
  ###NRC
  # Calculate the average sentiment score per day
  arxiv_avg_nrc <- aggregate(filtered_grouped$nrc_sen, by=list(filtered_grouped$update_date), FUN=mean)
  colnames(arxiv_avg_nrc) <- c("Date", "Avg_Sentiment")
  
  # Calculate the 30-day rolling mean of sentiment score
  arxiv_avg_nrc$Rolling_Mean <- rollmean(arxiv_avg_nrc$Avg_Sentiment, k = 30, fill = NA, align = "right")
  
  # Create a plotly line chart of the average sentiment score per day
  p <- plot_ly(arxiv_avg_nrc, x = ~Date, y = ~Avg_Sentiment, type = 'scatter', mode = 'lines', name = 'Daily Average_NRC') %>%
    layout(title = paste("Average Sentiment Score per Day (Topic:", t, ")"), 
           xaxis = list(title = "Date"), 
           yaxis = list(title = "Average Sentiment Score"))
  
  # Add the 7-day rolling mean to the plot
  p <- add_trace(p, x = ~Date, y = ~Rolling_Mean, type = 'scatter', mode = 'lines', name = '30-day Rolling Mean NRC')
  
  # Add a red vertical line at 30 November 2022
  marker_date <- as.Date("2022-11-30")
  p <- add_segments(p, x = marker_date, xend = marker_date, y = 0, yend = 12, line = list(color = 'red'), name = 'ChatGPT Release')
  
  # Return the plot
  return(p)
}

topic4 <- 'imag object method map deep'
generate_topic_plot_nrc(topic4)

topic7 <- 'network neural deep architectur convolut'
generate_topic_plot_nrc(topic7)

topic10 <- 'interpret decis human explain understand'
generate_topic_plot_nrc(topic10)

topic11 <- 'predict time use event seri'
generate_topic_plot_nrc(topic11)

topic14 <- 'languag task generat use code'
generate_topic_plot_nrc(topic14)

topic15 <- 'research develop applic intellig challeng'
generate_topic_plot_nrc(topic15)

topic18 <- 'featur classif use propos method'
generate_topic_plot_nrc(topic18)