library(tidyverse)
library(rtweet)
library(grid)
library(visdat)
library(tidytext)
library(textdata)
library(gutenbergr)
library(igraph)
library(ggraph)

head(sentiments)

get_sentiments("bing")

titles <- c("The War of the Worlds",
            "The Time Machine",
            "Twenty Thousand Leagues under the Sea", 
            "The Invisible Man: A Grotesque Romance")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

all_text <- books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

# Generate some summary data of The War of The Worlds
all_text %>%
  filter(title == "The War of the Worlds") %>%
  group_by(word) %>%
  tally(sort = TRUE) %>%
  top_n(10)

all_text %>%
  filter(title == "The War of the Worlds") %>%  
  count(word) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) +
  labs(title = "Top 10 words in The War of the Worlds", 
       x = "Word",
       y = "Count")

# Sentiment analysis
all_text_sent <- all_text %>%
  inner_join(get_sentiments("bing")) 

all_text_sent %>%
  filter(title == "The War of the Worlds") %>%
  count(word, sentiment, sort = TRUE) %>%
  top_n(25) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(title = "Sentiment Analysis of Top 25 Words in The War of the Worlds", 
       x = "Word",
       y = "Count") 

all_text_sent %>%
  filter(title == "The Time Machine") %>%
  count(word, sentiment, sort = TRUE) %>%
  top_n(25) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(title = "Sentiment Analysis of Top 25 Words in The Time Machine", 
       x = "Word",
       y = "Count") 

#Zipfs law
book_words <- all_text %>% group_by(title) %>% count(title, word, sort = TRUE)
total_words <- book_words %>% group_by(title) %>% summarise(total = sum(n))
book_words <- left_join(book_words, total_words)

book_words %>%
  mutate(proportion = n/total) %>%
  group_by(title) %>%
  arrange(desc(proportion)) %>%
  top_n(5)

book_words %>%
  ggplot(aes(x = n/total, fill = title)) +
  geom_histogram(show.legend = FALSE, bins = 10) +
  xlim(NA, 0.0009) +
  facet_wrap(~title, ncol = 2, scales = "free")



# The bind_tf_idf Function
# Working out the important words for each book - adds a weighting to each word,
# decreading the weight for commonly used words and increasing the weight for
# words not used much in the corpus.
book_words <- book_words %>%
  bind_tf_idf(word, title, n)

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>%
  group_by(title) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Term frequency and inverse document frequency") +
  facet_wrap(~title, ncol = 2, scales = "free") +
  coord_flip()

# Tokenizing by N-gram
wells_bigrams <- books %>% 
  filter(title == "The War of the Worlds") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- wells_bigrams %>%
  separate(bigram, c("word1", "word2", sep = " "))

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Network graph
bigram_graph <- bigrams_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()

set.seed(1234)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  labs(title = "Graph of Bigram Relationships in War of the Worlds") +
  theme_void()



# Titter scraping
tweets <- search_tweets("Opeth", n = 500, include_rts = FALSE, retryonratelimit = TRUE) 

tweets <- tweets %>% separate(col = created_at, into = c("date", "time"), sep = " ") 

img <- jpeg::readJPEG("images/opeth.jpg")
g <- rasterGrob(img, interpolate=TRUE) 

ggplot(tweets, aes (x = date)) + 
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_bar(fill = "white", alpha = .5) + 
  labs(x = "Date", y = "Number of Tweets") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = paste("Tweets Mentioning Opeth Scraped On", Sys.Date()),
       subtitle = "Using Mike Kearney's rtweet package") 

timeline_tweets <- get_timeline(user = c("neilhimself", "StephenKing"), 
                                n = 1000, max_id = NULL, home = FALSE, 
                                parse = TRUE, check = TRUE)

timeline_tweets %>%
  filter(is_retweet == FALSE) %>%
  group_by(screen_name) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(word != "https" & word != "t.co") %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(aes(fill = screen_name)) +
  coord_flip() +
  labs(x = NULL) +
  facet_wrap(~screen_name, scales = "free") +
  guides(fill = FALSE)

# Which words characterise Neil Gaiman's Tweets vs Stephne King's?
tweet_words <- timeline_tweets %>% 
  filter(is_retweet == FALSE) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(word != "https" & word != "t.co") %>%
  group_by(screen_name) %>% 
  count(screen_name, word, sort = TRUE)

total_tweets <- tweet_words %>% 
  group_by(screen_name) %>% 
  summarise(total = sum(n))

tweet_words <- left_join(tweet_words, total_tweets)

tweet_words %>%
  mutate(proportion = n/total) %>%
  group_by(screen_name) %>%
  arrange(desc(proportion)) %>%
  top_n(5)

tweet_words <- tweet_words %>%
  bind_tf_idf(word, screen_name, n)

tweet_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

tweet_words %>%
  group_by(screen_name) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Term frequency and inverse document frequency") +
  facet_wrap(~screen_name, ncol = 2, scales = "free") +
  coord_flip()

# Time of Tweeting
timeline_tweets %>% 
  filter(is_retweet == FALSE) %>%
  separate(col = created_at, into = c("date", "time"), sep = " ") %>%
  mutate(time = parse_time(time)) %>%
  ggplot(aes(x = time)) +
  geom_histogram(alpha = .5, bins = 24) +
  facet_wrap(~screen_name, ncol = 1, scales = "free") +
  labs(x = "Time of Day Tweet Created", y = "Number of Tweets")

# Type of Device
timeline_tweets %>%
  group_by(screen_name, source) %>%
  tally()

# Neil Gaiman's Tweets by Source
timeline_tweets %>% 
  filter(screen_name == "neilhimself") %>%
  #filter(source == "Twitter for Android" | source == "Twitter for iPhone") %>%
  separate(col = created_at, into = c("date", "time"), sep = " ") %>%
  mutate(time = parse_time(time)) %>%
  ggplot(aes(x = time)) +
  geom_histogram(alpha = .5, bins = 24) +
  facet_wrap(~source)

