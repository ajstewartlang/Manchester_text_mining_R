library(tidyverse)
library(rtweet)
library(tidytext)
library(grid)
library(igraph)
library(ggraph)
library(stringi)
library(leaflet)
library(ggthemes)

tweets <- search_tweets("Opeth", n = 200000, include_rts = FALSE, retryonratelimit = TRUE) 

tweets <- tweets %>% separate(col = created_at, into = c("date", "time"), sep = " ") 

img <- jpeg::readJPEG("images/opeth2.jpg")
g <- rasterGrob(img, interpolate=TRUE) 

ggplot(tweets, aes (x = date)) + 
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_bar(fill = "white", alpha = .5) + 
  labs(x = "Date", y = "Number of Tweets") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = paste("Tweets Mentioning Opeth Scraped On", Sys.Date())) 

# bigrams

opeth_bigrams_tbl <- tibble(tweets$text) 

opeth_bigrams <- opeth_bigrams_tbl %>% 
  unnest_tokens(bigram, tweets$text, token = "ngrams", n = 2)

opeth_bigrams_separated <- opeth_bigrams  %>%
  separate(bigram, c("word1", "word2", sep = " "))

bigrams_filtered <- opeth_bigrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Network graph
bigram_graph <- bigrams_counts %>%
  filter(stri_enc_isascii(word1)) %>%
  filter(n > 25) %>%
  graph_from_data_frame()

img <- jpeg::readJPEG("images/opeth3.jpg")
g <- rasterGrob(img, interpolate=TRUE) 

set.seed(3)
ggraph(bigram_graph, layout = "fr") +
  #annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_edge_link(colour = "black") +
  geom_node_point(colour = "black") +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, colour = "black", size = 4) +
  labs(title = paste("Graph of Bigram Relationships in Tweets mentioning Opeth", Sys.Date())) +
  theme(text = element_text(size = 20)) +
  theme_void()

# 1-word ngram

to_plot <- tibble(tweets$text)

all_text <- to_plot %>%
  unnest_tokens(word, tweets$text) %>%
  anti_join(stop_words) 

all_text %>%
  filter(word != "https" & 
           word != "t.co" &
           word != "opeth" &
           word != "") %>%
  count(word) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  guides(fill = FALSE) +
  labs(title = "Top 10 words in Tweets about Opeth", 
       x = "Word",
       y = "Count")

all_text_sent <- all_text %>%
  inner_join(get_sentiments("bing")) 

all_text_sent %>%
  mutate(Sentiment = sentiment) %>%
  count(word, sentiment, sort = TRUE) %>%
  top_n(25) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(title = "Sentiment Analysis of \nTweets about Opeth",
       x = NULL,
       y = "Count") +
  theme_economist() +
  theme(text = element_text(size=12))
        
all_text_sent <- all_text %>%
  inner_join(get_sentiments("nrc")) 

all_text_sent %>%
  count(word, sentiment, sort = TRUE) %>%
  top_n(35) %>%
  #mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(title = paste("Sentiment Analysis of Opeth Tweets ", Sys.Date()),
       x = NULL,
       y = "Count") +
  theme_economist() +
  theme(text = element_text(size = 12))

# search

pdrj <- search_tweets("progressive doom rock jazz", n = 1000, include_rts = FALSE, retryonratelimit = TRUE) 

# mapping
my_map <- lat_lng(tweets)
to_plot <- leaflet(my_map) %>% 
  addTiles()

to_plot %>% addCircles(lng = ~lng, lat = ~lat, weight = 8, radius = 40, 
                       color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)

# Scrap below

tweets <- search_tweets("Trump", n = 20000, include_rts = FALSE, retryonratelimit = TRUE) 

tweets <- tweets %>% separate(col = created_at, into = c("date", "time"), sep = " ") 

# trigrams

bigrams_tbl <- tibble(tweets$text) 

tweets %>% 
  as_tibble() %>%
  select(text) %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 3) %>%
  group_by(ngram) %>%
  tally(sort = TRUE) %>% 
  view()

bigrams <- bigrams_tbl %>% 
  unnest_tokens(bigram, tweets$text, token = "ngrams", n = 3)

bigrams %>% 
  group_by(bigram) %>%
  tally(sort = TRUE)

view(bigrams_counts) 

bigrams_separated <- bigrams  %>%
  separate(bigram, c("word1", "word2", "word3", sep = " "))

bigrams_filtered <- bigrams_separated  %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

bigrams_counts <- bigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE)

bigrams %>% 
  group_by(bigram) %>%
  tally(sort = TRUE)

view(bigrams_counts) 

# Network graph
bigram_graph <- bigrams_counts %>%
  filter(stri_enc_isascii(word1)) %>%
  filter(stri_enc_isascii(word2)) %>%
  filter(n > 15) %>%
  graph_from_data_frame()

img <- jpeg::readJPEG("images/opeth3.jpg")
g <- rasterGrob(img, interpolate=TRUE) 

set.seed(3)
ggraph(bigram_graph, layout = "fr") +
  #annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  #geom_edge_link(colour = "black", alpha = .2) +
  geom_edge_arc(edge_width = 0.5, curvature = 0.2, alpha = 0.2) +
  geom_node_point(colour = "black") +
  geom_node_text(aes(label = name), repel = TRUE, colour = "black", size = 4) +
  labs(title = paste("Graph of Bigram Relationships in Tweets mentioning Opeth", Sys.Date())) 
  #theme(text = element_text(size = 20)) +
  #theme_void()
