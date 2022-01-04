library(tidyverse)
library(tidytext)
library(hrbrthemes)
library(viridis)
library(extrafont)
library(ggimage)



`%notin%` = function(x,y) !(x %in% y)

data <- readRDS(file="apple.RDS") 

data <-  data %>% 
    mutate(linenumber = row_number())  




data2 <- data %>%  select(textDisplay, linenumber)

text_df <- data2 %>% 
    unnest_tokens(word,textDisplay) %>% 
    filter(word %notin% c("a","it","39",'44','www.youtube.com', 'https',
                          'href', 'br','quot', 'f21mtojmcac', 'racist')) %>% 
    mutate(linenumber = row_number())


data("stop_words")

tidy_df <- text_df %>% 
    anti_join(stop_words) %>% 
    filter(stringr::str_detect(word,"[a-z`]$"),
           !word %in% stop_words$word)


# sentiment

vidsentiment <- text_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  filter(sentiment > 5)


# ggplot(vidsentiment, aes(word, sentiment, fill = sentiment)) +
#   geom_col(show.legend = FALSE) +
#   theme_ipsum() +
#   ggExtra::removeGrid() +
#   coord_flip() +
# 
#   ggtitle("Overall Sentiment Score - Apple",
#           subtitle = "Bob can tear an apple in half with his bare hands") +
#   labs(x = NULL, y = NULL) +
#   geom_text(aes(label = sentiment), hjust = 0, nudge_y = 7) +
#   theme(axis.text.x = element_blank(),
#         axis.ticks.x = element_blank())




# wc <- tidy_df %>% 
#     count(word, sort = TRUE) %>%
#     filter(n > 5) %>% 
#     ungroup() %>% 
#     mutate(word = reorder(word,n)) 


wc <- vidsentiment %>% 
 select(word, sentiment) %>% 
  filter(sentiment > 5) %>% 
    rename(n = sentiment) %>% 
  mutate(word = reorder(word,n)) 

wordcloud2::wordcloud2(wc)
