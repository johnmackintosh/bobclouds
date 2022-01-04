library(tidyverse)
library(tidytext)
library(hrbrthemes)
library(viridis)
library(extrafont)
library(ggimage)



`%notin%` = function(x,y) !(x %in% y)

data <- readRDS(file="shrubbery.RDS") 

data <-  data %>% 
    mutate(linenumber = row_number())  




data2 <- data %>%  select(textDisplay, linenumber)

text_df <- data2 %>% 
    unnest_tokens(word,textDisplay) %>% 
    filter(word %notin% c("a","it","39",'44','www.youtube.com', 'https','href', 'br','quot', 'f21mtojmcac', 'msuuivzs6js', 'amp', 'https.www.youtube.com','39', '39 s', '39 t', 'fucking','muslims', 'racist','muslim', 'trump')) %>% 
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

wc <- vidsentiment %>% 
  select(word, sentiment) %>% 
  filter(sentiment > 5) %>% 
  rename(n = sentiment) %>% 
  mutate(word = reorder(word,n)) 

wordcloud2::wordcloud2(wc)
