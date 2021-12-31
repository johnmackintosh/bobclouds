library(tidyverse)
library(tidytext)
library(hrbrthemes)
library(viridis)
library(extrafont)
library(ggimage)



`%notin%` = function(x,y) !(x %in% y)

data <- readRDS(file="mavis.RDS") 

data <-  data %>% 
    mutate(linenumber = row_number())  




data2 <- data %>%  select(textDisplay, linenumber)

text_df <- data2 %>% 
    unnest_tokens(word,textDisplay) %>% 
    filter(word %notin% c("a","it","39",'44','www.youtube.com', 
                          'https','href', 'br','quot', 'f21mtojmcac',
                          'msuuivzs6js', 'amp', 'https.www.youtube.com',
                          '39', '39 s', '39 t', 'fucking', 'muslims', 'racist',
                          'muslim', 'qs8amwyhftq'
    )) %>% 
    mutate(linenumber = row_number())


data("stop_words")

tidy_df <- text_df %>% 
    anti_join(stop_words) %>% 
    filter(stringr::str_detect(word,"[a-z`]$"),
           !word %in% stop_words$word)

wc <- tidy_df %>% 
    count(word, sort = TRUE) %>%
    filter(n > 5) %>% 
    ungroup() %>% 
    mutate(word = reorder(word,n)) 

wordcloud2::wordcloud2(wc)
