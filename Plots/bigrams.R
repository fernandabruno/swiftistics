####################
####n-Gram Graph####
## August 31,2020 ##
####################

library(tm)
library(ggplot2)
library(ggraph)
library(igraph)

#Data cleaning function
data.cleaning = function(x)
{
  # Words to lower case
  x = tolower(x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

#Reading TS lyrics deduplicated
lyrics<_dedup-read.csv2(file = "lyrics_dedup.csv",stringsAsFactors = FALSE)

#Filtering folklore
bigrams <- lyrics_dedup %>%
    filter(album_name=="folklore")%>%
    select(album_name,text,track_name)%>%
    group_by(album_name,track_name)%>%
    ungroup() %>%
    mutate(clean_lyrics_ok = data.cleaning(text))%>%
    mutate(clean_lyrics = removeWords(clean_lyrics_ok, c(stopwords("english"),"ooh","iii","didi", "woah","ohohoh","uhoh","haah","huh","woh","hea","till", "and", "are","is","that","cause","this","with","has","for","that","also","more","must","have","\n","(slower)","ive","youre","shouldve")))%>%
    unnest_tokens(pairs, clean_lyrics,token="ngrams",n=2) %>%
    count(pairs, sort = TRUE)

#Separeting words
bigrams_separated <- 
  bigrams %>%
  separate(pairs, c("word1", "word2"), sep = " ")

#Filtering occurence for n > 3
bigrams_separated %>%
filter(n >= 3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "#56B4E9", size = 3) +
  geom_node_text(color = "blue", aes(label = name), vjust = 2, size=3) +
  labs(title= "Taylor Swift Folklore Lyrics Bigrams",
       subtitle = "Examining which words tend to follow others on Taylor Swift lyrics ",
       caption = "Graph by @fernanda_bruno_ | Code available on https://github.com/fernandabruno/swiftistics",
       x = "", y = "")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none")
