####################
#####Chord Diagram#####
## August 25,2020 ##
####################

library(sentiment)
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(stringr)
library(textdata)

#Read data
lyrics<-read.csv2(file = "taytaylyrics.csv",stringsAsFactors = FALSE)

glimpse(lyrics)
#Dedup songs and organize level on albuns
lyrics_dedup<-
  lyrics %>%
  group_by(track_name)%>%
  mutate(total=n())%>%
  ungroup()%>%
  select(album_name,track_name,text,total)%>%
  mutate(album_name = factor(album_name, levels = c("Taylor Swift", "Fearless", "Speak Now", "Red", "1989", "reputation","Lover","folklore")))%>%
  unique()

glimpse(lyrics_dedup)

#Tokenizing lyrics 
tidy_lyrics <- lyrics_dedup %>%
  select(album_name,text,track_name)%>%
  group_by(album_name,track_name)%>%
  ungroup() %>%
  unnest_tokens(word, text)

#getting emotions to analyze sentiment
emotions<-
tidy_lyrics %>%
inner_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  group_by(album_name,track_name)%>%
  count(sentiment, sort = TRUE)%>%
  arrange(desc(album_name))

#Preparing chord diagram axis
grid.col = c("Taylor Swift" = "grey", 
             "Fearless" ="grey", 
             "Speak Now" = "grey", 
             "Red" = "grey", 
             "1989" = "grey",
             "reputation" = "grey",
             "Lover" = "grey",
             "folklore" = "grey",
             "anger" = "#E69F00", 
             "anticipation" = "#F0E442", 
             "disgust" = "#0072B2", 
             "fear" = "#D55E00", 
             "joy" = "#CC79A7", 
             "sadness" = "#009E73", 
             "surprise" = "#999999", 
             "trust" = "#56B4E9",
             "positive"="#CC0000",
             "negative"="#000099")


# Grouping songs on albuns to calculate intensity of emotions
chord <- emotions %>%
  ungroup() %>%
  select(sentiment,album_name,n) %>%
  group_by(album_name,sentiment)%>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup() %>%
  select(sentiment,album_name,sentiment_sum)

#Clear plot
circos.clear()

#Set the variables you want to show on the plot
circos.par(gap.after = c(rep(8, length(unique(chord[[1]])) - 1), 15,
                         rep(8, length(unique(chord[[2]])) - 1), 15))

library(circlize)

#Plot chord diagram
chordDiagram(chord,grid.col = grid.col, transparency = .2)
title("Taylor Swift Mood Over Her Albuns")