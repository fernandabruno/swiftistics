####################
#####Word Cloud#####
## August 25,2020 ##
####################

#Packages used
library(dplyr)
library(tidyverse)
library(vagalumeR)
library(purrr)
library(wordcloud)

key <- "VAGALUME_KEY"

# defining the artist 
artist <- "taylor-swift"

# song names retrieval
song <- songNames(artist)

#Using vagalume API to retrieve lyrics
lyrics_data <- song %>% 
  pull(song.id) %>% 
  purrr::map(lyrics, 
             artist = artist,
             type = "id", 
             key = key) %>%
  purrr::map_df(data.frame)
#slice(-15) # There is a repeated lyric there!

#Join join id to find the track name
taytaylyrics<- song %>%
  inner_join(lyrics_data,by=c("song.id" = "song.id")) %>%
  select(-song.y,-translation,-language,-lang,-name.y,-id.y,-id.x)%>%
  mutate(track_name=tolower(song.x),
         artist.name = name.x) %>%
  select(-song.x,-artist.name,-name.x)

glimpse(lyrics_data)

write.csv2(lyrics_data, file = "taytaylyrics.csv",row.names = FALSE)

#Join Spotify Data to map songs to albuns
lyrics<-
  tay_cds %>%
  select(-artist_name,-album_images,-artist_id,-album_id,-album_release_date,-track_id,-time_signature,-track_uri,-key) %>%
  mutate(track_name= tolower(track_name)) %>%
  inner_join(taytaylyrics,by=c("track_name"="track_name"))

#Getting songs lyrics by album

#Taylor Swift
ts <- lyrics %>%
  filter(album_name=="Taylor Swift")%>%
  select(text)

#Fearless
speaknow <- lyrics %>%
  filter(album_name=="Fearless")%>%
  select(text)

# Speak Now
speaknow <- lyrics %>%
  filter(album_name=="Speak Now")%>%
  select(text)

# Red
speaknow <- lyrics %>%
  filter(album_name=="Red")%>%
  select(text)

# 1989
nineteen <- lyrics %>%
  filter(album_name=="1989")%>%
  select(text)

# Speak Now
speaknow <- lyrics %>%
  filter(album_name=="Speak Now")%>%
  select(text)

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


# Cleanning lyrics
ts_stage = data.cleaning(ts)
fearless_stage = data.cleaning(fearless)
speaknow_stage = data.cleaning(speaknow)
red_stage = data.cleaning(red)
nineteen_stage = data.cleaning(nineteen)
reputation_stage = data.cleaning(reputation)
lover_stage = data.cleaning(lover)
folklore_stage = data.cleaning(folklore)

#Join lyrics on a unique vector
ts_ok = paste(ts_stage, collapse=" ")
fearless_ok = paste(fearless_stage, collapse=" ")
speaknow_ok = paste(speaknow_stage, collapse=" ")
red_ok = paste(red_stage, collapse=" ")
nineteen_ok= paste(nineteen_stage, collapse=" ")
reputation_ok= paste(reputation_stage, collapse=" ")
lover_ok= paste(lover_stage, collapse=" ")
folklore_ok= paste(folklore_stage, collapse=" ")

# Putting everything in a single vector
all = c(ts_ok,fearless_ok,speaknow_ok,red_ok,nineteen_ok,reputation_ok,lover_ok,folklore_ok)

# Removing words that have no important meaning
all = removeWords(all,
c(stopwords("english"),"ooh","iii","didi", "woah","ohohoh","uhoh","haah","huh","woh","hea","till", "and", "are","is","that","cause","this","with","has","for","that","also","more","must","have","\n","(slower)","ive","youre","shouldve"))

# Creating corpus
corpus = Corpus(VectorSource(all))

# Creating term-document matrix
tdm = TermDocumentMatrix(corpus)

# Converting to a matrix
tdm = as.matrix(tdm)

# Adding column names to label the comparison
colnames(tdm) = c("Taylor Swift","Fearless","Speak Now","Red","1989","reputation","Lover","folklore")

#Creating wordcloud comparting albuns
pdf("TayTayComparisonCloud.pdf", width=8, height=8)
#png(file="wordcloud.png",height=600,width=1200)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                 title.size=1.5, 
                 max.words=3000)
dev.off()

#Word cloud that shows common words on songs
pdf("CommonWordsCloud.pdf", width=8, height=8)
commonality.cloud(tdm, random.order=FALSE, 
                  colors = brewer.pal(8, "Dark2"),
                  title.size=1.5)
dev.off()
