############################
#####Drinking References####
##### November 23,2021 #####
############################

#Packages used
library(dplyr)
library(tidyverse)
library(vagalumeR)
library(purrr)
library(tm)
library(quanteda)
library(ggplot2)
library(reshape2)

key <- VAGALUME_KEY

# defining the artist 
artist <- "taylor-swift"

# song names retrieval
song <- songNames(artist)

#Get artist info
artist %>%  map_dfr(artistInfo)

#get song names to get lyrics
songs <- artist %>%  
         map_dfr(songNames)

#Using vagalume API to retrieve lyrics
lyr <- songs %>% 
  pull(song.id) %>% 
  map_dfr(lyrics, 
          artist = "taylor-swift", 
          type = "id", 
          key = key)

glimpse(lyr)

#Join id to find the track name
taytaylyrics<- songs %>%
  inner_join(lyr,by=c("song.id" = "song.id")) %>%
  unique() %>%
  select(-song.y,-translation,-language,-lang,-name.y,-id.y,-id.x)%>%
  mutate(track_name=tolower(song.x),
         artist.name = name.x) %>%
  select(-song.x,-artist.name,-name.x) 

glimpse(taytaylyrics)

#Upload data from Spotify from acquisition.R
tay_cds<-read.csv2(file = "taytayspotify.csv",stringsAsFactors = FALSE)

#Join Spotify Data to map songs to albuns
lyrics<-
  tay_cds %>%
  select(-artist_name,-artist_id,-album_id,-album_release_date,-track_id,-time_signature,-track_uri,-key) %>%
  mutate(track_name= tolower(track_name)) %>%
  inner_join(taytaylyrics,by=c("track_name"="track_name")) %>%
  select(-song.id) %>%
  unique()

csv_ok<-read.csv2(file = "data/taytaylyrics.csv")

#Counting alchool references
dfmat <- tokens(lyrics$text) %>%
  tokens_select(mydict) %>%
  dfm()

drinking<-
  tokens(lyrics$text) %>%
  tokens_select(c("wine","whiskey", "patron","beer","cider","drunk",
                  "rose","champagne","old fashioned","dom perignon",
                  "merlot","beers","liquor","drinking","drinks",
                  "bottles","toast","bar")) %>%
  dfm()

# Converting to a matrix
drinking_matrix = as.matrix(drinking)

#converting to data frame
drinking.df<-as.data.frame(drinking_matrix)

#binding both data frames
drinking_full <- cbind(lyrics,drinking.df) 

drinking_words<-
  drinking_full %>%
  select(album_name,21:32)
  cols <- c("wine","champagne","rose","merlot","bar","beers","drunk","whiskey",
          "toast","drinks","drinking","beer")

#aggregate words by album
drinking_agg<-
  drinking_words %>% 
  group_by(album_name) %>% 
  summarise_all(sum)

#sum of words by album
drinking_sum <-
  drinking_agg %>%
  rowwise() %>% 
  mutate(sumrange = sum(c_across(2:13), na.rm = T))%>%
  
library(ggplot2)
library(reshape2)

#Melting columns to help with viz
drinking.melt = melt(drinking_sum,id=c("album_name","sumrange"))

#Making viz
drinking.melt %>%
  mutate(album_name = factor(album_name,
                             levels=c("Taylor Swift","Fearless","Speak Now",
                                      "Red","1989","reputation","Lover",
                                      "folklore","evermore")),
  variable = as.character(variable),
  words = if_else((variable == "beers" | variable == "beer"), "beer", variable))%>%
  ggplot(aes(x = words , y = value)) +
  geom_point() +
  facet_wrap(~ album_name, scales = "free") +
  coord_flip() +
  ylim(1, 20)+
  labs(x = "Words", y = "Frequency")+
  labs(title = "Taylor Swift’s References to Alcohol by Album",  
       subtile  = "Data Source: VagaLume API",
       caption = "by @fernanda_bruno_ | Code available on https://github.com/fernandabruno/swiftistics")+
  theme(plot.title = element_text(hjust = 0.5))

#Stacked bar plot
library(viridis)
drinking.melt %>%
  mutate(album_name = factor(album_name,
                             levels=c("Taylor Swift","Fearless","Speak Now",
                                      "Red","1989","reputation","Lover",
                                      "folklore","evermore")),
         variable = as.character(variable),
         words = if_else((variable == "beers" | variable == "beer"), "beer", variable))%>%
  ggplot(aes(fill=words, y=value, x=album_name,label = value)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_brewer(palette="Spectral") +
  labs(x = "Album", y = "Frequency")+
  labs(title = "Taylor Swift’s References to Alcohol by Album",  
       subtile  = "Data Source: VagaLume API",
       caption = "by @fernanda_bruno_ | Code available on https://github.com/fernandabruno/swiftistics")+
  theme(plot.title = element_text(hjust = 0.5))
  
