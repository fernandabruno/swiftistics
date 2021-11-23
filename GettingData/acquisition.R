####################
##Data Acquisition##
## August 20,2020 ##
####################

#Spotify package used
#devtools::install_github('charlie86/spotifyr')

#Packages used
library(spotifyr)
library(dplyr)
library(tidyverse)

#setting up access to spotify API
#Use your information available on https://developer.spotify.com/dashboard/applications
Sys.setenv(SPOTIFY_CLIENT_ID = 'CLIENT_ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'CLIENT_SECRET')
access_token <- get_spotify_access_token()

###############
#IDs e albuns#
##############
#red (taylor's version) - "6kZ42qRrzov54LcAk4onW9"
#fearless (taylor's version) - "4hDok0OAJd57SGIT8xuWJH"
#evermore - "2Xoteh7uEpea4TohMxjtaq"
#Folklore - "2fenSS68JI1h4Fo296JfGr"
#Lover - "1NAmidJlEaVgA3MpcPFYGq"
#Reputation - "6DEjYFkNZh67HP7R9PSZvv"
#1989 - "5fy0X0JmZRZnVa2UEicIOl"
#Red - "1KVKqWeRuXsJDLTW0VuD29"
#Speak now - "6Ar2o9KCqcyYF9J0aQP3au"
#Fearless - "2gP2LMVcIFgVczSJqn340t"
#Taylor Swift - "5eyZZoQEFQWRHkV2xgAeBw"

ids_albuns=c("6kZ42qRrzov54LcAk4onW9",
             "4hDok0OAJd57SGIT8xuWJH",
             "2Xoteh7uEpea4TohMxjtaq",
             "2fenSS68JI1h4Fo296JfGr",
             "1NAmidJlEaVgA3MpcPFYGq",
             "6DEjYFkNZh67HP7R9PSZvv",
             "5fy0X0JmZRZnVa2UEicIOl",
             "1KVKqWeRuXsJDLTW0VuD29",
             "6Ar2o9KCqcyYF9J0aQP3au",
             "2gP2LMVcIFgVczSJqn340t",
             "5eyZZoQEFQWRHkV2xgAeBw")

id_albuns = data.frame(id = c("6kZ42qRrzov54LcAk4onW9",
                              "4hDok0OAJd57SGIT8xuWJH",
                              "2Xoteh7uEpea4TohMxjtaq",
                              "2fenSS68JI1h4Fo296JfGr",
                              "1NAmidJlEaVgA3MpcPFYGq",
                              "6DEjYFkNZh67HP7R9PSZvv",
                              "5fy0X0JmZRZnVa2UEicIOl",
                              "1KVKqWeRuXsJDLTW0VuD29",
                              "6Ar2o9KCqcyYF9J0aQP3au",
                              "2gP2LMVcIFgVczSJqn340t",
                              "5eyZZoQEFQWRHkV2xgAeBw"))

nome_albuns = data.frame(nome=c("Red (Taylor's Version","Fearless (Taylor's Version)","Evermore",
                                "Folkore","Lover","Reputation","1989","Red","Speak Now","Fearless","Taylor Swift"))

#Get album data using spotifyr package
taytay<-get_artist_audio_features(artist = "Taylor Swift", include_groups = "album",
                                  return_closest_artist = TRUE, dedupe_albums = TRUE,
                                  authorization = get_spotify_access_token())

#Cleaning data and keeping only studio albuns mapped above removing columns that seems irrelevant to the analysis
#Tearsdrops On My Guitar on the 2006 - Taylor Swift album, there are 2 versions: Pop Version and Radio Single Remix
#Naming Fearless Platinum Edition to Fearless for clarity
tay_cds <-
  taytay %>%
  select(-album_type,-album_release_date_precision,-analysis_url,-artists,-available_markets,
         -disc_number,-track_href,-is_local,-track_preview_url,-type,-external_urls.spotify)%>%
  filter(album_name %in% c("folklore","Lover","reputation","1989","Red", "Speak Now",
                           "Fearless Platinum Edition","Taylor Swift")) %>%
  filter(!grepl(".*Karaoke Version.*",track_name)) %>%
  filter(!grepl(".*Piano Version.*",track_name)) %>%
  filter(!grepl(".*POP Mix.*",track_name)) %>%
  mutate(album_name= ifelse(album_name=="Fearless Platinum Edition","Fearless",album_name))

#checking structure 
str(tay_cds)

#Cheking values
glimpse(tay_cds)

write.csv2(tay_cds, file = "taytayspotify.csv",row.names = FALSE)
