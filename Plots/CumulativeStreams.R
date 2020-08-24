######################
##Cumulative Streams##
#######Animation######
######22/08/2020######
######################

library(tidyverse)
library(fs)
library(gsubfn)
library(gganimate)
library(gifski)
library(png)

#Get my directory for my file on spotify
folder="YOUR_DIRECTORY"
data_dir <- path(folder)
#Read the contents of my directory
data_list = fs::dir_ls(data_dir, regexp = "\\.csv$")

#Read data and create new column based on date file name
charts <- data_list %>% 
  map_dfr(read_csv,skip=1, .id = "Source")%>%
  mutate(Date=strapplyc(Source, "\\d+-\\d+-\\d+", simplify = TRUE))%>%
  mutate(Date=as.Date(Date, "%Y-%m-%d"))%>%
  mutate(BarColor=ifelse(Artist=="Taylor Swift","#D55E00","#999999"))%>%
  select_all(~gsub("\\s+|\\.", "_", .)) %>%
  select_all(tolower)

#Lets show only the top 10 songs on the charts
#hard coded some things on the august 1st data do adjust cumulative sums
cumulative<-charts %>% 
  select(track_name,artist,date,streams,barcolor)%>%
  rbind(c("Love Story","Taylor Swift","2020-08-01",0))%>%
  filter(artist=="Taylor Swift") %>%
  mutate(barcolor=ifelse(track_name=="Love Story","#009E73",barcolor))%>%
  mutate(date = as.Date(date,"%Y-%m-%d"))%>%
  group_by(artist,track_name)%>%
  mutate(cum_streams = cumsum(streams))%>%
  select(track_name,artist,date,cum_streams,streams,barcolor)%>%
  mutate(Value_lbl = paste0(" ",round(cum_streams/1000000,3)))%>%
  mutate(cum_streams = ifelse(track_name =="Love Story" & streams==0,1354784,cum_streams))%>%
  mutate(Value_lbl = ifelse(track_name =="Love Story" & streams==0,"1.355",Value_lbl))%>%
  arrange(track_name,date)

#Create plots based on data to enable gif creation

library(gganimate)
animated_charts<-
  ggplot(cumulative, aes(x = track_name,y = cum_streams,fill=barcolor)) +
  geom_col(width = 0.8,position= "identity") +
  coord_flip() +
  labs(title = 'Cumulative Folklore Streams in Millions Day by Day: {closest_state}',  
       subtile  = "Data Source: https://spotifycharts.com/",
       caption = "Animation by @fernanda_bruno_ | Code available on https://github.com/fernandabruno/swiftistics")+
  #theme_void() +
  geom_text(aes(label = Value_lbl, y = cum_streams),
            position = position_dodge(0.9), hjust = 0 ) +
  theme(axis.text.y = element_text(hjust = 1),
        axis.text.x=element_blank(),
        axis.title.x =element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        axis.ticks.x=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  transition_states(states = date, transition_length = 2, state_length = 1) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

#Glue plots together to make a gif
animate(animated_charts, nframes = 350,fps = 15,width = 800, height = 600,
        renderer = gifski_renderer("chart_animation.gif"))
  