###############
###Animation###
##22/08/2020 ##
###############

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
charts10 = charts %>% 
  group_by(date)%>%      
  mutate(rank = rank(-streams),
         Value_rel = streams/streams[rank==1],
         Value_lbl = paste0(" ",streams)) %>%
  group_by(track_name) %>%
  filter(rank <= 20)

#Create plots based on data to enable gif creation

library(gganimate)
animated_charts = ggplot(charts10, aes(rank, group = track_name,fill=barcolor))+
  geom_tile(aes(y = streams/2,
                height = streams,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(track_name, " ")), vjust = 0.2, hjust = 1, size = 7) + #determine size of the Nationlity label
  geom_text(aes(y=streams,label = Value_lbl, hjust=0),size = 8 ) +  #determine size of the value label
  coord_flip(clip = "off", expand = TRUE) +
  scale_fill_identity()+
  scale_x_reverse() +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold",     colour="black", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=15, hjust=0.5, face="italic", color="black"),
        plot.background=element_blank(),
        plot.margin = margin(1,4, 1, 8, "cm")) +
  transition_states(date, transition_length = 4, state_length = 1) +
  ease_aes('sine-in-out') +
  labs(title = 'Top 20 Spotify Charts After Folklore Release Day by Day: {closest_state}',  
       subtile  = "Data Source: https://spotifycharts.com/",
       caption = "Animation by @fernanda_bruno_ | Code available on https://github.com/fernandabruno/swiftistics")

#Glue plots together to make a gif

animate(animated_charts, nframes = 350,fps = 15,  width = 1200, height = 1000, 
        renderer = gifski_renderer("chart_animation.gif"))
