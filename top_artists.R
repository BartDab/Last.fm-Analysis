library(tidyverse)
library(janitor)
library(data.table)
library(gganimate)

data<-read.csv("E:/R/iamtomahawk.csv",header=FALSE)
names(data)<-c('Artist','Album','Title','Date')
df<-data.frame(data$Artist,data$Date)
names(df)<-c('Artist','Datetime')
df <- separate(df,Datetime,
                 into=c("Day","Month","Year","Time"),
                 sep=" ")
df$Month<-match(df$Month,month.abb)


df<-unite(df,Date,into=c("Year","Month","Day"),sep="-")
#df<-unite(df,Datetime,into=c('Date','Time'),sep=' ')
df$Date<-as.Date(df$Date)
df2<-df[1:2]
str(df2)
help(clean_names)

#df3<-data.frame(unique(df2$Artist),
df3<-df2 %>%
  mutate(Scrobbles=1)

df35<-df3%>%
  complete(Artist,Date)
set.seed(2137)
df35[is.na(df35)]<-0#runif(1,0,0.0000000000001)

df4<-df35 %>%
  arrange(Artist,Date)%>%
  group_by(Artist)%>%
  mutate(Cumsum=cumsum(Scrobbles)+runif(1,0,0.0000000000001),
         Cumsum_formatted=formatC(Cumsum,0,format="f"))#ave(Scrobbles,c('Artist','Date'),FUN=cumsum))
#df4<-order(df4,as.vector(df4[,4]),decreasing=TRUE)
#  mutate(SumScrobbles = cumsum(Scrobbles))
df5<-df4%>%
  group_by(Date)%>%
  mutate(Rank=rank(-Cumsum))%>%
  #mutate(Cumsum_rel = Cumsum/Cumsum[Rank==1])%>%
  group_by(Artist)%>%
  filter(Rank<=10)%>%
  ungroup()

  staticplot = ggplot(df5, aes(Rank, group = Artist, 
                                       fill = as.factor(Artist), color = as.factor(Artist)))+
  geom_tile(aes(y = Cumsum/2,
                height = Cumsum,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Artist, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Cumsum,label = Cumsum_formatted, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
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
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))
  
  
  animated = staticplot + transition_states(Date, transition_length = 4, state_length = 1) +
    view_follow(fixed_x = TRUE)  +
    labs(title = 'My top artists on last.fm: {closest_state}',  
         subtitle  =  "Top 10",
         caption  = "Scrobbling since 9th March 2018")
  
  animate(animated, 1400, fps = 20,  width = 1200, height = 1000, 
          renderer = ffmpeg_renderer()) -> for_mp4
  anim_save("top_artists.mp4", animation = for_mp4 )
  