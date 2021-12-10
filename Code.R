library(tidyverse)
library(readxl)
library(ggridges)
library(ggtext)
read_excel("Google search trends.xlsx")->data
data%>%
  type.convert()->data

colnames(data)<-c("Week","Indian Premier League","CoWIN","ICC T20 World Cup","Euro Cup","Tokyo Olympics","COVID Vaccine",
                  "Free Fire Redeem Code","Copa America","Neeraj Chopra","Aryan Khan")

data
data%>%
  gather("Trend","Value",2:11)%>%
  mutate(Week=as.Date(Week))->data

data%>%
  mutate(Trend=fct_relevel(Trend,"Aryan Khan","Neeraj Chopra","Copa America","Free Fire Redeem Code","COVID Vaccine","Tokyo Olympics",
                           "Euro Cup", "ICC T20 World Cup","CoWIN", "Indian Premier League"))->data



ggplot(data, aes(x = Week, y=Trend, height=Value, fill = Trend)) +
  geom_density_ridges(show.legend = FALSE, alpha=0.7,stat = "identity", scale = 5)+
  scale_fill_brewer(palette="Spectral",direction=-1)+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(colour="white",face = "bold"),
        axis.line=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
        strip.background = element_rect(fill="gray10"),
        strip.text = element_text(colour="white",size=10,face="bold",margin = margin(t=5,b=5)),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(size=14,face="bold",color="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=21)),
        plot.caption = element_markdown(size=10, colour="white",hjust=0,margin=margin(t=25)))+  
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="WHAT WAS TRENDING IN INDIA IN 2021?",
       subtitle = str_wrap("The below visualization shows the trend interest over time in 2021, on Google, for the top 10 topics that were searched the most, on web",100),
       caption="<span style='color:white'>Numbers represent search interest relative to the highest point on the chart for the given region and time.<br> A value of 100 is the peak popularity for the term. A value of 50 means that the term is half as popular.<br> A score of 0 means there was not enough data for this term.<br> <br>Data: Google Trends| Design and Analysis: @annapurani93</span>")->plot1

ggsave("searchtrends1.png",plot1,width = 8,height=8)
