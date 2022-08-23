life_ex <- read.csv('../기대수명.csv')

library(readxl)
library(gifski)
library(ggplot2)
library(gganimate)
library(tidyverse)
#plotting static plot
life_ex$Value <- as.integer(life_ex$Value)

life_ex_1 <- life_ex[, c('LOCATION', 'TIME', 'Value')]

life_ex_6 <- aggregate(Value~LOCATION+TIME, life_ex_1, sum)

life_ex_2<-life_ex_6 %>% group_by(TIME) %>% transmute(TIME, rank = rank(-Value))
life_ex_6$rank <- life_ex_2$rank
str(life_ex_6)
str(life_ex_2)
unique(life_ex_2$rank)

# 그래프4_기대수명 그래프
static_plot<-ggplot(life_ex_6,aes(x=Value, group=LOCATION,fill=as.factor(LOCATION),color=as.factor(LOCATION))) +
  geom_tile(aes(y = Value/2,
                height = Value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(LOCATION,'')), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Value,label = paste('',Value)), hjust=0)+
  coord_flip(clip = 'off', expand = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = 'none', fill = 'none') +
  theme_minimal() +
  theme(
    plot.title=element_text(size=25, hjust=0.5, face='bold', colour='grey', vjust=-1),
    plot.subtitle=element_text(size=18, hjust=0.5, face='italic', color='grey'),
    plot.caption =element_text(size=8, hjust=0.5, face='italic', color='grey'),
    axis.ticks.y = element_blank(), 
    axis.text.y = element_blank(), 
    plot.margin = margin(1,1,1,4, 'cm')
  )

#creating final animation
plt<-static_plot + transition_states(states =TIME, transition_length = 4, state_length = 1) + 
  ease_aes('cubic-in-out') +
  labs(title = 'Life expectancy at birth _ {closest_state}', 
       subtitle = 'Top 10 Countries',
       x='Countries',y='Life expectancy at birth')
#rendering the animation for gif
final_animation<-animate(plt,100,fps = 20,duration = 30, width = 950, height = 750, renderer = gifski_renderer())
final_animation

