library(readxl)
pop <- read_excel('./project_data.xlsx', sheet='장래인구')
pop <- as.data.frame(pop)
str(pop)
dim(pop)
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(viridis)
library(plotly)
library(tidyverse)
library(gganimate)
library(nord)
library(viridis)
library(gapminder)
library(av)
library(reshape2)
library(esquisse)

esquisser()
ggplot(m_pop, aes(x=시점, y=value, group=variable, color=variable))+
  geom_line()+
  transition_states(m_pop$시점,transition_length=1.5,state_length=0.5)

options(max.print = 10000)
pop
pop_1 <- subset(pop, select=-c(아시아, 북아메리카, 유럽, 남아메리카, 오세아니아))
str(pop_1)
is.na(pop_1)
pop_1[1:10,2] <- replace(pop_1[1:10,2], pop_1[1:10,2]=='-', 25012)
pop_1[119:152,2] <- replace(pop_1[119:152,2], pop_1[119:152,2]=='-', 39294)
sum(is.na(pop_1))
pop_1 <- pop_1[1:151,]

m_pop <- melt(pop_1, id.vars=c('시점'))
colnames(m)[2] <- 'country'
str(m_pop)

don <- m_pop %>% 
  filter(variable %in% c("대한민국", "이스라엘", "일본", '튀르키예'))
don$시점 <- as.numeric(don$시점)
don$value <- as.numeric(don$value)
ggesm <- 
  ggplot(don, aes(x=시점, y=value, group=variable, color=variable))+
  geom_line()+
  geom_point()+
  scale_color_discrete_sequential('Sunset')+
  theme_minimal()+
  theme(legend.position='bottom')+
  transition_states(시점,transition_length=1.5,state_length=0.5)
ggesm
don$value

str(don)
library(transformr)
