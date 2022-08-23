library(readxl)
library(VIM)
library(hrbrthemes)
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
library(stringr)

## 취업인구 데이터 전처리
work_pop <- read_excel('./취업인구.xlsx')
v <- c()
for (i in c(1, seq(4, 115, 3))) v <- c(v, work_pop[, i])
work_pop <- data.frame(v)
work_pop <- work_pop[-1, ]
work_pop <- work_pop[c(41:62), ]
m_work_pop<-melt(work_pop, id.vars=c('시점'))
m_work_pop$시점 <- as.numeric(m_work_pop$시점)
m_work_pop$value <- as.numeric(m_work_pop$value)
names(m_work_pop) <- c('year', 'country_work', 'work_pop')
m_work_pop
v <- c()
for (i in m_work_pop$country_work) {
  j <- str_replace(i, '\\...', ' ')  # ... 제거 
  v <- c(v, strsplit(j, ' ')[[1]][1])  # 숫자 제거
}
m_work_pop$country_work <- v

## 활동인구 데이터 전처리
act_pop <- read_excel('./활동인구.xlsx')
v <- c()
for (i in c(1, seq(4, 115, 3))) v <- c(v, act_pop[, i])
act_pop <- data.frame(v)
act_pop <- act_pop[-1, ]
act_pop <- act_pop[c(41:62), ]
m_act_pop<-melt(act_pop, id.vars=c('시점'))
m_act_pop$시점 <- as.numeric(m_act_pop$시점)
m_act_pop$value <- as.numeric(m_act_pop$value)
names(m_act_pop) <- c('year', 'country_act', 'act_pop')
v <- c()
for (i in m_act_pop$country_act) {
  j <- str_replace(i, '\\...', ' ')  # ... 제거 
  v <- c(v, strsplit(j, ' ')[[1]][1])  # 숫자 제거
}
m_act_pop$country_act <- v

## m_df 만들기
df <- cbind(m_work_pop, act_pop =m_act_pop[3])

m_df<-melt(df, id.vars=c('year', 'country_work'))
df


df$continent <- ifelse(df$country_work %in% c('대한민국', '이스라엘', '일본', '튀르키예'),"아시아",
       ifelse(df$country_work %in% c('캐나다','멕시코','미국'), '북아메리카', 
              ifelse(df$country_work %in% c('칠레','콜롬비아','코스타리카'), '남아메리카',
                     ifelse(df$country_work %in% c('오스트레일리아','뉴질랜드'), '오세아니아', '유럽')))) 
asia_ <- c('대한민국', '이스라엘', '일본', '튀르키예')
norame_ <- c('캐나다','멕시코','미국')
souame_ <- c('칠레','콜롬비아','코스타리카')
eu_ <- c('오스트리아','벨기에','체코','덴마크','에스토니아','핀란드',
         '프랑스','독일','그리스','헝가리','아이슬란드','아일랜드',
         '이탈리아','라트비아','리투아니아','룩셈부르크','네덜란드',
         '노르웨이','폴란드','포르투갈','슬로바키아','슬로베니아',
         '스페인','스웨덴','스위스','영국')
oce_ <- c('오스트레일리아','뉴질랜드')



names(m_df) <- c('individual', 'observation', 'value', 'group')

head(m_df, 5)






head(data)
str(data)
str(m_df)
m_df$value <- as.integer(m_df$value)

data <- m_df
str(data)

write.csv(data,"data.csv")
## 그래프1
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-100000,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar()+
  geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  
  # Ad labels on top of each bar
  #geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  #geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  #geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
p

library(gganimate)
library(gifski)
gdp <- read_excel('./1인당국내총생산.xlsx')
melt(gdp, id.vars=c('year', 'country_work'))

gdp <-data.frame(gdp) # df로 변환
#data_1p_gdp <- data_dead[c(1:15),] # 20년도까지만 추출
gdp <- melt(gdp,id.vars=c('시점'))
gdp_result <- gdp[gdp$시점 %in% seq(1960,2020,5),]
dim(gdp_result)
m_act_pop_result <- m_act_pop[c(1:835), ]
m_work_pop_result <- m_work_pop[c(1:835), ]
m_work_pop_result
m_act_pop$continent <- continent
m_work_pop$continent <- continent
head(m_work_pop)
result=cbind(m_act_pop_result,m_work_pop_result=m_work_pop_result[3])
head(result)
head(df)

#그래프2
myplot<-ggplot(df, aes(act_pop,work_pop, size=10,color=continent))+
  geom_point()+
  scale_x_log10() +
  theme_bw()+
  labs(title = 'Year: {frame_time}', x='활동가능인구', y = '취업인구')+
  transition_time(year) +
  ease_aes('linear')
animate(myplot, duration = 5, fps = 20, width = 500, height = 500, renderer = gifski_renderer())
anim_save('bubble.gif')

# 대륙별
# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)

# 그래프3_그래프2를 대륙별로 나눈 그래프
ggplot(df, aes(act_pop,work_pop, colour=continent)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  #scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = '활동가능인구', y = '취업인구') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate2.gif")

library(tidyverse)
library(reshape2)
library(ggthemes)
library(gganimate)
library(gifski)
#install.packages('ggthemes')
life_ex <- read.csv('./life_ex.csv')
life_ex_plotly <- read_excel('./기대수명.xlsx')

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
anim_save('final_animation.gif')
#rendering the animation for mp4
animate(plt,100,fps = 20,duration = 30, width = 950, height = 750, renderer = ffmpeg_renderer())


life_ex_1

plotly_result <- life_ex_1 %>% filter(TIME%in%c(2005:2016))
plotly_gdp <- plotly_gdp %>% filter(TIME%in%c(2005:2016))
plotly_result
plotly_gdp <- read.csv('./gdp.csv')
plotly_result$gdp <- plotly_gdp$Value

options(max.print = 10000)
str(gdp)
gdp_plotly <- gdp %>% filter(시점%in%c(2000:2020))
df_plotly <- df %>% filter(year%in%c(2000:2020))
str(df_plotly)
str(gdp_plotly)
df_plotly$gdp <- gdp_plotly$value


df_plotly_1 <- df_plotly %>% filter(year%in%c(2020))
#----------------------------------------------------------------------------------------------
life_ex_plotly <- read_excel('./기대수명.xlsx')
v <- c()
for (i in c(1, seq(4, 115, 2))) v <- c(v, life_ex_plotly[, i])
life_ex_plotly <- data.frame(v)
life_ex_plotly <- life_ex_plotly[-1, ]
life_ex_plotly <- life_ex_plotly[c(41:62), ]
m_life_ex_plotly<-melt(work_pop, id.vars=c('시점'))
m_life_ex_plotly$시점 <- as.numeric(m_life_ex_plotly$시점)
m_life_ex_plotly$value <- as.numeric(m_life_ex_plotly$value)
names(m_life_ex_plotly) <- c('year', 'country', 'life_exp')
m_life_ex_plotly
v <- c()
for (i in m_life_ex_plotly$country) {
  j <- str_replace(i, '\\...', ' ')  # ... 제거 
  v <- c(v, strsplit(j, ' ')[[1]][1])  # 숫자 제거
}
m_life_ex_plotly$country <- v
df_plotly_1

head(m_life_ex_plotly)

# 열 추출
v <- c()
for (i in c(1, seq(2, 130, 3))) v <- c(v, life_ex_plotly[, i])
v
# 프레임화
life_ex_plotly <- data.frame(v)
life_ex_plotly <- life_ex_plotly[-1, ]
head(life_ex_plotly, 10)

# 컬럼명 처리
library(stringr)

v <- c()
for (i in colnames(life_ex_plotly)) {
  j <- str_replace(i, '\\...', ' ')  # ... 제거 
  v <- c(v, strsplit(j, ' ')[[1]][1])  # 숫자 제거
}

colnames(life_ex_plotly) <- v
colnames(life_ex_plotly)
head(life_ex_plotly)
df_plotly_1
write.csv(df_plotly_1, 'df_plotly_1.csv')
#대륙 처리
rem <- c('아시아', '북아메리카', '남아메리카', '유럽', '오세아니아')

u <- c()
for (i in colnames(life_ex_plotly)) {
  for (j in rem) {
    if (i==j) u <- c(u, which(colnames(life_ex_plotly) == i))  # 대륙 인덱스 찾기
  }
}

life_ex_plotly <- life_ex_plotly[, -u]
#
# life_ex_plotly$continent <- ifelse(life_ex_plotly$country %in% c('대한민국', '이스라엘', '일본', '튀르키예'),"아시아",
#        ifelse(life_ex_plotly$countryk %in% c('캐나다','멕시코','미국'), '북아메리카', 
#               ifelse(life_ex_plotly$country %in% c('칠레','콜롬비아','코스타리카'), '남아메리카',
#                      ifelse(life_ex_plotly$country %in% c('오스트레일리아','뉴질랜드'), '오세아니아', '유럽')))) 
# # 인덱스를 정리, 형 변환
rownames(life_ex_plotly) <- 1:dim(life_ex_plotly)[1]

life_ex_plotly$시점 <- factor(life_ex_plotly$시점)
for (i in colnames(life_ex_plotly)[2:dim(life_ex_plotly)[2]]) {
  life_ex_plotly[[i]] <- as.numeric(life_ex_plotly[[i]])
}


t <- c()

for (i in colnames(life_ex_plotly)) {
  t <- c(t, life_ex_plotly[[i]])
}
t
df_plotly_1$life_exp <- t[2:length(t)]
df_plotly_1
write.csv(df_plotly_1, 'df_plotly_1.csv')

##plotly
df_plotly_1 %>% 
  filter(year == 2020) %>% 
  plot_ly(x = ~gdp, y = ~life_exp, color = ~continent) %>% 
  add_markers(colors = "Dark2")

life_exp
write.csv(life_exp,"life_exp.csv")

gap_loess <- loess(life_exp ~ gdp, data = df_plotly_1)
gap_lm    <- lm(life_exp ~ gdp, data = df_plotly_1)

df_plotly_1 %>% 
  plot_ly(x = ~gdp, y = ~life_exp, hoverinfo = "text",
          text = ~paste0("<b>", "OECD_POP", "</b> <br>",
                         "인당 GDP: ", gdp, "<br>",
                         "기대수명: ", life_exp)) %>% 
  add_markers(colors = c("orange", "darkgray", "skyblue", "black", "red"), showlegend = FALSE) %>% 
  add_lines(y = ~fitted(gap_lm), name = "회귀선") %>% 
  add_lines(y = ~fitted(gap_loess), name = "LOESS")
