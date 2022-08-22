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

data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)

# Transform data in a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2))


str(data)
str(m_df)



head(data)
str(data)
str(m_df)
m_df$value <- as.integer(m_df$value)
str
data <- m_df
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
head(result,100)

head(df)
myplot<-ggplot(df, aes(act_pop,work_pop, size=10,color=continent))+
  geom_point()+
  scale_x_log10() +
  theme_bw()+
  labs(title = 'Year: {frame_time}', x='birth', y = 'dead')+
  transition_time(year) +
  ease_aes('linear')
animate(myplot, duration = 5, fps = 20, width = 500, height = 500, renderer = gifski_renderer())



# 대륙별
# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)

# Make a ggplot, but add frame=year: one image per year
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
life_ex <- read.csv('../life_ex.csv')

str(life_ex_2)
#plotting static plot
life_ex_1$Value <- as.integer(life_ex$Value)
life_ex_1 <- life_ex[, c('LOCATION', 'TIME', 'Value')]

life_ex_6 <- aggregate(Value~LOCATION+TIME, life_ex_1, sum)
life_ex_2<-life_ex_6 %>% group_by(TIME) %>% mutate(rank = row_number(-Value) * 1) %>%
  ungroup()

unique(life_ex_2$rank)
static_plot<-ggplot(life_ex_2,aes(x=Value, group=LOCATION,fill=as.factor(LOCATION),color=as.factor(LOCATION))) +
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
  #view_follow(fixed_x = TRUE) +
  labs(title = 'Total Suicides per Year : {closest_state}', 
       subtitle = 'Top 10 Countries',
       caption = 'Data Source: World Bank Data',
       x='Countries',y='Total Suicides per year')
#rendering the animation for gif
final_animation<-animate(plt,100,fps = 20,duration = 30, width = 950, height = 750, renderer = gifski_renderer())
final_animation
#rendering the animation for mp4
animate(plt,100,fps = 20,duration = 30, width = 950, height = 750, renderer = ffmpeg_renderer())
# -------------------------------------------------------------------------------
library(tidyverse)
library(reshape2)
library(ggthemes)
library(gganimate)
library(gifski)

suicide_data <- read.csv('./kaggle.csv', header = TRUE,stringsAsFactors = FALSE)
suicide_sub<-suicide_data %>% select("country","year" ,"sex","suicides_no")
#function to sum the total suicide per country
n<-unique(suicide_sub$country)
country<-function(x){
  suicide2<-suicide_sub %>% filter(country==x)
  sum(suicide2$suicides_no)
}
suicide2$suicides_no
#return a list with all total deaths per country
country_total<-sapply(n,function(x) country(x))
country_total
#creating a dataframe with top 10 total suicides per country
df<-do.call(rbind,Map(data.frame,Country=n,Total_Suicides=country_total))
df2<-df %>% arrange(desc(Total_Suicides))
df3<-head(df2,n=10)
df3
write.csv(df3,"./total_suicide.csv")
#plotting the top 10 countries leading in the total suicide rates
ggplot(df3,aes(reorder(Country,Total_Suicides),Total_Suicides,fill=as.factor(Country)))+
  geom_col()+
  coord_flip(clip = "off", expand = FALSE)+
  guides( fill = FALSE) +
  labs(title="TOTAL SUICIDE DEATHS PER COUNTRY FROM 1985-2016", 
       y="Total Suicides Per Country", x="Country")+
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = paste(Total_Suicides,"")), hjust = 1)
#subset initial data with top 10 countries
top_suicide<-suicide_sub%>%filter(country==c('Russian Federation','United States','Japan','France','Ukraine','Germany','Republic of Korea','Brazil','Polan','United Kingdom'))
top_suicide
#filtering years with consistent data
top_suicide2<-top_suicide %>% filter(year %in%c(1990:2014)) 
top_suicide2
top_suicide2$sex<-as.factor(top_suicide2$sex)
#summing the total male & female suicides per country for each year
sm3<-aggregate(suicides_no~country+year,top_suicide2,sum)
life_ex_1 <- aggregate(Value~LOCATION+TIME, life_ex, sum)
life_ex_1
#* 1 ensures we have non-integer ranks while sliding
sm4<-sm3 %>% group_by(year) %>% mutate(rank = min_rank(-suicides_no) * 1) %>%
  ungroup()
sm3
suicides_no
life_ex_1 <- life_ex[, c('LOCATION', 'TIME', 'Value')]
life_ex_1
life_ex_2<-life_ex_1 %>% group_by(TIME) %>% mutate(rank = min_rank(Value) * 1) %>%
  ungroup()


static_plot<-ggplot(sm4,aes(rank,group=country,fill=as.factor(country),color=as.factor(country))) +
  geom_tile(aes(y = suicides_no/2,
                height = suicides_no,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, '')), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=suicides_no,label = paste('',suicides_no)), hjust=0)+
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
