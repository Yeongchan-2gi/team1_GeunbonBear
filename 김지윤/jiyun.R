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

## work_pop 전처리
work_pop <- read_excel('./취업인구.xlsx')
v <- c()
for (i in c(1, seq(4, 115, 3))) v <- c(v, work_pop[, i])
work_pop <- data.frame(v)
work_pop <- work_pop[-1, ]
work_pop <- work_pop[c(41:62), ]
str(work_pop)
m_work_pop<-melt(work_pop, id.vars=c('시점'))
m_work_pop$시점 <- as.numeric(m_work_pop$시점)
m_work_pop$value <- as.numeric(m_work_pop$value)
str(m_work_pop)
names(m_work_pop) <- c('year', 'country_work', 'work_pop')
act_pop <- read_excel('./활동인구.xlsx')
v <- c()
for (i in c(1, seq(4, 115, 3))) v <- c(v, act_pop[, i])
act_pop <- data.frame(v)
act_pop <- act_pop[-1, ]
act_pop <- act_pop[c(41:62), ]
act_pop
m_act_pop<-melt(act_pop, id.vars=c('시점'))
m_act_pop$시점 <- as.numeric(m_act_pop$시점)
m_act_pop$value <- as.numeric(m_act_pop$value)
str(m_act_pop)
names(m_act_pop) <- c('year', 'country_act', 'act_pop')
library(stringr)
v <- c()
for (i in m_act_pop$country_act) {
  j <- str_replace(i, '\\...', ' ')  # ... 제거 
  v <- c(v, strsplit(j, ' ')[[1]][1])  # 숫자 제거
}
m_act_pop$country_act <- v
head(m_act_pop,5)

v <- c()
for (i in m_work_pop$country_work) {
  j <- str_replace(i, '\\...', ' ')  # ... 제거 
  v <- c(v, strsplit(j, ' ')[[1]][1])  # 숫자 제거
}
m_work_pop$country_work <- v
head(m_work_pop,5)
df <- cbind(m_work_pop, act_pop =m_act_pop[3])
str(df)
df
m_df<-melt(df, id.vars=c('year', 'country_work'))
head(m_df,5)


library(gapminder)
library(ggplot2)
library(gganimate)
#install.packages('gifski')
library(gifski)
options(max.print = 10000)

#df$total_pop <- df$work_pop+df$act_pop
df
library(tidyverse)
library(viridis)

# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)
str(data)
str(m_df)
# Transform data in a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(m_df$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(m_df$group)*nObsType, ncol(m_df)) )
colnames(to_add) <- colnames(m_df)
to_add$group <- rep(levels(m_df$group), each=empty_bar*nObsType )
m_df <- rbind(m_df, to_add)
m_df <- m_df %>% arrange(group, individual)
m_df$id <- rep( seq(1, nrow(m_df)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- m_df %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- m_df %>% 
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
p <- ggplot(m_df) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(m_df=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(m_df=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(m_df=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(m_df=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(m_df=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(m_df$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(m_df=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(m_df=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(m_df=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p
# Save at png
ggsave(p, file="output.png", width=10, height=10)


data
m_df <- m_df[m_df$year==2021,]
m_df <- m_df[,-1]
head(m_df, 5)
head(data,5)
names(m_df) <- c('individual', 'observation', 'value')


asia_ <- c('대한민국', '이스라엘', '일본', '튀르키예')
norame_ <- c('캐나다','멕시코','미국')
souame_ <- c('칠레','콜롬비아','코스타리카')
eu_ <- c('오스트리아','벨기에','체코','덴마크','에스토니아','핀란드',
         '프랑스','독일','그리스','헝가리','아이슬란드','아일랜드',
         '이탈리아','라트비아','리투아니아','룩셈부르크','네덜란드',
         '노르웨이','폴란드','포르투갈','슬로바키아','슬로베니아',
         '스페인','스웨덴','스위스','영국')
oce_ <- c('오스트레일리아','뉴질랜드')

# 대륙에 해당 하는 나라 인덱스 받기

asia_idx <- c()
norame_idx <- c()
souame_idx <- c()
eu_idx <- c()
oce_idx <- c()

for (i in m_df$individual) {
  if (i %in% asia_) asia_idx <- c(asia_idx, which(m_df$individual == i))
  else if (i %in% norame_) norame_idx <- c(norame_idx, which(m_df$individual == i))
  else if (i %in% souame_) souame_idx <- c(souame_idx, which(m_df$individual == i))
  else if (i %in% eu_) eu_idx <- c(eu_idx, which(m_df$individual == i))
  else if (i %in% oce_) oce_idx <- c(oce_idx, which(m_df$individual == i))
} 

# 대륙에 해당 하는 나라 인덱스 중복 제거
library(dplyr)
asia_idx <- unique(asia_idx)
norame_idx <- unique(norame_idx)
souame_idx <- unique(souame_idx)
eu_idx <- unique(eu_idx)
oce_idx <- unique(oce_idx)

# 대륙 넣기

continent <- 1:dim(m_df)[1]
continent
continent[asia_idx] <- '아시아'
continent[norame_idx] <- '북아메리카'
continent[souame_idx] <- '남아메리카'
continent[eu_idx] <- '유럽'
continent[oce_idx] <- '오세아니아'

head(m_df, 5)
head(data,5)
names(m_df) <- c('individual', 'observation', 'value', 'group')
m_df$continent <- continent
m_df <- m_df[, -5]





library(tidyverse)
library(viridis)

# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)

# Transform data in a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 

# Set a number of 'empty bar' to add at the end of each group
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
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)


# Save at png
ggsave(p, file="output.png", width=10, height=10)
str(data)
str(m_df)
p
