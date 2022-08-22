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




data
m_df <- m_df[m_df$year==2021,]
m_df <- m_df[,-1]
head(m_df, 5)

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
m_df$continent <- continent
names(m_df) <- c('individual', 'observation', 'value', 'group')

str(m_df)




library(tidyverse)
library(viridis)

