# [ 9주차 팀프로젝트 ]  신민수

# 영아 출산률 - 영아 사망률
# 연도별 (1955~ 2100) 5년 단위
# 국가별 (OECD국가: 36개, 대륙별 데이터 전체 결측치)
# 각각 꺾은선 그래프
# 두 지표 상관관계 : 버블플랏
# --------------------------------------------------------------

# 1) 데이터 불러오기
library(readxl)
library(VIM)

data_birth <- read_excel("./project_data.xlsx", sheet = '합계출산율')
data_dead  <- read_excel("./project_data.xlsx", sheet = '영아사망률')
data_1p_gdp <- read_excel("./1인당국내총생산.xlsx")
# 2. 데이터셋 확인

# 2-1) 행,열 수 확인 : 30행 44열
dim(data_birth)
dim(data_dead)
dim(data_1p_gdp)

# 2-2) 데이터 설명
str(data_birth)
str(data_dead)

str(data_1p_gdp)

# 2-3) 결측치 확인 : 대륙별 통합 데이터 5열 전부 결측치
aggr(data_birth, prop = FALSE, numbers = TRUE)
aggr(data_dead, prop = FALSE, numbers = TRUE)
aggr(data_1p_gdp, prop = FALSE, numbers = TRUE)


data_birth <- data_birth[-c(2,7,11,15,42)]
data_dead <- data_dead[-c(2,7,11,15,42)]

# 3. 데이터 재구성 --------------------------------------------------
#install.packages('reshape')
library(reshape)

# melt
# 출산율합계
data_birth<-data.frame(data_birth) # df로 변환
data_birth <- data_birth[c(2:14),] # 20년도까지만 추출
data_birth <- melt(data_birth,id.vars=c('시점')) # 시점을 기준으로 각 국가별 데이터 재구성 ( 시점 / 국가 / 값 )
colnames(data_birth)[1]<-'year'
colnames(data_birth)[2]<-'country'
colnames(data_birth)[3]<-'birth_rate' # 컬럼 이름 변경
head(data_birth,100)


# 영아사망률
data_dead<-data.frame(data_dead) # df로 변환
data_dead <- data_dead[c(2:14),] # 20년도까지만 추출
data_dead <- melt(data_dead,id.vars=c('시점')) # 시점을 기준으로 각 국가별 데이터 재구성 ( 시점 / 국가 / 값 )
colnames(data_dead)[2]<-'country' # 컬럼 이름 변경
colnames(data_dead)[3]<-'dead_rate'
head(data_dead,150)

# 1인당 gdp
data_1p_gdp <-data.frame(data_1p_gdp) # df로 변환
#data_1p_gdp <- data_dead[c(1:15),] # 20년도까지만 추출
data_1p_gdp <- melt(data_1p_gdp,id.vars=c('시점')) # 시점을 기준으로 각 국가별 데이터 재구성 ( 시점 / 국가 / 값 )

colnames(data_1p_gdp)[2]<-'country' # 컬럼 이름 변경
colnames(data_1p_gdp)[3]<-'data_1p_gdp'
data_1p_gdp <- data_1p_gdp[data_1p_gdp$시점 %in% seq(1960,2020,5),]
data_1p_gdp
head(data_1p_gdp,150)
row.names(data_1p_gdp)<-NULL

# 데이터 통합------------------------------
data_birth[3]
data_dead[3]
data_1p_gdp[3]

data_bubble=cbind(data_birth,data_dead=data_dead[3],data_1p_gdp = data_1p_gdp[3])
dim(data_bubble)

# 대륙 컬럼 추가------------------------------
data_bubble
data_bubble$continent <-c('아시아')
data_bubble[c(53:91),6] <-c('북아메리카')
data_bubble[c(92:130),6] <-c('남아메리카')
data_bubble[c(131:468),6] <-c('유럽')
data_bubble[c(469:494),6] <-c('오세아니아')

str(data_bubble)
data_bubble$year <- as.integer(data_bubble$year)

write.csv(data_bubble,'mydf.csv')
# 시각화--------------------------------------------------------------
# 전체 국가 1화면
# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
#install.packages('gganimate')
library(gganimate)
str(data_bubble)

library(gifski)

# Make a ggplot, but add frame=year: one image per year

myplot<-ggplot(data_bubble, aes(birth_rate,dead_rate, size = data_1p_gdp, color=continent))+
  geom_point()+
  #scale_x_log10() +
  theme_bw()+
  labs(title = 'Year: {frame_time}', x='birth', y = 'dead')+
  transition_time(year) +
  ease_aes('linear')

animate(myplot, duration = 5, fps = 20, width = 500, height = 500, renderer = gifski_renderer())
#anim_save("output.gif")

#------------------------------------------------------------------------
# 대륙별 5개
# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)

# Make a ggplot, but add frame=year: one image per year
myplot2 <-ggplot(data_bubble, aes(birth_rate,dead_rate, size = data_1p_gdp, colour=continent)) +
  geom_point(alpha = 0.7) +
  #scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x='birth', y = 'dead') +
  transition_time(year) +
  ease_aes('linear')

animate(myplot2, duration = 5, fps = 20, width = 500, height = 500, renderer = gifski_renderer())

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate2.gif")



#---------------------------------------------------------------
# shiny

library(shiny)
library(ggplot2)
library(gapminder)

ui <- fluidPage(
  plotOutput("plot", click = "plot_click"),
  tableOutput("data")
)



server<-function(input,output){
  output$plot <-renderplot({
    myplot2 <-ggplot(data_bubble, aes(birth_rate,dead_rate, size = data_1p_gdp, colour=continent)) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(2, 12)) +
      scale_x_log10() +
      facet_wrap(~continent) +
      
      # Here comes the gganimate specific bits
      labs(title = 'Year: {frame_time}', x='birth', y = 'dead') +
      transition_time(year) +
      ease_aes('linear')
    
    animate(myplot2, duration = 5, fps = 20, width = 500, height = 500, renderer = gifski_renderer())
  })
  
  output$data <- renderTable({
    req(input$plot_click)
    nearPoints(data_bubble, input$plot_click)
  })
}




# # bar_race: 캐글 따라하기
# #stable version of the package can be installed using install.packages(“gganimate”) 
# #latest development version can be obtained by devtools::install_github(“thomasp85/gganimate”).
# #loading required libraries

# install.packages('ggthemes')
# install.packages('gifski')

# library(tidyverse)
# library(reshape2)
# library(ggthemes)
# library(gganimate)
# library(gifski)

# #loading dataset
# suicide_data<-read.csv("./ARTICLES/suicide rates/master.csv",header = TRUE,stringsAsFactors = FALSE)
# #selecting columns to work with
# suicide_sub<-suicide_data %>% select("ï..country","year" ,"sex","suicides_no")  %>% rename("country"="ï..country" )
# #function to sum the total suicide per country
# n<-unique(suicide_sub$country)
# country<-function(x){
#   suicide2<-suicide_sub %>% filter(country==x)
#   sum(suicide2$suicides_no)
# }
# #return a list with all total deaths per country
# country_total<-sapply(n,function(x) country(x))
# #creating a dataframe with top 10 total suicides per country
# df<-do.call(rbind,Map(data.frame,Country=n,Total_Suicides=country_total))
# df2<-df %>% arrange(desc(Total_Suicides))
# df3<-head(df2,n=10)
# write.csv(df3,"./ARTICLES/suicide rates/total_suicide.csv")
# #plotting the top 10 countries leading in the total suicide rates
# ggplot(df3,aes(reorder(Country,Total_Suicides),Total_Suicides,fill=as.factor(Country)))+
#     geom_col()+
#     coord_flip(clip = "off", expand = FALSE)+
#     guides( fill = FALSE) +
#     labs(title="TOTAL SUICIDE DEATHS PER COUNTRY FROM 1985-2016", 
#          y="Total Suicides Per Country", x="Country")+
#     scale_y_continuous(labels = scales::comma) +
#      geom_text(aes(label = paste(Total_Suicides,"")), hjust = 1)


# data <- read.csv('./master.csv')
# data
