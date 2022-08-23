# OECD 국가별 도시화율 --------------------------------------------1
library(readxl)
library(tibble)
#1950~2050 by=1
urban <- read_excel('./project_data.xlsx', sheet = '도시화율') # tibble
urban
urban <- urban[,-c(2, 7, 11, 15, 43)]
urbanDF <- data.frame(urban)
names(urbanDF)

plot(urban)
names(urban)
# [1] "시점"           "아시아"2         "대한민국"       "이스라엘"      
# [5] "일본"           "튀르키예"       "북아메리카"7     "캐나다"        
# [9] "멕시코"         "미국"           "남아메리카"11     "칠레"          
# [13] "콜롬비아"       "코스타리카"     "유럽"15           "오스트리아"    
# [17] "벨기에"         "체코"           "덴마크"         "에스토니아"    
# [21] "핀란드"         "프랑스"         "독일"           "그리스"        
# [25] "헝가리"         "아이슬란드"     "아일랜드"       "이탈리아"      
# [29] "라트비아"       "리히텐슈타인"   "리투아니아"     "룩셈부르크"    
# [33] "네덜란드"       "노르웨이"       "폴란드"         "포르투갈"      
# [37] "슬로바키아"     "슬로베니아"     "스페인"         "스웨덴"        
# [41] "스위스"         "영국"           "오세아니아"43     "오스트레일리아"
# [45] "뉴질랜드"
str(urban)

# Libraries
library(ggplot2)
library(hrbrthemes)
str(urban$대한민국)
str(urban$남아메리카)
urban$칠레
urban <- urban[,-c(2, 7, 11, 15, 43)]
str(urban)
ncol(urban)
names(urban)
seq(1950, 2050, 1)
cumsum(rnorm(10))
# create data
xValue <- seq(1950, 2050, 1)
# for (variable in vector) {
#   
# }
# for (i in 1:40) {
#   yValue_i <- urban[,i+1]
# }
class(urban[,2])

yValue_1 <- urbanDF[,2]
yValue_2 <- urbanDF[,3]
yValue_3 <- urbanDF[,4]
yValue_4 <- urbanDF[,5]
yValue_5 <- urbanDF[,6]
yValue_6 <- urbanDF[,7]
yValue_7 <- urbanDF[,8]
yValue_8 <- urbanDF[,9]
yValue_9 <- urbanDF[,10]
yValue_10 <- urbanDF[,11]
yValue_11 <- urbanDF[,12]
yValue_12 <- urbanDF[,13]
yValue_13 <- urbanDF[,14]
yValue_14 <- urbanDF[,15]
yValue_15 <- urbanDF[,16]
yValue_16 <- urbanDF[,17]
yValue_17 <- urbanDF[,18]
yValue_18 <- urbanDF[,19]
yValue_19 <- urbanDF[,20]
yValue_20 <- urbanDF[,21]
yValue_21 <- urbanDF[,22]
yValue_22 <- urbanDF[,23]
yValue_23 <- urbanDF[,24]
yValue_24 <- urbanDF[,25]
yValue_25 <- urbanDF[,26]
yValue_26 <- urbanDF[,27]
yValue_27 <- urbanDF[,28]
yValue_28 <- urbanDF[,29]
yValue_29 <- urbanDF[,30]
yValue_30 <- urbanDF[,31]
yValue_31 <- urbanDF[,32]
yValue_32 <- urbanDF[,33]
yValue_33 <- urbanDF[,34]
yValue_34 <- urbanDF[,35]
yValue_35 <- urbanDF[,36]
yValue_36 <- urbanDF[,37]
yValue_37 <- urbanDF[,38]
yValue_38 <- urbanDF[,39]
yValue_39 <- urbanDF[,40]



# data <- data.frame(xValue,yValue)
colors()
# Plot
?ggplot()
ggplot(urban) +
  geom_line(aes(x=xValue, y=yValue_1), color=2, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_2), color=2, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_3), color=2, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_4), color=2, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_5), color=3, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_6), color=3, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_7), color=3, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_8), color=4, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_9), color=4, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_10), color=4, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_11), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_12), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_13), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_14), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_15), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_16), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_17), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_18), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_19), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_20), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_21), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_22), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_23), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_24), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_25), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_26), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_27), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_28), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_29), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_30), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_31), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_32), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_33), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_34), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_35), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_36), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_37), color=5, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_38), color=6, size=1, alpha=0.9, linetype=3) +
  geom_line(aes(x=xValue, y=yValue_39), color=6, size=1, alpha=0.9, linetype=3) +
  theme_ipsum(grid = F, axis = F) +
  ggtitle("Urbanization rate")
?theme_ipsum
# 벨기에 국토면적 ----------------------------------------------------2-
# 2000~2019
areaB <- read_excel('./project_data.xlsx', sheet = '국토면적') # tibble
str(areaB)
# areaB[92:97] <- na.omit(areaB[92:97])
seq(2000, 2019, 1)
areaB_DF<- data.frame(areaB)
str(areaB_DF)
areaB_DF[c(1, 92:97)]
areaB_DF_1 <- na.omit(areaB_DF[c(1, 92:97)])
areaB_DF_1
areaB_DF_1[c(2:21),]
plot(areaB_DF_1)
names(areaB_DF_1[c(2:21),]) <- c("시점","총면적","육지면적","농경지","농경지(경지)",
                                 "농경지(과수원)", "비농경지")

nrow(areaB_DF_1[c(1:21),])

# 그래프 샘플--------------------------------------------
library(plotly)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- txhousing 
fig <- df %>%
  filter(year > 2005, city %in% c("Abilene", "Bay Area"))
fig <- fig %>% accumulate_by(~date)


fig <- fig %>%
  plot_ly(
    x = ~date, 
    y = ~median,
    split = ~city,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  )
fig <- fig %>% layout(
  xaxis = list(
    title = "Date",
    zeroline = F
  ),
  yaxis = list(
    title = "Median",
    zeroline = F
  )
) 
fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  hide = T
)
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)

fig
# --------------------------------------------



# 애니메이션 선형 그래프----
library(plotly)
names(txhousing)
summary(txhousing$median)
class(txhousing$median)
# [1] "city"      "year"      "month"     "sales"     "volume"    "median"   
# [7] "listings"  "inventory" "date"     
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


# 벨기에
summary(areaB_DF_1)
areaB_DF_1 
as.numeric(areaB_DF_1)
colnames(areaB_DF_1) <- c('시점', '총면적', '육지면적',
                          '농경지', '농경지(경지)',
                          '농경지(과수원)', '비농경지')
areaB_DF_2 <- areaB_DF_1[-1,]
summary(areaB_DF_2)
areaB_DF_2[,1:7]
areaB_DF_2[,1] <- as.numeric(areaB_DF_2[,1])
areaB_DF_2[,2] <- as.numeric(areaB_DF_2[,2])
areaB_DF_2[,3] <- as.numeric(areaB_DF_2[,3])
areaB_DF_2[,4] <- as.numeric(areaB_DF_2[,4])
areaB_DF_2[,5] <- as.numeric(areaB_DF_2[,5])
areaB_DF_2[,6] <- as.numeric(areaB_DF_2[,6])
areaB_DF_2[,7] <- as.numeric(areaB_DF_2[,7])

plot(areaB_DF_2[,1])
plot(areaB_DF_2[,2])
plot(areaB_DF_2[,3])
plot(areaB_DF_2[,4])
plot(areaB_DF_2[,5])
plot(areaB_DF_2[,6])
plot(areaB_DF_2[,7])

str(areaB_DF_2)
names(areaB_DF_2)
as.numeric(areaB_DF_2[,1:7])
class(areaB_DF_2[,2])

?plotly
df <- areaB_DF_2 
fig <- df
# fig <- fig %>% accumulate_by(~seq(2000, 2019, 1))

fig <- fig %>%
  plot_ly(
    x = ~seq(2000, 2019, 1), 
    y = ~시점,
    split = ~c('총면적', '육지면적', '농경지',
               '농경지(경지)', '농경지(과수원)', '비농경지'),
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  )
# x,y축----
fig <- fig %>% layout(
  xaxis = list(
    title = "Year",
    zeroline = F
  ),
  yaxis = list(
    title = "Area",
    zeroline = F
  )
)
# 애니메이션 속도 -----
fig <- fig %>% animation_opts(
  frame = 50, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  hide = T
)
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)

fig

areaB_DF_2
# (면적)산점도 -------------
library(plotly)

fig <- plot_ly(data = areaB_DF_2, x = ~areaB_DF_2[,4], y = ~areaB_DF_2[,5])

fig
# (면적)산점도(plotting markes)
library(plotly)

trace_0 <- areaB_DF_2[,2]
trace_1 <- areaB_DF_2[,3]
trace_2 <- areaB_DF_2[,4]
trace_3 <- areaB_DF_2[,5]
trace_4 <- areaB_DF_2[,6]
trace_5 <- areaB_DF_2[,7]
x <- seq(2000, 2019, 1)

data <- data.frame(x, trace_0, trace_1, trace_2, trace_3, trace_4, trace_5)

fig <- plot_ly(data, x = ~x)
fig <- fig %>% add_trace(y = ~trace_0, name = '총면적',mode = 'lines+markers',
                         marker = list(symbol = 1 , 
                                       color = 'rgb(17, 157, 255)',size = 12))
fig <- fig %>% add_trace(y = ~trace_1, name = '육지면적', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~trace_2, name = '농경지', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~trace_3, name = '농경지(경지)', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~trace_4, name = '농경지(과수원)', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~trace_5, name = '비농경지', mode = 'lines+markers')

fig

# 도시화율이 낮은 벨기에, 총면적에서 1. 육지면적, 2.비농경지, 3.농경지,
# 4. 농경지(경지), 5(농경지(과수원))
# 장래인구 -------------------------------------------------------
# 1950~2100
popfutureB <- read_excel('./project_data.xlsx', sheet = '장래인구') # tibble
popfutureB
popfutureB_DF <- data.frame(popfutureB)
popfutureB_DF[c('시점', '벨기에')]
summary(popfutureB_DF[c('시점', '벨기에')])
plot(popfutureB_DF[c('시점', '벨기에')])
plot(popfutureB_DF['벨기에'])
?theme_ipsum

library(plotly)

popfutureB_DF['시점']
popfutureB_DF['벨기에']
#     인터렉티브선형 그래프 ----
pop_Belgie_year <- seq(1950,2100,1)
pop_Belgie <- popfutureB_DF$벨기에
data <- popfutureB_DF
fig <- plot_ly(data, x = ~pop_Belgie_year, y = ~pop_Belgie,
               type = 'scatter', mode = 'lines')
layout(fig, title='future population of Belgie')
fig
?plot_ly
?layout
# 주요도시인구 ---------------------------------------------------

mjctpopB <- read_excel('./project_data.xlsx', sheet = '주요도시인구') # tibble
mjctpopB
mjctpopB_DF <- data.frame(mjctpopB)
mjctpopB_DF['시점']
summary(mjctpopB_DF['벨기에.브뤼셀.'])
# 1950~2035

mjctpopB_year <- seq(1950,2035,1)
mjctpopB_Belgie <- mjpopB_DF$벨기에.브뤼셀.
data <- mjctpopB_DF
fig <- plot_ly(data, x = ~mjctpopB_year, y = ~mjctpopB_Belgie,
               type = 'scatter', mode = 'lines')
layout(fig, title='city population of Belgie')
fig

# 분석 결과 :  지속적인 우상향 그래프

# 기대수명 -------------------------------------------------------

expectLifeB <- read_excel('./project_data.xlsx', sheet = '기대수명') # tibble
expectLifeB['시점']
expectLifeB_DF <- data.frame(expectLifeB)
summary(expectLifeB_DF)
nrow(expectLifeB_DF)
summary(expectLifeB_DF[47:49])
expectLifeB_DF_1 <- expectLifeB_DF[,c(1,47:49)]
expectLifeB_DF_1
colnames(expectLifeB_DF_1) <- c('시점', '합계', '남자', '여자')
expectLifeB_DF_2 <- expectLifeB_DF_1[c(2:31),]
expectLifeB_DF_2[3:4]
expectLifeB_DF_2[,3] <- as.numeric(expectLifeB_DF_2[,3])
expectLifeB_DF_2[,4] <- as.numeric(expectLifeB_DF_2[,4])
class(expectLifeB_DF_2[,3])
fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = ~expectLifeB_DF_2[3])
fig <- fig %>% add_histogram(x = ~expectLifeB_DF_2[4] + 1)
fig <- fig %>% layout(barmode = "overlay")
expectLifeB_DF_2[1]
fig
# 기대수명(남여)막대그래프 ----------------------------------------
library(plotly)
seq(1955,2100,5)
x <- seq(1955,2100,5)
y1 <- expectLifeB_DF_2[,3]
y2 <- expectLifeB_DF_2[,4]
data <- data.frame(x, y1, y2)

#The default order will be alphabetized unless specified as below:
data$x <- factor(data$x, levels = data[["x"]])

fig <- plot_ly(data, x = ~x, y = ~y1, type = 'bar',
               name = 'male',
               marker = list(color = 'rgb(49,130,189)')
               )
fig <- fig %>% add_trace(y = ~y2, name = 'female', marker = list(color = 'rgb(204,204,204)'))
fig <- fig %>% layout(xaxis = list(title = "", tickangle = -45),
                      yaxis = list(title = ""),
                      margin = list(b = 100),
                      barmode = 'group')
fig





# -----------------------------------------------------------------
library(plotly)
library(quantmod)
# install.packages('quantmod')
df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)
df$ID <- seq.int(nrow(df))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- df %>% accumulate_by(~ID)
fig <- df %>% plot_ly(
  x = ~ID, 
  y = ~AAPL.Close, 
  frame = ~frame,
  type = 'scatter', 
  mode = 'lines', 
  fill = 'tozeroy', 
  fillcolor='rgba(114, 186, 59, 0.5)',
  line = list(color = 'rgb(114, 186, 59)'),
  text = ~paste("Day: ", ID, "<br>Close: $", AAPL.Close), 
  hoverinfo = 'text'
)
fig <- fig %>% layout(
  title = "AAPL: Last 30 days",
  yaxis = list(
    title = "Close", 
    range = c(0,250), 
    zeroline = F,
    tickprefix = "$"
  ),
  xaxis = list(
    title = "Day", 
    range = c(0,30), 
    zeroline = F, 
    showgrid = F
  )
) 
fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  currentvalue = list(
    prefix = "Day "
  )
)

fig


# ----남여 구성비가 있는 그래프--------
library(plotly)

labels = c('male','female')
values = c(4500, 2500)

fig <- plot_ly(type='pie', labels=labels, values=values, 
               textinfo='label+percent',
               insidetextorientation='radial',
               hole=0.6)
# fig <- fig %>% add_pie(hole = 0.6)
fig

# 인구동태_밀도--------------------------------------------------
# 1998 ~ 2020
# popdenB<- read_excel('./project_data.xlsx', sheet = '인구동태_밀도')
# popdenB
# popdenB_DF <- data.frame(popdenB)
# dim(popdenB_DF)
# names(popdenB_DF)
# unique(popdenB_DF$시점)
# 
# popdenB_DF[c(1,102:108)]
# 


# 부양비_노령화지수 --------------------------------------------
# 1950~2100
supportingB <- read_excel('./project_data.xlsx', sheet = '부양비_노령화지수') # tibble
supportingB
supportingB_DF <- data.frame(supportingB)
supportingB_DF[c(1,122:129)]

# -----무슨 그래프--------

# 순이동률 ------------------------------------------------------
# 특정기간동안 전입자수에서 전출자수를 뺀 수치를 해당기간의 총인구로 나눈수치로, 1000분율로 나타냄
movementB <- read_excel('./project_data.xlsx', sheet = '순이동률') # tibble
movementB
movementB_DF <- data.frame(movementB)
movementB_DF[c('시점','벨기에')]
# lolipop차트(순이동률)----
# Library
library(tidyverse)

# Create data
data <- movementB_DF
movement_Belgie=movementB_DF$시점
year=movementB_DF$벨기에
# plot
ggplot(data, aes(x=movement_Belgie, y=year)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)

# 합계출산율 ------------------------------------------------------
#1955 ~ 2100
birthB  <- read_excel('./project_data.xlsx', sheet = '합계출산율')
birthB
birthB_DF <- data.frame(birthB)
birthB_DF
birthB_DF_B <- birthB_DF[c('시점', '벨기에')]
birthB_DF_B

birthB_year <- seq(1955,2100,5)
birth_Belgie <- birthB_DF$벨기에
data <- birthB_DF
fig <- plot_ly(data, x = ~birthB_year, y = ~birth_Belgie,
               type = 'scatter', mode = 'lines')
layout(fig, title='birth rate of Belgie')
fig

# 분석결과 :  1965년 출생률 피크


# 영아사망률 ------------------------------------------------------
infantB  <- read_excel('./project_data.xlsx', sheet = '영아사망률')
infantB
infantB_DF <- data.frame(infantB)

infantB_DF_B <- infantB_DF[c('시점','벨기에')]
infantB_DF_B

infantB_year <- seq(1955,2100,5)
infantB_Belgie <- infantB$벨기에
data <- infantB_DF
fig <- plot_ly(data, x = ~infantB_year, y = ~infantB_Belgie,
               type = 'scatter', mode = 'lines')
layout(fig, title='infant mortality of Belgie')
fig

BI_DF<- cbind(birthB_DF_B, infantB_DF_B[2])
colnames(BI_DF) <- c('시점', '합계출산율', '영아사망률')
BI_DF
# 분석결과 :  1955 피크
# why?
# 이중축그래프 ----------------
# Value used to transform the data
coeff <- 1/10

# A few constants
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)
year <- seq(1955,2100,5)
ggplot(BI_DF, aes(x=year)) +
  
  geom_line( aes(y=birthB_DF$벨기에/coeff), size=2, color=temperatureColor) + 
  geom_line( aes(y=infantB$벨기에), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature (Celsius °)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Price ($)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  
  ggtitle("Temperature down, price up")

# ------------------
# data |>
#   plot_ly(x= ~시점,
#           y= ~bill_depth_mm,
#           name=~species,
#           hovertext=~island,
#           hoverinfo='x+y+name+text')
str(urban)
urban[,c(2:40)]
names(urban[,c(2:40)])

class(urban$캐나다)

urban |>
  plot_ly() |>
  add_trace(x= ~시점,
            y= ~urban$대한민국,
            type = 'bar')|>
  layout(title = 'Bar Plot',
         xaxis = list(title = 'years'),
         yaxis = list(title= 'rate'))
class(urban$시점)
class(urban)
?geom_line()
str(urban)
# y= ~urban[,c(2:40)],

urban_1 <- as.numeric(urban[,c(2:40)])


as.numeric(urban[,2])


# 그래프 합치기 ------------------


# 애니메이션 플롯--------------------

# install.packages("png")
# install.packages("gif")
# dev.off()
# 
# library(viridis)
# # libraries:
# library(ggplot2)
# library(gganimate)
# library(babynames)
# library(hrbrthemes)
# library(gifski)
# library(png)
# library(gif)
# ??babynames
# str(babynames)
# # Keep only 3 names
# don <- babynames %>% 
#   filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
#   filter(sex=="F")
# 
# # Plot
# don %>%
#   ggplot( aes(x=year, y=n, group=name, color=name)) +
#   geom_line() +
#   geom_point() +
#   # scale_color_viridis(discrete = TRUE) +
#   ggtitle("Popularity of American names in the previous 30 years") +
#   theme_ipsum() +
#   ylab("Number of babies born")
#   # transition_reveal(year) +
#   # anim_save("287-smooth-animation-with-tweenr.gif")
# 
# p
# 
# # Save at gif:
# anim_save("287-smooth-animation-with-tweenr.gif")
# 
# # animate(p, duration = 5, fps = 20, width = 200, height = 200, renderer = gifski_renderer())
# # anim_save("output.gif")
# 
