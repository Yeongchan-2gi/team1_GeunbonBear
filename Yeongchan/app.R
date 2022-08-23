library(shiny)
library(ggridges)
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(hrbrthemes)
library(plotly)
library(forcats)
library(reshape)

getwd()

# 도시화율 데이터
urban <- read_excel('./project_data.xlsx', sheet = '도시화율') # tibble
urban <- urban[,-c(2, 7, 11, 15, 43)]
urbanDF <- data.frame(urban)

# 기대수명 데이터
expectLifeB <- read_excel('./project_data_1.xlsx', sheet = '기대수명') # tibble
expectLifeB_DF <- data.frame(expectLifeB)
expectLifeB_DF_1 <- expectLifeB_DF[,c(1,47:49)]
colnames(expectLifeB_DF_1) <- c('시점', '합계', '남자', '여자')
expectLifeB_DF_2 <- expectLifeB_DF_1[c(2:31),]
expectLifeB_DF_2[3:4]
expectLifeB_DF_2[,3] <- as.numeric(expectLifeB_DF_2[,3])
expectLifeB_DF_2[,4] <- as.numeric(expectLifeB_DF_2[,4])

# 합계출산율 데이터
birthB  <- read_excel('./project_data.xlsx', sheet = '합계출산율')
birthB_DF <- data.frame(birthB)
birthB_DF_B <- birthB_DF[c('시점', '벨기에')]

# 영아사망률 데이터
infantB  <- read_excel('./project_data.xlsx', sheet = '영아사망률')
infantB_DF <- data.frame(infantB)

# 5_graph_데이터
lifeExp_B <- read_excel('./project_data_1.xlsx', sheet = '기대수명')
old_B<- read_excel('./project_data_1.xlsx', sheet = '부양비_노령화지수')
movement_B<- read_excel('./project_data_1.xlsx', sheet = '순이동률')
brith_B<- read_excel('./project_data_1.xlsx', sheet = '합계출산율')
infant_B<- read_excel('./project_data_1.xlsx', sheet = '영아사망률')
# 시점, 벨기에로 분류
lifeExp_B_1 <- lifeExp_B[,c(1,47)]
old_B_1 <- old_B[,c(1,129)]
movement_B_1 <- movement_B[,c(1,17)]
brith_B_1 <- brith_B[,c(1,17)]
infant_B_1 <- infant_B[,c(1,17)]
# 데이터프레임화
lifeExp_B_DF <- data.frame(lifeExp_B_1)
old_B_DF <- data.frame(old_B_1)
movement_B_DF<- data.frame(movement_B_1)
brith_B_DF<- data.frame(brith_B_1)
infant_B_DF<- data.frame(infant_B_1)
# 열이름 변경
colnames(lifeExp_B_DF) <- c("year", 'LifeExpectancy')
lifeExp_B_DF_1 <- lifeExp_B_DF[-1,]
colnames(old_B_DF)<- c("year", 'AgingIndex')
old_B_DF_1 <- old_B_DF[c(-(1:2)),]
colnames(movement_B_DF) <- c("year", 'MigrationRate')
movement_B_DF_1 <- movement_B_DF
colnames(brith_B_DF) <- c("year", 'FertilityRate')
brith_B_DF_1 <- brith_B_DF
colnames(infant_B_DF) <- c("year", 'InfantMortality')
infant_B_DF_1 <- infant_B_DF
# 인덱스 재구성
rownames(lifeExp_B_DF_1) <- NULL
rownames(old_B_DF_1) <- NULL
# five_DF를 cbind로 생성
five_DF <- cbind(lifeExp_B_DF_1, old_B_DF_1[2], movement_B_DF_1[2],
                 brith_B_DF_1[2], infant_B_DF_1[2])
# 멜트
five_DF_1<- melt(five_DF, id.vars = c('year'))
# five_DF_1의 value열을 뉴머릭화
five_DF_1[,3] <- as.numeric(five_DF_1[,3])
# min.max.scale로 value를 비율화
# min.max.scale함수
min.max.scale<-function (x) {
  return ((x-min(x))/(max(x)-min(x)))
}


# five_DF를 five_DF_m로 복제
five_DF_m <- five_DF
# five_DF의 시점을 제외한 열 뉴머릭화
five_DF[,2] <- as.numeric(five_DF[,2])
five_DF[,3] <- as.numeric(five_DF[,3])
five_DF[,4] <- as.numeric(five_DF[,4])
five_DF[,5] <- as.numeric(five_DF[,5])
five_DF[,6] <- as.numeric(five_DF[,6])
# five_DF_m에 five_DF의 각 항목을 비율화
five_DF_m$LifeExpectancy<-min.max.scale(five_DF$LifeExpectancy)
five_DF_m$AgingIndex<-min.max.scale(five_DF$AgingIndex)
five_DF_m$MigrationRate<-min.max.scale(five_DF$MigrationRate)
five_DF_m$FertilityRate<-min.max.scale(five_DF$FertilityRate)
five_DF_m$InfantMortality<-min.max.scale(five_DF$InfantMortality)


# five_DF_m을 melt시켜 five_DF_M로 형성
five_DF_M <- melt(five_DF_m, id.vars = c('year'))




# --------------------------------------------------------------------
# --------------------------------------------------------------------
# --------------------------------------------------------------------
ui <- fluidPage(
  
  tags$h1('프로젝트_방영찬'),
  
  mainPanel(
    tabsetPanel(
      tabPanel('도시화율', plotlyOutput('graph_urban')),
      tabPanel('기대수명', plotlyOutput('graph_expectLifeB')),
      tabPanel('5가지 항목', plotOutput('graph_five')),
      # tabPanel('col', plotlyOutput('graph_infantB')),
      tabPanel('2가지 항목', plotlyOutput('graph_birth_infant')),
    )
  ),

  # plotlyOutput('graph_urban'),
  # plotlyOutput('graph_expectLifeB'),
  # plotOutput('graph_five'),
  # plotlyOutput('graph_infantB'),
  # plotlyOutput('graph_birth_infant'),

  # plotlyOutput('graph', click = 'plot_click'),
  # tags$h3('이거슨 사용자 클릭에 반응하기:'),
  # tableOutput('data')
  
)
# --------------------------------------------------------------------
# --------------------------------------------------------------------
# --------------------------------------------------------------------
server <- function(input, output){
  # 도시화율----  
  output$graph_urban <- renderPlotly({
    
    xValue <- seq(1950, 2050, 1)
    
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
  })
  
  # 기대수명 ----
  output$graph_expectLifeB <- renderPlotly({
    
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
  })
  # five----
  output$graph_five <- renderPlot({
    
    # Load dataset from github
    # five_DF_M <- melt(five_DF_m, id.vars = c('시점'))
    
    # Plot
    five_DF_M %>%
      mutate(variable = fct_reorder(variable, value)) %>%
      ggplot( aes(y=variable, x=value,  fill=variable)) +
      geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
      theme_ridges() +
      theme(
        legend.position="none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab("") +
      ylab("Assigned Probability (%)")
  })
  # 출생률과 영아사망률----
  output$graph_birth_infant <- renderPlotly({
    
    coeff <- 1/10
    
    temperatureColor <- "#69b3a2"
    priceColor <- rgb(0.2, 0.6, 0.9, 1)
    year <- seq(1955,2100,5)
    ggplot(BI_DF, aes(x=year)) +
      
      geom_line( aes(y=birthB_DF$벨기에/coeff), size=2, color=temperatureColor) + 
      geom_line( aes(y=infantB_DF$벨기에), size=2, color=priceColor) +
      
      scale_y_continuous(
        
        name = "birthrate",
        
        sec.axis = sec_axis(~.*coeff, name="infant mortality")
      ) +
      theme_ipsum(grid=F) +
      theme(
        axis.title.y = element_text(color = temperatureColor, size=13),
        axis.title.y.right = element_text(color = priceColor, size=13)
      ) +
      ggtitle("birthrate & infant mortality")
    
  })
  # 영아사망률----
  # output$graph_infantB <- renderPlotly({
  #   
  #   fig <- plot_ly(infantB_DF, x = ~infantB_year, y = ~infantB_Belgie,
  #                  type = 'scatter', mode = 'lines')
  #   layout(fig, title='infant mortality of Belgie')
  #   fig
  # })
  
  # output$data <- renderTable(
  #   {
  #     req(input$plot_click) # req => request
  #     nearPoints(data, input$plot_click)
  #   }
  # )
  # 
}
# --------------------------------------------------------------------
# --------------------------------------------------------------------
# --------------------------------------------------------------------

shinyApp(ui, server)