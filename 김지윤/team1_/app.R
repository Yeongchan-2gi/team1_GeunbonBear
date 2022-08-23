
library(shiny)
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
library(gganimate)
library(gifski)
library(gganimate)
library(gapminder)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(ggthemes)
library(gganimate)
library(gifski)
library(stringr)









df_plotly_1 <- read.csv("./df_plotly_1.csv")
data <- read.csv('./data.csv')

ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel('plotly', plotlyOutput('mytable1')),
      tabPanel('G1', 
               img(src='https://media0.giphy.com/media/1jsCBnDZYOGGblGoOm/giphy.gif?cid=790b7611f3976296b788662696f1f6fa8d8ada825a8655f7&rid=giphy.gif&ct=g')), 
      tabPanel('G2', 
               img(src='https://media0.giphy.com/media/YUkmTSzXSYjs92YqLQ/giphy.gif?cid=790b7611425d3183ad21fb44ab650234bd5e8e606d83aab3&rid=giphy.gif&ct=g')) 
      
    )
  )
  
  
)

server <- function(input, output){
  
  output$mytable1 <- renderPlotly({
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
    
  })

}
shinyApp(ui, server)
