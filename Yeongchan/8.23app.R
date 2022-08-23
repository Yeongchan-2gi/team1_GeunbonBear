library(shiny)
library(gapminder)
library(ggplot2)

infantB  <- read_excel('../project_data.xlsx', sheet = '영아사망률')
infantB_DF <- data.frame(infantB)

ui <- fluidPage(

  tags$h3('이거슨 그림 그리기:'),
  
  plotlyOutput('graph_1'),
  plotlyOutput('graph_2'),
  # plotlyOutput('graph', click = 'plot_click'),
  tags$h3('이거슨 사용자 클릭에 반응하기:'),
  # tableOutput('data')
  
)

server <- function(input, output){
    
  output$graph_1 <- renderPlotly({
    
    fig <- plot_ly(infantB_DF, x = ~infantB_year, y = ~infantB_Belgie,
                   type = 'scatter', mode = 'lines')
    layout(fig, title='infant mortality of Belgie')
    fig
  })
  
  
  
  output$graph_2 <- renderPlotly({
    
    fig <- plot_ly(infantB_DF, x = ~infantB_year, y = ~infantB_Belgie,
                   type = 'scatter', mode = 'lines')
    layout(fig, title='infant mortality of Belgie')
    fig
  })
  
  # output$data <- renderTable(
  #   {
  #     req(input$plot_click) # req => request
  #     nearPoints(data, input$plot_click)
  #   }
  # )
  
}


shinyApp(ui, server)