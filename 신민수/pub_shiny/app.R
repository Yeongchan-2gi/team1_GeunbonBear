# 가공한 데이터 불러오기
#data <- read.csv('../mydf.csv')
library(shiny)
library(ggplot2)
library(gganimate)
library(gifski)

# 1.UI ----------------------------------------------------------

ui<-fluidPage(
  # headerPanel(h1('OECD국가별 출산율/신생아율 연도별 변화')),
  # 
  # sidebarPanel()
  mainPanel(
     tabsetPanel(
      tabPanel('Team1_GeunbonBear intro',
               h3("OECD 국가의 인구 변화"),
               h3("팀 구성"),
                  fluidRow(div("방영찬 : OECD국가의 연도별 도시화율을 선형그래프로 나타내어 변화율이 눈에띄는 국가의 각 지표를 밀도 그래프로 시각화"),
                  fluidRow(div("신민수 : OECD국가의 연도별 출산율과 신생아 사망률을 버블 그래프 애니메이션으로 시각화")),
                  fluidRow(div("김도영 : OECD국가의 연도별 노령화지수에 대한 부양비 interactive 그래프, 대륙에 대한 기대수명 boxplot 그래프, 대륙에 대한 장례인구 및 노령화지수 barplot 그래프, 
                                        기대수명에 대한 노령화지수 bubble chart 그래프")),
                  fluidRow(div("김지윤 : 활동가능 인구수와 취업인구수를 3D패션디자인 그래프화, 기대수명과 인당 gdp간의 산점도와 회귀선을 plotly로  interactive한 그래프, 연도별 기대수명의 순위 Racebarplot"),
                
                h3("출처"),    
                fluidRow(div("KOSIS 국가통계포털: https://kosis.kr/search/search.do")),
                fluidRow(div("OECD DATA: https://data.oecd.org"))
                  ),
      
      tabPanel('grape1',
               h3("출산율/신생아사망률"),
               img(src="https://media4.giphy.com/media/mJr3pvSWMzLJxCm9ir/giphy.gif?cid=790b761195670aa2111935f4e743a68340c287b384e565a4&rid=giphy.gif&ct=g")),
      
      tabPanel('grape2',
               h3("출산율/신생아사망률(대륙별)"),
               img(src="https://media0.giphy.com/media/QhmauuR1yGZBhAQfHC/giphy.gif?cid=790b7611a17506e7c924d32c197c018d53b52f89d3033318&rid=giphy.gif&ct=g"))
    )
  )
)
# 2. SERVER -----------------------------------------------------
server <- function(input, output, session){

    

}




# 3. SHINYAPP(UI,SERVER) ----------------------------------------
shinyApp(ui = ui, server = server)


