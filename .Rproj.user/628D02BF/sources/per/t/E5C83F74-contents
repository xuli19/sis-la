library(visNetwork)
library(shiny)

shinyUI(fluidPage(
  
  titlePanel("A Student-Facing, Interactive Network (SIN) Tool"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("week", "Choose a week:", 
                  c("Week01"=1,"Week02"=2,"Week03"=3),
                  helpText("This is a tool demo"
                  ),
                  width="100%"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("student-student network", visNetworkOutput("stos",height = '650px')), 
        tabPanel("student-term network", visNetworkOutput("stot",height = '650px')),
        tabPanel("term-term network", visNetworkOutput("ttot",height = '650px')),
        tabPanel("More info", htmlOutput("info"))
        
        
      )
    )
  )
))
