library(shiny)
library(DT)

shinyUI(fluidPage(
  titlePanel("FluCode model comparison"),
  
  sidebarLayout(
    sidebarPanel(width = 2,

                 
                 selectInput("outcome", "Select Outcome:",
                             choices = c("Symp. Illness", 'Hospitalizations', 'Deaths', 'Antiviral RX'),
                             selected = 'Symp. Illness'),
                 

                 selectInput("grp", "Select Agegroup:",
                             choices = c("Overall", '0-4 years', '5-17 years', '18-49 years', '50-64 years', '65+ years'),
                             selected = 'Overall'),
                 
                 radioButtons('scn', 'Scenario group', choiceNames = c('2009', 'Hypothetical'), choiceValues = c('RL', 'HP'))
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Cumulative", plotOutput("cmlPlot", height = '800px')),
                  tabPanel("Peak magnitude", plotOutput("peakIntPlot", height = '800px')),
                  tabPanel("Legend", imageOutput("scnLegend", height = '800px'))
      )
    )
    
    
  )
))
