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
                             selected = 'Overall')
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Cumulative", plotOutput("cmlPlot", height = '800px')),
                  tabPanel("Peak magnitude", plotOutput("peakIntPlot", height = '800px')),
                  #tabPanel("Peak timing", plotOutput("peakPlot", height = '800px')),
                  tabPanel("Trajectory", plotOutput("trajPlot", height = '800px')),
                  tabPanel("Table", dataTableOutput("smTable", height = '800px')),
                  tabPanel("Cumulative - Diff", plotOutput("cmlPlotDiff", height = '800px')),
                  tabPanel("Peak magnitude - Diff", plotOutput("peakIntPlotDiff", height = '800px')),
                  #tabPanel("Trajectory - Diff", plotOutput("trajPlotDiff", height = '800px')),
                  tabPanel("Table - Diff", dataTableOutput("smTableDiff", height = '800px')),
                  tabPanel("Burden Reduction", plotOutput("rankPlot", height = '800px')),
                  tabPanel("Legend", imageOutput("scnLegend", height = '800px'))
      )
    )
    
    
  )
))
