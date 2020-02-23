###########################################################################################
#                                                                                         #
#                                 IMPORTING LIBRARIES                                     #
#                                                                                         #
###########################################################################################

library(plotly)
library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(ggplot2)
library(ggridges)

fare_data <- read.csv("data/fares.csv")
###########################################################################################
#                                                                                         #
#                                       SERVER CODES                                      #
#                                                                                         #
###########################################################################################
# Define server logic 
server <- function(input, output, session) {
  
  # Ridge Plot
  companyridge_data <- reactive({
    companyridge_data <- fare_data %>% 
      filter(company %in% c(input$companyA,input$companyB)) %>%
      select(fare,company) 
  })
  
  output$fare_ridgeplot <- renderPlot({
    ggplot(companyridge_data(), aes(y=1,x=fare,fill=company)) +
      geom_density_ridges(alpha=0.5) +
      xlim(0,100) +
      theme(axis.text=element_text(size=10,), axis.title.y = element_blank(), legend.position = "bottom") 
  })

}