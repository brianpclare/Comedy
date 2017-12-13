#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Stand Up Comedy"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("term", "Word", value = "redneck")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datasetInput <- reactive({
         filter(master, word == input$term) %>% top_n(10, rf)
  })
          
          
   output$distPlot <- renderPlot({
     ggplot(datasetInput(), mapping = aes(x = reorder(name, -rf), y = rf, fill = name)) + 
       geom_col() + theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
       labs(x = "Comedian", y = "Frequency", title = str_c("Comedians who say '", input$term, "' most frequently"))
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

