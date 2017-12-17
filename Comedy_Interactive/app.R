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
library(markdown)
library(ggmap)
library(maps)
library(mapdata)

# Define UI for application that draws a histogram
 
ui <- navbarPage("Comedy",  
  
   tabPanel("By Word",
    sidebarLayout(
        sidebarPanel(
           textInput("term", "Word", value = "redneck")
      ),
    
      mainPanel(
         plotOutput("distPlot")
      )
    )
  ),

  tabPanel("By Comedian",
           sidebarLayout(
             sidebarPanel(
               selectInput("name", "Most Distinctive Vocabulary For:", comedians_list)
             ),
             
             mainPanel(
               tableOutput("dist_words")
             )
           )
        ),
  tabPanel("Ranks",
           sidebarLayout(
             sidebarPanel(
               helpText("Ranking of Comedians")
             ),
             
             mainPanel(
               dataTableOutput("ranks")
             )
           )
  ),
  tabPanel("Touring - US",
           sidebarLayout(
             sidebarPanel(
               helpText("Proportion of Shows in US")
             ),

             mainPanel(
               dataTableOutput("US_tour")
             )
           )
  ),
  tabPanel("Touring - Map",
           sidebarLayout(
             sidebarPanel(
               helpText("Most Visits to Each US State")
             ),
             
             mainPanel(
               plotOutput("Map")
             )
           )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datasetInput <- reactive({
         filter(master, word == input$term) %>% top_n(10, rf)
  })
  
  comedianInput <- reactive({
    idf %>% filter(name == input$name) %>% top_n(10, tf_idf) %>% arrange(desc(tf_idf)) %>% 
      mutate(tf_idf = 1000*tf_idf) %>% select(word, "Count" = n, "Distinctiveness" = tf_idf)
  })
  
  tourInput <- reactive({
    Ratios %>% arrange(desc(`Ratio of US Shows`))
    
  })
  
  rankInput <- reactive({
    ranks %>% select(-Avg, -adjustment) %>% mutate(Aggregate = adj_avg) %>% select(-adj_avg)
  })
  
          
   output$distPlot <- renderPlot({
     ggplot(datasetInput(), mapping = aes(x = reorder(name, -rf), y = rf, fill = name)) + 
       geom_col() + theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "none") +
       labs(x = "Comedian", y = "Frequency", title = str_c("Comedians who say '", input$term, "' most frequently"))
     
   })
   
   output$dist_words <- renderTable({comedianInput()})
   
   output$US_tour <- renderDataTable({tourInput()})

   output$ranks <- renderDataTable({rankInput()})
   
   output$map <- renderPlot({
     US_states <- map_data("states")
     ditch_the_axes <- theme(
       axis.text = element_blank(),
       axis.line = element_blank(),
       axis.ticks = element_blank(),
       panel.border = element_blank(),
       panel.grid = element_blank(),
       axis.title = element_blank()
     )
     ggplot(data = state_freqs) + 
       ditch_the_axes
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

