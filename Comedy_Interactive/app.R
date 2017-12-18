library(shiny)
library(tidyverse)
library(markdown)
library(maps)
library(mapdata)
library(readr)

comedians_list <- c("Amy Schumer", "Aziz Ansari", "Ali Wong", "Bill Burr", "Bill Hicks",
                    "Bo Burnham", "Brian Posehn", "Chris Rock", "Dave Chappelle", "Demetri Martin",
                    "Donald Glover", "Daniel Tosh", "Eddie Murphy", "Frankie Boyle", "George Carlin",
                    "Iliza Shlesinger", "Jeff Foxworthy", "Jimmy Carr", "Jim Gaffigan", "Jim Jefferies",
                    "John Mulaney", "Louis CK", "Lenny Bruce", "Maria Bamford", "Neal Brennan", "Patton Oswalt",
                    "Richard Pryor", "Ron White", "Ricky Gervais", "Steve Harvey", "Tom Segura", "Steven Wright",
                    "Redd Foxx", "Jerry Seinfeld", "Hannibal Burress", "Larry the Cable Guy", "Mitch Hedberg",
                    "Bill Engvall")

tour_names <- c("Amy Schumer", "Aziz Ansari", "Bill Burr", "Bill Engvall", "Bo Burnham", "Brian Posehn",
                "Chris Rock", "Daniel Tosh", "Dave Chappelle", "Demetri Martin", "Frankie Boyle", "Hannibal Burress", 
                "Iliza Schlesinger", "Jeff Foxworthy", "Jim Gaffigan", "Jim Jefferies", "Jimmy Carr",
                "John Mulaney", "Louis CK", "Neal Brennan", "Patton Oswalt", "Ron White", "Tom Segura",
                "Larry the Cable Guy", "Maria Bamford")

master <- read_csv("master.csv")

ranks <- read_csv("ranks.csv")

Ratios <- read_csv("Ratios.csv")

tours_by_state <- read_csv("tours_by_state.csv")

idf <- read_csv("idf.csv")

 
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
               selectInput("map_name", "Proportion of Shows by State", tour_names)
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
  
  mapInput <- reactive({
    input$map_name
  })
          
   output$distPlot <- renderPlot({
     ggplot(datasetInput(), mapping = aes(x = reorder(name, -rf), y = rf, fill = name)) + 
       geom_col() + theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "none") +
       labs(x = "Comedian", y = "Frequency", title = str_c("Comedians who say '", input$term, "' most frequently"))
     
   })
   
   output$dist_words <- renderTable({comedianInput()})
   
   output$US_tour <- renderDataTable({tourInput()})

   output$ranks <- renderDataTable({rankInput()})
   
   output$Map <- renderPlot({
     ditch_the_axes <- theme(
       axis.text = element_blank(),
       axis.line = element_blank(),
       axis.ticks = element_blank(),
       panel.border = element_blank(),
       panel.grid = element_blank(),
       axis.title = element_blank(),
       legend.title = element_blank()
     )
     ggplot() + geom_polygon(data = tours_by_state,
                             mapping = aes(x = long, y = lat, group = group, fill = tours_by_state[mapInput()]),
                             color = "black") +
       coord_fixed(1.3) + ditch_the_axes + 
       scale_fill_gradient2(midpoint = log(0.05), low = "red", high = "blue", mid = "white",
                            space = "Lab", trans = "log")
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

