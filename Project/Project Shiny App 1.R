if (!require("tidyverse")) install.packages('tidyverse')
if (!require("shiny")) install.packages('shiny')
if (!require("shinydashboard")) install.packages('shinydashboard')


library(tidyverse)
library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title = "Suicide Per 100k Pop by Age Group"),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      box(title = "Plot Options", width = 3,
          selectInput("x", "Select Year", choices = unique(suicide$year), 
                      selected = "2010"),
          selectInput("y", "Select Country", choices = unique(suicide$country),
                      selected = "Albania"),
          selectInput("z", "Select Sex", choices = unique(suicide$sex),
                      selected = "Male")
      ), 
      box(title = "Suicide Per 100k Pop by Age Group", width = 7,
          plotOutput("plot", width = "600px", height = "500px")
      ) 
    ) 
  ) 
) 

server <- function(input, output, session) { 
  
  output$plot <- renderPlot({
    suicide %>% 
      filter(year==input$x & country==input$y & sex==input$z) %>% 
      ggplot(aes(x=age, y=suicide_per_100kpop)) + 
      geom_col(color="black", fill="steelblue", alpha=0.75) +
      theme_light(base_size = 18) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      labs(x = "Age Group", y = "Suicide Per 100k Pop")
  })
  
  
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui, server)