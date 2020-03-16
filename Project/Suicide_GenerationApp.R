if (!require("tidyverse")) install.packages('tidyverse')
if (!require("shiny")) install.packages('shiny')
if (!require("shinydashboard")) install.packages('shinydashboard')


library(tidyverse)
library(shiny)
library(shinydashboard)

suicide <- readr::read_csv("data/master.csv")
names(suicide) <- str_replace_all(names(suicide), c(" " = "_", "/" = "_", "-" = "_"))
suicide <-
  suicide %>% 
  dplyr::rename(
    suicide_number = suicides_no,
    suicide_per_100kpop = suicides_100k_pop)
suicide$country <- as.factor(suicide$country)
suicide$age <- as.factor(suicide$age)
suicide$sex <- as.factor(suicide$sex)
suicide$country_year <- as.factor(suicide$country_year)
suicide$generation <- as.factor(suicide$generation)
suicide$year <- as.factor(suicide$year)

ui <- 
    dashboardPage(
    dashboardHeader(title = "Suicide-Generation App"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      fluidRow(
        box(title = "Plot Options", background = "maroon", solidHeader = TRUE, width = 2,
            selectInput("generation", "Generation", choices = c("G.I. Generation", "Silent", "Boomers", "Generation X", "Millenials", "Generation Z"),
                        selected = "G.I. Generation"),
            selectInput("year", "Year", choices = c("1985", "1990", "1995", "2000", "2005", "2010", "2016"),
                        selected = "1985"),
            selectInput("sex", "Sex", choices = c("male", "female"))),
        box(title = "Suicide Generation Plot", background = "maroon", solidHeader = TRUE, width = 10,
            plotOutput("plot1", width = "1100px", height = "400px"))),
      fluidRow(
        box(title = "Plot Options", background = "fuchsia", solidHeader = TRUE, width = 2, 
            selectInput("yearr", "Year", choices = c("1985", "1990", "1995", "2000", "2005", "2010", "2016"),
                        selected = "1985"),
            selectInput("agee", "Age", choices = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"), selected = "5-14 Years")),
        box(title = "Suicide Sex Plot", background = "fuchsia", solidHeader = TRUE, width = 10,
            plotOutput("plot2", width = "1100px", height = "400px"
            )
        )
      )
    )
  )

server <- function(input, output, session) { 
  
  
  output$plot1 <- renderPlot({
    
    suicide %>%
      filter(sex == input$sex, year == input$year, generation == input$generation) %>%
      ggplot(aes(x = age, y = suicide_number, fill = age))+
      geom_bar(stat = "identity")
  })
  output$plot2 <- renderPlot({
    
    suicide %>%
      filter(year == input$yearr, age == input$agee) %>%
      ggplot(aes(x = sex, y = suicide_number, fill = sex))+
      geom_bar(stat = "identity")
  })
  
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui, server)