if (!require("tidyverse")) install.packages('tidyverse')
library("tidyverse")
if (!require("naniar")) install.packages('naniar')
library("naniar")
if (!require("shiny")) install.packages('shiny')
library("shiny")
if (!require("shinydashboard")) install.packages('shinydashboard')
library("shinydashboard")
if (!require("shinythemes")) install.packages('shinythemes')
library("shinythemes")

srate <- readr::read_csv("master.csv")
sratee <- srate %>%
  rename(gdpyr = `gdp_for_year ($)`, gdpcap = `gdp_per_capita ($)`, suicper100k = `suicides/100k pop`)

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
  
  # the code to make the plot of iris data grouped by species
  output$plot1 <- renderPlot({
    
    sratee %>%
      filter(sex == input$sex, year == input$year, generation == input$generation) %>%
      ggplot(aes(x = age, y = suicides_no, fill = age))+
      geom_bar(stat = "identity")
  })
  output$plot2 <- renderPlot({
    
    sratee %>%
      filter(year == input$yearr, age == input$agee) %>%
      ggplot(aes(x = sex, y = suicides_no, fill = sex))+
      geom_bar(stat = "identity")
  })
  # stop the app when we close it
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui, server)