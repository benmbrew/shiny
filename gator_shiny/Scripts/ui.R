library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Through the Years"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    conditionalPanel(condition = "input.tabs = 'Wins/Losses'",
    # Simple integer interval
    sliderInput("year", "Year", 
                min=2000, max=2015, value = c(2000,2015), sep = '')
    ),
    
    conditionalPanel(condition = "input.tabs = 'Wins/Losses'",
    selectInput("opp", "Opponent", 
              c(unique(sort(as.character(dat$opp))), 'All Opponents'),
              selected = 'All Opponents')
    )
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    tabsetPanel(id = 'tabs',
    tabPanel("Wins/Losses",
             plotOutput("plot1"))
    )
  )
))