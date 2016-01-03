library(shiny)
library(dplyr)
library(reshape)

# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Reactive expression to compose a data frame containing all of the values
  year_set <- reactive({
    
  dat[which(dat$year >= input$year[1] & dat$year <= input$year[2]),]
  }) 
    
  
  # Show the values using an HTML table
  output$plot1 <- renderPlot({
  
    temp <- year_set()
    temp2 <- temp %>%
       group_by(result) %>%
       summarise(count = n())
    temp2$col <- ifelse(temp2$result == 'W', 'blue', 'orange')
    
    ggplot(data = temp2, aes(result, count)) + 
      geom_bar(stat= 'identity', fill = temp2$col, alpha = 0.8) + 
      geom_text(data=temp2,aes(result,count,label=count),vjust=0) + 
      xlab('Result') + ylab('Wins/Losses') + tt 
      
  })
})
