
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

# Read in cleaned data
source('read_cleaned_data.R')

# Source the helper functions
source('functions.R')


shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({
    # Plot
    if(!input$state %in% c('alaska', 'hawaii')){
      usa <- usa[which(!usa$state %in% c('alaska', 'hawaii')),]
    }
    choro(shape = usa,
          #select all rows of variable 
          var = usa@data[,input$variable])
    # subset (shows on map which state was chosen)
    plot(usa[which(usa$state == input$state),],
         add = TRUE,
         border = 'grey',
         lwd = 3)
    #     points(
    #       x = coordinates(usa)[which(usa$state == input$state),1],
    #       y = coordinates(usa)[which(usa$state == input$state),2],
    #       cex = 4,
    #       col = adjustcolor('blue', alpha.f = 0.6)
    #     )
  })
  
  
  output$plot2 <- renderPlot({
    state_stat <- usa@data[which(usa@data$state == input$state), input$variable]
    
    hist(usa@data[,input$variable], breaks = 10,
         col = 'lightblue',
         border = 'white',
         xlab = paste0('Number of ', input$variable),
         main = paste0('Comparison of ', input$state, ' with rest of country'))
    abline(v = state_stat,
           col = adjustcolor('red', alpha.f = 0.6), 
           lwd = 3)
    
    text(x = state_stat,
         y = 5,
         pos = 4,
         labels = toupper(input$state))
  })
  
  
  output$plot3 <- renderPlot({
    over_time <- rcra_eval %>%
      group_by(year, state) %>%
      summarise(SNY = length(evaluation_type[evaluation_type == 'SNY']),
                FRR = length(evaluation_type[evaluation_type == 'FRR']),
                Violation = length(found_violation[found_violation == 'Y  ']),
                CEI = length(evaluation_type[evaluation_type == 'CEI']),
                FCI = length(evaluation_type[evaluation_type == 'FCI']),
                SNN = length(evaluation_type[evaluation_type == 'SNN']))
    over_time <- over_time[which(over_time$year >= 1980),]
    
    over_time <- data.frame(over_time)
    y_max <- max(over_time[,input$variable])
    plot(c(1982, 2012), c(0,y_max),
         xlab = 'Year',
         ylab = 'Number',
         type = 'n') # type n, gives just background
    states <- unique(sort(over_time$state))
    #for each state, subset over_time data frame
    for (i in 1:length(states)){
      #over_time only for the state selected
      sub_state <- data.frame(over_time[which(over_time$state == states[i]),])
      #draws line for each state and only for variable selected
      lines(sub_state$year,
            sub_state[,input$variable],
            col = adjustcolor('black', alpha.f = 0.4))
    }
    selected_state <- data.frame(over_time[which(over_time$state == input$state),])
    lines(x = selected_state$year,
          y = selected_state[,input$variable],
          col = adjustcolor('darkred', alpha.f = 0.6), 
          lwd = 4)
    legend('topleft',
           lty = 1,
           lwd = c(1,4),
           col = adjustcolor(c('black', 'darkred'), alpha.f = 0.6),
           legend = c('other states', input$state))
  })
  
  
  output$plot4 <- renderPlot({
    sub_zip(state = input$state,
            variable = input$variable)
  })
  
  output$plot5 <- renderPlot({
    
    temp <- by_state_year[which(by_state_year$year >= input$years[1] & 
                                  by_state_year$year <= input$years[2]),]
    
    temp2 <- temp %>%
      group_by(state) %>%
      summarise(SNY = sum(SNY),
                FRR = sum(FRR),
                Violation = sum(Violation),
                CEI = sum(CEI),
                FCI = sum(FCI),
                SNN = sum(SNN))
    
    usa_sub <- usa
    usa_sub$SNY <- NULL
    usa_sub$FRR <- NULL 
    usa_sub$Violation <- NULL 
    usa_sub$CEI <- NULL 
    usa_sub$FCI <- NULL
    usa_sub$SNN <- NULL
    
    usa_sub@data <- left_join(usa_sub@data, temp2)
    
    # Plot
    if(!input$state %in% c('alaska', 'hawaii')){
      usa_sub <- usa_sub[which(!usa_sub$state %in% c('alaska', 'hawaii')),]
    }
    choro(shape = usa_sub,
          #select all rows of variable 
          var = usa_sub@data[,input$variable])
    # subset (shows on map which state was chosen)
    plot(usa_sub[which(usa_sub$state == input$state),],
         add = TRUE,
         border = 'grey',
         lwd = 3)
   
  })
  
 
})
