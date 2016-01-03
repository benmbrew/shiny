library(shiny)
library(dplyr)
library(reshape)


# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Reactive expression to compose a data frame containing all of the values
  year_set <- reactive({
    
  dat[which(dat$year >= input$year[1] & dat$year <= input$year[2]),]
  }) 
  
  
  # Plot 1
  output$plot1 <- renderPlot({
      

    if(input$opp == 'All Opponents'){
      
    temp <- year_set()
    temp2 <- temp %>%
       group_by(result) %>%
       summarise(count = n())
    temp2$col <- ifelse(temp2$result == 'W', 'blue', 'orange')
    
    ggplot(data = temp2, aes(result, count)) + 
      geom_bar(stat= 'identity', fill = temp2$col, alpha = 0.8) + 
      geom_text(data=temp2,aes(result,count,label=count),vjust=0, size = 8) + 
      xlab('Result') + ylab('Wins/Losses') + 
      ylim(c(0, max(temp2$count) + 10)) + tt 
    
    }else{
    
      temp <- year_set()
      temp <- temp[which(temp$opp == input$opp),]
  
      validate(
        need(nrow(temp) > 0, "Please select a time frame and oppenent that coincide!")
      )
    
      temp2 <- temp %>%
        group_by(result) %>%
        summarise(count = n())
      temp2$col <- ifelse(temp2$result == 'W', 'blue', 'orange')
      
      ggplot(data = temp2, aes(result, count)) + 
        geom_bar(stat= 'identity', fill = temp2$col, alpha = 0.8) + 
        geom_text(data=temp2,aes(result,count,label=count),vjust=0, size = 8) + 
        xlab('Result') + ylab('Wins/Losses') + 
        ylim(c(0, max(temp2$count) + 10)) + tt 
      }
    
    })
  
  # Plot 2
  output$table1 <- renderDataTable({
    
    
    if(input$opp == 'All Opponents'){
      
      temp <- year_set()
      temp2 <- temp %>%
        group_by(year) %>%
        summarise(Points = mean(points),
                  Total_Yds = mean(tot_yds),
                  Pass_Yds = mean(pass_yds),
                  Rush_Yds = mean(rush_yds),
                  Opponent_Points = mean(opp_points),
                  Opponent_Total_Yds = mean(def_totyds),
                  Opponent_Pass_Yds = mean(def_passyds),
                  Opponenet_Rush_Yds =mean(def_rushyds))
                  
      
    temp2 <- roundInt(temp2)
    temp2
      
    }else{
      
      temp <- year_set()
      temp <- temp[which(temp$opp == input$opp),]
      temp2 <- temp %>%
        group_by(year) %>%
        summarise(Points = mean(points),
                  Total_Yds = mean(tot_yds),
                  Pass_Yds = mean(pass_yds),
                  Rush_Yds = mean(rush_yds),
                  Opponent_Points = mean(opp_points),
                  Opponent_Total_Yds = mean(def_totyds),
                  Opponent_Pass_Yds = mean(def_passyds),
                  Opponenet_Rush_Yds =mean(def_rushyds))
      
      
      validate(
        need(nrow(temp) > 0, "Please select a time frame and oppenent that coincide!")
      )
      
      temp2 <- roundInt(temp2)
      temp2
    }
    
  })
  
})
