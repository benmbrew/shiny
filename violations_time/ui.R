shinyUI(fluidPage(
  
  titlePanel("EPA data explorer"),
  
  sidebarPanel(
    selectInput('variable',
                'Indicator of interest',
                choices = c('Violations' = 'Violation',
                            'CEI' = 'CEI',
                            'FCI' = 'FCI',
                            'FRR' = 'FRR',
                            'NRR' = 'NRR',
                            'SNN' = 'SNN',
                            'SNY' = 'SNY')),   
    
    selectInput('state',
                'State', 
                choices = c('alabama',
                            'alaska',
                            'arizona',
                            'arkansas',
                            'california',
                            'colorado',
                            'connecticut',
                            'delaware',
                            'district of columbia',
                            'florida',
                            'georgia',
                            'hawaii',
                            'idaho',
                            'illinois',
                            'indiana',
                            'iowa',
                            'kansas',
                            'kentucky',
                            'louisiana',
                            'maine',
                            'maryland',
                            'massachusetts',
                            'michigan',
                            'minnesota',
                            'mississippi',
                            'missouri',
                            'montana',
                            'nebraska',
                            'nevada',
                            'new hampshire',
                            'new jersey',
                            'new mexico',
                            'new york',
                            'north carolina',
                            'north dakota',
                            'ohio',
                            'oklahoma',
                            'oregon',
                            'pennsylvania',
                            'rhode island',
                            'south carolina',
                            'south dakota',
                            'tennessee',
                            'texas',
                            'utah',
                            'vermont',
                            'virginia',
                            'washington',
                            'west virginia',
                            'wisconsin',
                            'wyoming'),
                selected = 'illinois'),
    
    
    conditionalPanel(condition = "input.tabs == 'National overview'"
     
      
    
     
    ),
    conditionalPanel(condition = "input.tabs == 'Over time'",
                     
                     sliderInput("years", label = "Years",
                                 min = 1980, max = 2015, value = c(1990, 2000), sep = "")
                     
                     ),
    conditionalPanel(condition = "input.tabs == 'Zip code'"
                    
                    )
    ),
    
    
    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("National overview", 
                 plotOutput("plot1"),
                 plotOutput("plot2")), 
        tabPanel('Over time',
                 plotOutput('plot3'),
                 plotOutput('plot5')),
        tabPanel("Zip code", 
                 plotOutput("plot4"))
      )
    )
  
))