##### Read in data and put it in form for shiny
functions <- 'Lib'
data <- 'Data'
dat <- read.csv(paste0(data, '/all_years.csv'))


# Source helper functions
source('Lib/helpers.R')

########### Change ranking greater than 25 to unranked.
rankUnrank <- function(data, variable){
  
  for(i in 1:nrow(data)){
    
    sub_dat <- data[,variable][i]
    
    if(sub_dat != 'unrank'){
      
      if(as.numeric(sub_dat) > 25){
        
        sub_dat <- 'unrank'
        
      }else{
        sub_dat <- sub_dat
      }
    }
    data[, variable][i] <- sub_dat
    print(i)
  }
  return(data)
}

dat <- rankUnrank(dat, variable = 'rank')
dat <- rankUnrank(dat, variable = 'opp_rank')

######### Change characters to factors
char2factor <- function(data) {
  
  data.frame(lapply(data, function (x) {
    
    if (is.character(x)) factor(x)
    
    else x
  }))
}

dat <- char2factor(dat)


