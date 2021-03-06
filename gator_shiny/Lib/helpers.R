library(ggplot2)

# Set the default theme
# Modify it with theme()
tt <- theme(axis.text = element_text(size=16, colour=NULL),
            axis.title = element_text(size = 20, colour= NULL),
            plot.title = element_text(size = 30),
            # axis.line = element_line(size = 1, colour = "black"),
            axis.text = element_text(colour = "blue"),
            axis.ticks = element_line(size = 2),
            legend.background = element_rect(fill = "grey"),
            legend.position = c(0.13, 0.88),
            panel.grid.major = element_line(colour = "grey80"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            text = element_text(size = 12, family = 'Times New Roman')
)

bcn_data_theme <- tt

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
# http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

#####
# Define simple mathematical function
# for getting euclidean distance in kilometers
#####
get_distance <- function(lon1, 
                         lat1, 
                         lon2, 
                         lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

#####
# CONFIDENCE INTERVALS ON PROPORTIONS
##### 

##### CONFIDENCE INTERVALS ON PROPORTIONS
# (https://aghaynes.wordpress.com/2014/04/09/calculating-confidence-intervals-for-proportions/)
simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}

##### CONFIDENCE INTERVALS ON PROPORTIONS
# (https://aghaynes.wordpress.com/2014/04/09/calculating-confidence-intervals-for-proportions/)
simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}

##### FARENHEIT TO CELSIUS
# http://swcarpentry.github.io/r-novice-inflammation/08-making-packages-R.html
f2c <- function(temp) {
  #Converts Fahrenheit to Celsius using fahr_to_kelvin() and kelvin_to_celsius()
  fahr_to_kelvin <- function(temp) {
    #Converts Fahrenheit to Kelvin
    kelvin <- ((temp - 32) * (5/9)) + 273.15
    kelvin
  }
  kelvin_to_celsius <- function(temp) {
    #Converts Kelvin to Celsius
    Celsius <- temp - 273.15
    Celsius
  }
  temp_k <- fahr_to_kelvin(temp)
  result <- kelvin_to_celsius(temp_k)
  result
}

#####
# MULTIPLOT
#####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

roundInt <- function(data) {
  
  data.frame(lapply(data, function (x) {
    
    if (is.numeric(x)) round(x)
    
    else x
  }))
}