#####
# THESE ARE SOME NECESSARY FUNCTIONS AND LIBRARIES
# FOR VISUALIZING THE EPA DATA GEOGRAPHICALLY
#####

library(dplyr)
library(rgdal)
library(maps)
library(sp)
library(rgeos)
library(RColorBrewer)
library(classInt)

#####
# Compass Rose
#####

#(from http://r-sig-geo.2731867.n2.nabble.com/How-to-diplasy-a-compass-rose-on-a-map-td4509034.html)
compass_rose <-function(x,y,rot=0,cex=1) { 
  oldcex<-par(cex=cex) 
  mheight<-strheight("M") 
  xylim<-par("usr") 
  plotdim<-par("pin") 
  xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1] 
  point.angles<-seq(0,7*pi/4,by=pi/4)+pi*rot/180 
  crspans<-rep(c(mheight*3,mheight/2),4) 
  xpoints<-cos(point.angles)*crspans*xmult+x 
  ypoints<-sin(point.angles)*crspans+y 
  polygon(xpoints,ypoints) 
  txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.33*crspans[1]*xmult+x 
  txtypoints<-sin(point.angles[c(1,3,5,7)])*1.33*crspans[1]+y 
  text(txtxpoints,txtypoints,c("E","N","W","S")) 
  par(oldcex) 
} 

#####
# COLLAPSE MAP INTO ONLY OUTER BOUNDARY
#####
collapse_map <- function(x){
  require(maptools)
  boundary <- unionSpatialPolygons(x, rep(1, length(x@polygons)))
}

library(RColorBrewer)
library(classInt)

#####
# CHOROPLETH MAP
#####
choro <- function(
  shape = NULL,
  boundary = NULL,
  main = NULL,
  var = NULL,
  color1 = "lightgrey",
  color2 = "red",
  color3 = 'darkred',
  legend_round = -2,
  legend_pos = "bottomleft",
  long_legend = TRUE,
  fixed_scale = TRUE,
  lwd = 0.2,
  border = "darkgrey",
  legend_text_col = "black",
  add = FALSE,
  compass = FALSE){
  my_colors <- colorRampPalette(c(color1, color2, color3))(10)
  if(!is.null(fixed_scale)){
    if(fixed_scale){
      fixed_scale <- seq(0, max(var, na.rm = TRUE), length = 10)
    }
    if(length(fixed_scale) != 10){stop("Fixed scale must be of length 10")}
    my_quantiles <- fixed_scale
  } else{
    my_quantiles <- quantile(var, na.rm = TRUE, probs = seq(0,1, length = 10))
  }
  my_values <- vector(mode = "numeric", length = length(var))
  for (i in 1:length(var)){
    diffs <- (var[i] - as.numeric(my_quantiles))^2
    best <- which.min(diffs)[1]
    my_values[i] <- best
  }
  map_colors <- my_colors[my_values]
  plot(shape, col = map_colors, border = border, lwd = lwd,
       main = main, add = add)
  if(long_legend){
    legend_colors <- colorRampPalette(my_colors)(25)
    legend(legend_pos, # position
           legend = c(min(round(my_quantiles, digits = legend_round)),
                      rep(NA, 11),
                      median(round(my_quantiles, digits = legend_round)),
                      rep(NA, 11),
                      max(round(my_quantiles, digits = legend_round))),
           fill = legend_colors,
           cex = 0.75,
           y.intersp = 0.5,
           border=NA,
           bty = "n",
           text.col = legend_text_col)
  } else{
    legend_colors <- colorRampPalette(my_colors)(11)
    legend(legend_pos, # position
           legend = c(min(round(my_quantiles, digits = legend_round)),
                      rep(NA, 4),
                      median(round(my_quantiles, digits = legend_round)),
                      rep(NA, 4),
                      max(round(my_quantiles, digits = legend_round))),
           fill = legend_colors,
           cex = 0.75,
           y.intersp = 0.5,
           border=NA,
           bty = "n",
           text.col = legend_text_col)
  }
  
  if(compass){
    compass_rose(x = -80.5 ,y = 30,rot=0,cex=0.25)
  }
}


#####
# DEFINE FUNCTION TO SUBSET ZIP BASED ON STATE
#####
sub_zip <- function(state = 'texas',
                    variable = 'non_complier',
                    add_title = TRUE){
  if(!state %in% usa$state){
    return(cat(paste0('That state aint right.  Pick from ',
                      paste0(usa$names, collapse = ', '))))
  } else {
    
    # Subset just for state
    state_name <- state
    state <- usa[which(usa$state == state),]

    # Get which zip codes are in that state
    # (this is how i did it before i had better state names)
#     temp <- over(zip,
#                  polygons(state))
#     state_zip <- zip[which(!is.na(temp)),]
    # (now it's more accurate to do this way:)
    state_zip <- zip[which(zip$state == state_name),]
    
    # ADD A BORDER
    plot(state, border = NA, col = 'lightgrey')
    
    # PLOT A CHOROPLETH OF THE DATA
    choro(shape = state_zip,
          var = state_zip@data[,variable],
          add = TRUE)
    

    
    # ADD A TITLE
    if(add_title){
      title(main = toupper(paste0(
        gsub('_', ' ', variable),
        ' in ',
        state_name)
      ))
      
    }
  }

}
