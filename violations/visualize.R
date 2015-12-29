#####
# THIS IS JUST AN EXAMPLE SCRIPT OF HOW ONE MIGHT GO ABOUT
# VISUALIZING EPA DATA
#####

library(dplyr)
library(rgdal)
library(maps)
library(sp)
library(rgeos)
library(RColorBrewer)
library(classInt)

#####
# LOAD IN THE rdata file created by read_in.R
#####
load('data/cleaned_data.RData')

#####
# SOURCE THE HELPER FUNCTIONS
#####
source('functions.R')


#####
# EXAMPLES
#####
sub_zip(state = 'florida',
        variable = 'non_complier')

sub_zip(state = 'vermont',
        variable = 'financial_review')


# A MAP OF EVERYTHING
pdf('~/Desktop/lots_of_maps.pdf',
    height = 11,
    width = 8.5)
par(mfrow = c(2,2))
for (i in sort(usa$state)){
  for (j in c('violations', 'non_complier', 'financial_review', 'self_disclosure')){
    sub_zip(state = i,
            variable = j)
    cat(paste0(i, '--', j, '\n'))
  }
}
dev.off()