#--- This script reads in EPA data, merges it appropriately,
#--- reads in some geographic files (states and zip codes), 
#--- and joins those to the EPA data.
#--- It's output is an RData file of cleaned data
#--- which can be used to more quickly load the above elements
#--- for the purpose of analysis of visualization,
#--- without having to run this (slow) script more than once.

library(dplyr)
library(rgdal)
# For rgdal on ubuntu, you may need to install some dependencies first:
# http://stackoverflow.com/questions/15248815/rgdal-package-installation
library(maps)
library(sp)
library(rgeos)
library(RColorBrewer)
library(classInt)

setwd('data/EPA')
rcra_fac <- read.csv("RCRA_FACILITIES.csv", stringsAsFactors=FALSE)

#Make lower case 
names(rcra_fac) <- tolower(names(rcra_fac))

# Recode rcra_fac variables 
# First recode full_full enforcement 
rcra_fac$full_enforcement_l <- ifelse(grepl("L", rcra_fac$full_enforcement), TRUE, FALSE)
rcra_fac$full_enforcement_i <- ifelse(grepl("I", rcra_fac$full_enforcement), TRUE, FALSE)
rcra_fac$full_enforcement_b <- ifelse(grepl("B", rcra_fac$full_enforcement), TRUE, FALSE)
rcra_fac$full_enforcement_s <- ifelse(grepl("S", rcra_fac$full_enforcement), TRUE, FALSE)
rcra_fac$full_enforcement_t <- ifelse(grepl("T", rcra_fac$full_enforcement), TRUE, FALSE)
rcra_fac$full_enforcement_h <- ifelse(grepl("H", rcra_fac$full_enforcement), TRUE, FALSE)
# New variable for full_enforcement universe or not
rcra_fac$full_enforcement <- gsub("\\s", "", rcra_fac$full_enforcement) 
rcra_fac$full_enforcement_fac <- ifelse(rcra_fac$full_enforcement != "------" & rcra_fac$full_enforcement != "-----H", TRUE, FALSE)

rcra_fac$active_site_h <- ifelse(grepl("H", rcra_fac$active_site), TRUE, FALSE)
rcra_fac$active_site_p <- ifelse(grepl("P", rcra_fac$active_site), TRUE, FALSE)
rcra_fac$active_site_a <- ifelse(grepl("A", rcra_fac$active_site), TRUE, FALSE)
rcra_fac$active_site_c <- ifelse(grepl("C", rcra_fac$active_site), TRUE, FALSE)
rcra_fac$active_site_s <- ifelse(grepl("S", rcra_fac$active_site), TRUE, FALSE)
# New variable for active site or not
rcra_fac$active_site_fac <- ifelse(rcra_fac$active_site != "-----", TRUE, FALSE)

rcra_fac$operating_tsdf_l <- ifelse(grepl("L", rcra_fac$operating_tsdf), TRUE, FALSE)
rcra_fac$operating_tsdf_i <- ifelse(grepl("I", rcra_fac$operating_tsdf), TRUE, FALSE)
rcra_fac$operating_tsdf_b <- ifelse(grepl("B", rcra_fac$operating_tsdf), TRUE, FALSE)
rcra_fac$operating_tsdf_s <- ifelse(grepl("S", rcra_fac$operating_tsdf), TRUE, FALSE)
rcra_fac$operating_tsdf_t <- ifelse(grepl("T", rcra_fac$operating_tsdf), TRUE, FALSE)
rcra_fac$operating_tsdf_h <- ifelse(grepl("H", rcra_fac$operating_tsdf), TRUE, FALSE)
# New variable for operating_tsdf universe or not 
rcra_fac$operating_tsdf <- gsub("\\s", "", rcra_fac$operating_tsdf) 
rcra_fac$operating_tsdf_fac <- ifelse(rcra_fac$operating_tsdf != "------" & rcra_fac$operating_tsdf != "-----H", TRUE, FALSE)

#Recode fed waste generator 
rcra_fac$fed_waste_generator <- ifelse(rcra_fac$fed_waste_generator == "", "unverified",
                                       ifelse(rcra_fac$fed_waste_generator =="1  ", "LQG",
                                              ifelse(rcra_fac$fed_waste_generator == "2  ", "SQG",
                                                     ifelse(rcra_fac$fed_waste_generator == "3  ", "CESQG", "None"))))

rcra_fac$fed_waste_generator <- as.factor(rcra_fac$fed_waste_generator)


# RCRA evaluations 
rcra_eval <- read.csv('RCRA_EVALUATIONS.csv', stringsAsFactors=FALSE)

#Make lower case 
names(rcra_eval) <- tolower(names(rcra_eval))

#merge inspect and eval 
# (joining on default dplyr takes all shared column names:
# in this case, id_number and activity location)
rcra_eval <- left_join(x = rcra_eval,
                       y = rcra_fac)

# Clean up zip codes by only taking the first 5 digits
rcra_eval$zip_code <- substr(rcra_eval$zip_code, 1, 5)
rm(rcra_fac)

#####
# READ IN ZIP CODE SHAPEFILE
#####
setwd('../zip/')
zip <- readOGR('.', 'tl_2013_us_zcta510')
zip$ZCTA5CE10 <- as.character(zip$ZCTA5CE10)

# READ IN POSTAL CODES EXTRA INFO
postal <- read.csv('../us_postal_codes.csv')
postal$Postal.Code <- as.character(postal$Postal.Code)
#####
# EXTRACT LONGITUDE AND LATITUDE FROM ZIP INTO RCRA_EVAL
#####
names(zip@data)[8:9] <- c('lat', 'lon')
zip$zip_code <- zip$ZCTA5CE10
zip_small <- zip@data[,c('zip_code', 'lat', 'lon')]
rcra_eval <- left_join(x = rcra_eval,
                       y = zip_small)

#####
# GROUP evaluations BY ZIP CODE
#####
by_zip <- rcra_eval %>%
  group_by(zip_code) %>%
  summarise(SNY = length(evaluation_type[evaluation_type == 'SNY']),
            SNN = length(evaluation_type[evaluation_type == 'SNN']),
            FRR = length(evaluation_type[evaluation_type == 'FRR']),
            Violation = length(found_violation[found_violation == 'Y  ']),
            CEI = length(evaluation_type[evaluation_type == 'CEI']),
            FCI = length(evaluation_type[evaluation_type == 'FCI']),
            NRR = length(evaluation_type[evaluation_type == 'NRR']))
# Add more variables here, within the summarise function above

#####
# BRING THE BY_ZIP DATA INTO ZIP
#####
zip@data$Postal.Code <- zip@data$zip_code
zip@data <- left_join(x = zip@data,
                      y = by_zip)

#####
# BRING THE POSTAL DATA INTO ZIP
#####

# Make both numeric before merge to account for 0s
postal$Postal.Code <- as.numeric(postal$Postal.Code)
zip@data$Postal.Code <- as.numeric(zip@data$Postal.Code)
zip@data <- left_join(x = zip@data,
                      y = postal)

# Make state lower case
zip$state <- tolower(zip$State)

# Replace NA's with 0
zip@data$SNY[which(is.na(zip@data$SNY))] <- 0
zip@data$FRR[which(is.na(zip@data$FRR))] <- 0
zip@data$Violation[which(is.na(zip@data$Violation))] <- 0
zip@data$CEI[which(is.na(zip@data$CEI))] <- 0
zip@data$SNN[which(is.na(zip@data$SNN))] <- 0
zip@data$FCI[which(is.na(zip@data$FCI))] <- 0
zip@data$NRR[which(is.na(zip@data$NRR))] <- 0



#####
# GET A SIMPLE USA MAP
#####
usa <- readOGR('../state', 'states')
usa$state <- tolower(usa$STATE_NAME)

# Clean up date
rcra_eval$date <- as.Date(rcra_eval$evaluation_start_date,
                          format = '%m/%d/%Y')

# Get year
rcra_eval$year <- as.numeric(format(rcra_eval$date, format = '%Y'))

# Get state abbreviations/names matched up in rcra_eval
abbreviations <- read.csv('http://www.fonz.net/blog/wp-content/uploads/2008/04/states.csv')
abbreviations$state <- tolower(abbreviations$State)
abbreviations$state_code <- abbreviations$Abbreviation
abbreviations <- abbreviations[,c('state', 'state_code')]
rcra_eval <- left_join(x = rcra_eval,
                       y = abbreviations)

# Now that we've got a common linker (state) in both 
# rcra_eval (dataframe) and usa (shapefile) we can m
# 1. group by state in rcra_eval
# 2. join with usa

# get statistics by state
by_state <- rcra_eval %>%
  group_by(state) %>%
  summarise(SNY = length(evaluation_type[evaluation_type == 'SNY']),
            FRR = length(evaluation_type[evaluation_type == 'FRR']),
            Violation = length(found_violation[found_violation == 'Y  ']),
            CEI = length(evaluation_type[evaluation_type == 'CEI']),
            FCI = length(evaluation_type[evaluation_type == 'FCI']),
            SNN = length(evaluation_type[evaluation_type == 'SNN']))
# join by_state to usa's data part (usa@data)
usa@data <- left_join(x = usa@data,
                      y = by_state)

# To save time, save an RData file - this will 
# allow us to load in rcra_eval much more quickly
setwd('..')
save.image('cleaned_data.RData')

