library(dplyr)

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

# To save time, save an RData file - this will 
# allow us to load in rcra_eval much more quickly
rm(rcra_fac)

#####
# READ IN ZIP CODE SHAPEFILE
#####
library(rgdal)
# On ubuntu, you may need to install some dependencies first:
# http://stackoverflow.com/questions/15248815/rgdal-package-installation
setwd('../zip/')
zip <- readOGR('.', 'tl_2013_us_zcta510')

setwd('..')
save.image('cleaned_data.RData')
