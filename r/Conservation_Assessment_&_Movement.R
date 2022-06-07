# Script Details                                                       ####

# Author: Kelly Proffitt and Peter Mumford

# Date:2022-05-18 

# Purpose: Report changes to hunter access, travel management, and recreation that occurred in study area.
  # Evaluate seasonal ranges and movement corridors of elk.

###############################################################################
# Library / Functions / Data                                           ####

# Library                                                              ####

# Functions                                                            ####

# Data                                                                 ####
 
###############################################################################

#load gps data and bind males and females, taken from "C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\DATA\\Telemetry\\GPS_122121_ALL_NSERP2.CSV"
write.csv(gps.sf.male, file="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP2_GPS_Data_ALL_MALE.csv", append =T, row.names = FALSE)
write.csv(gps.sf.female, file="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP2_GPS_Data_ALL_FEMALE.csv", append =T, row.names = FALSE)

snippet 
#####################################################################################
######### CREATE SEASONAL RANGES ####################################################
#####################################################################################

# Load PACKAGES

library(tidyverse) 
library(sf)
library(sp) #for kernel centroid estimate
library(raster)
library(rgdal)
library(adehabitatHR)
library(RODBC)
library(spatialEco)
library(ggmap) #requires development version of ggmap devtools::install_github("dkahle/ggmap")
library(gridExtra)
library(animation)
library(knitr)
library(kableExtra)
library(readr)
library(here)
library(mapview)
library(rlang)
library(lubridate)

#set WD

here()

# Data Folder
dir.create(paste0(getwd(),"/data"))

# Scripts Folder
dir.create(paste0(getwd(),"/r"))

# Outputs Folder
dir.create(paste0(getwd(),"/output"))


# NOTES
# 
# -date deployed is capture date in animal info table
# -date off is TransEndDate from MortalityInfo table
# 
# # This section will process the GPS data and write a cleaned .rds file of all the gps point and line data.
# # Read in updated GPS datafile and clean it up ####
# #  - Datafile in the format that comes straight from the Lotek Webservice
# 
# gps.dat2 <- read.csv("C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\DATA\\Telemetry\\GPS_122121_ALL_NSERP2.CSV", head=T) #FINAL STUDY PERIOD DURATION, SEE CSV DATES
# 
# gps.dat2 <- gps.dat2 %>% 
#   rename_all(list(~gsub("\\.*", "", .))) %>% # get rid of dots in name
#   rename(Date=DateTimeLocal, Lat=Latitude, Long=Longitude) %>%
#   filter(Lat > 46) %>% filter(Long < -112) %>% filter(Long > -114.3) %>%   #Get rid bad fixes
#   filter(DOP < 10)
# head(gps.dat2)
# 
# # Update animal info from dbase stored on shared drive
# ns.db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
#                             dbq= C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/DATA/DB/NSERP2/NSERP2.accdb")
# animal.dat2 <- sqlQuery(ns.db, "SELECT * FROM AnimalInfo")
# animal.dat2 <- animal.dat2 %>%
#   dplyr::select(AnimalID, CaptureDate, CollarID, Location=CaptureArea, Sex) %>% 
#   mutate(AnimalID=as.factor(AnimalID)) %>% 
#   mutate(CaptureDate=as.Date(CaptureDate, format="%m/%d/%Y"))
# head(animal.dat2)
# 
# animal.dat2$Sex <-ifelse(animal.dat2$Sex == "M", "Male",
#                          ifelse(animal.dat2$Sex == "F", "Female", NA))
# 
# collar.dat2 <- sqlQuery(ns.db, "SELECT * FROM CollarInfo")
# collar.dat2 <- collar.dat2 %>%
#   dplyr::select(AnimalID, CollarID, DateDeployed, DateOff) %>% 
#   mutate(AnimalID=as.factor(AnimalID)) %>% 
#   mutate(CaptureDate=as.Date(DateDeployed, format="%m/%d/%Y")) %>% #
#   mutate(CaptureDate=as.Date(DateOff, format="%m/%d/%Y"))
# head(collar.dat2)
# 
# odbcCloseAll()
# 
# # Link collar info to GPS data and clean it up
# gps.dat2 <- left_join(gps.dat2, collar.dat2, by=c("DeviceID" = "CollarID")) %>% 
#   # Make date/time format correct
#   mutate(Date=as.POSIXct(Date*(60*60*24), origin="1899-12-30", tz="GMT"), #tz="MST" would subtract 7 hours, GMT keeps it as is, which is already corrected
#          Time=format(Date, "%H:%M:%S"),
#          Date=as.Date(Date, tz="GMT")) %>% 
#   # remove low accuracy locations
#   filter(DOP < 10) %>% 
#   filter(Lat > 0) %>% 
#   filter(Long < -113) %>% #Filter out bad records with long = -112
#   mutate(AnimalID=as.factor(AnimalID)) %>% 
#   # retain desired fields
#   dplyr::select(AnimalID, Date, Time, Long, Lat, DateDeployed, DateOff)
# 
# #Add sex from animal info table
# gps.dat2 <- left_join(gps.dat2, animal.dat2, by=c("AnimalID" = "AnimalID")) %>% 
#   dplyr::select(AnimalID, Date, Time, Sex, Long, Lat, DateDeployed, DateOff)
# 
# # Filter each animals gps data to the start and end date 
# gps.dat2 <- gps.dat2  %>% 
#   # Filter the GPS data for each animal to start the day after capture and end the day before the mortality.                
#   group_by(AnimalID) %>%
#   filter(Date > DateDeployed,
#          Date < DateOff) %>%
#   dplyr::select(AnimalID, Date, Time, Sex, Long, Lat)
# head(gps.dat2)
# 
# #Inspect and filter out any bad locations manually with lines of code here
# 
# # - create a spatial data frame using the sf package
# gps_sf2 <- gps.dat2 %>% 
#   st_as_sf(coords = c("Long", "Lat"), remove=F, crs = 4326) %>% # wgs84
#   st_transform("+init=EPSG:32100") %>% # ensure in MT state lane - may not be necessary depending on what projection your data already is in
#   
#   #st_transform(mtstateplane) %>% #  project it to the MT state plane
#   mutate(DateTime=as.POSIXct(paste(Date, Time, sep=" "), tz="GMT")) %>% # create DateTime for creating lines
#   ungroup() %>% 
#   mutate(UTME = st_coordinates(.)[,1], # add UTMS
#          UTMN = st_coordinates(.)[,2]) %>% 
#   arrange(AnimalID, DateTime)
# 
# str(gps_sf2$DateTime)
# 
# # Define seasons ####
# #  - Winter: Dec 1 - Mar 31
# #  - Spring: Apr 1 - Jun 30
# #  - Summer: Jul 1 - Aug 31
# #  - Fall: Sep 1 - Nov 30
# gps_sf2 <- gps_sf2 %>%
#   mutate(MonthDay=format(Date, "%m%d"),
#          Season=ifelse(MonthDay >= "1201" | MonthDay <= "0331", "WINTER",
#                        ifelse(MonthDay >= "0401" & MonthDay <= "0630", "SPRING",
#                               ifelse(MonthDay >= "0701" & MonthDay <= "0831", "SUMMER",
#                                      ifelse(MonthDay >= "0901" & MonthDay <= "1130", "FALL",
#                                             NA))))) %>% 
#   
#   dplyr::select(-MonthDay) %>%  # remove field
#   
#   #rifle season, year specific  
#   mutate(MonthDayYear=format(as.Date(Date), "%Y-%m-%d"),
#          RifleSeason= ifelse(MonthDayYear >= "2019-10-26" & MonthDayYear <= "2019-12-01", "RIFLE19",
#                              ifelse(MonthDayYear >= "2020-10-24" & MonthDayYear <= "2020-11-29", "RIFLE20",
#                                     NA))) %>%     
#   dplyr::select(-MonthDayYear) # remove field
# 
# 
# summary(as.factor(gps_sf2$Season)) #check n for seasons
# summary(as.factor(gps_sf2$RifleSeason)) #check n rifle seasons
# 
# 
# # Write gps data ####
# # Write .rds file
# write_rds(gps_sf2, "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/ns2_gps_data.rds")
# 
# # Write shapefile 
# st_write(gps_sf2, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting", layer="ns2_gps_data.shp", driver="ESRI Shapefile", update=T, delete_layer=T)
# 
# 
# # This section creates a summary of gps location data and North Sapphire mortalities to date. Update the monthly report with these values
# # Summarize location data ####
# gps_sf2 <- read_rds("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/ns2_gps_data.rds")
# head(gps_sf2)
# 
# # Identify 1st date, last date, & total days of data per individual
# today<-Sys.time()
# today=as.Date(today, format="%Y-%d-%m %H:%M:%S")
# 
# # Summary of the animals collared
# gps.sum2 <- gps_sf2 %>% 
#   st_set_geometry(NULL) %>% # remove geometry
#   group_by(AnimalID, Sex) %>%
#   summarise(FirstLocation = as.Date(min(Date)),
#             LastLocation = as.Date(max(Date)),
#             DaysCollected = as.numeric(LastLocation- FirstLocation),
#             DaySinceToday = as.numeric(LastLocation- today)) 
# 
# 
# NumberTelemetryLocations <- nrow(gps_sf2)
# NumberTelemetryLocations # Collected a total of this many locations from the all animals
# 
# 
# 
# #This section will estimate and output individual animal seasonal and annual spatial data (points, kud, lines) into Reporting folders
# # Create spatial data (points, lines, kud, etc) for individuals 
# #  - Create folders to save outputs
# if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_Locs")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_Locs"))}
# if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_Lines")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_Lines"))}
# if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs"))}
# if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON"))}
# 
# #  load all gps points
# 
# gps_sf2 <- read_rds("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/ns2_gps_data.rds")
# head(gps_sf2)
# 
# #ids2 <- unique(gps_sf2$AnimalID) #This should be 40 if all collars downloaded
# 
# #
# # Get number of locations by animal and rifle season combinations
# #
# 
# 
# gpslocs2<- gps_sf2%>% 
#   group_by(AnimalID, RifleSeason)%>%
#   summarise(n())
# 
# gpslocs2<- gpslocs2[!is.na(gpslocs2$RifleSeason),] 
# 
# gpslocs2$ID.Rifle<- paste(as.character(gpslocs2$AnimalID), gpslocs2$RifleSeason, sep=".")
# gps_sf2$ID.Rifle<- paste(gps_sf2$AnimalID,gps_sf2$RifleSeason, sep=".")
# 
# # gps_sf2<- gps_sf2 %>%
# #   filter(ID.Rifle %in% gpslocs2$ID.Rifle)
# 
# gps_sf2Rifle<- gps_sf2 %>% 
#   filter(ID.Rifle %in% gpslocs2$ID.Rifle)
# 
# #
# #write all points for rifle seasons
# #
# 
# st_write(gps_sf2Rifle, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting", layer=paste("ns2", "All", "Rifle", "pts", sep="_"), driver="ESRI Shapefile", delete_layer=T)
# 
# 
# #set ids for rifle season, which is combo of animal id and rifle season id
# 
# #19709 only has 8 points during RIFLE19, can write KUD, returns error, screen out
# 
# ids2Rifle<-unique(gps_sf2Rifle$ID.Rifle)
# 
# #examine n locs for each ind during rifle
# 
# nrowInd2Rifle <- gps_sf2Rifle %>% 
#   st_set_geometry(NULL) %>% # remove geometry
#   group_by(ID.Rifle, Sex) %>% 
#   summarise(numPts = n())
# #19709 and 20287 problematic since low n
# 
# 
# #
# # Output Rifle Season 95% KUDs as shapefile for each animal
# # 
# 
# #i <-3 #for testing
# #a <- 59    #testing 
# 
# for (a in 1:length(ids2Rifle)) {    
#   # Subset to only current animal from list of animals
#   riflepts2 <- gps_sf2Rifle %>% 
#     filter(ID.Rifle==ids2Rifle[a]) #RifleID for annual outputs
#   
#   if (nrow(riflepts2)<24) {next} #only estimate ind with more than one day (24 locs/ day with 1 hour fix rate) of locs during rifle
#   
#   sex2 <-  unique(riflepts2$Sex) #get sex and retain in shp
#   ids2Animal <- unique(riflepts2$AnimalID)
#   RifleSeason2 <- unique(riflepts2$RifleSeason) 
#   
#   
#   st_write(riflepts2, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_Locs/RIFLESEASON", layer=paste("ns2", ids2Rifle[a], sex2, "pts", sep="_"), driver="ESRI Shapefile", delete_layer=T)
#   
#   
#   
#   #RifleSn2 <- as.character(na.omit(unique(pts2$RifleSeason))) #%>% #as.character cleans na.omit func
#   
#   #use if animal does not have locs during rifle season
#   #if (rlang::is_empty(RifleSn2)) next 
#   #  if (length(RifleSn2>0))
#   
#   
#   #a <- 2 #for testing
#   
#   # ssn.pts2 <- pts2 %>%
#   #   filter(RifleSeason==RifleSn2[j]) %>%
#   #   group_by(Date) %>%  # Group locations by elk by day
#   #   sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
#   #   as_Spatial() # turn to sp bc kernelUD func requires sp format
#   
#   riflepts2 <- riflepts2 %>%
#     #  filter(RifleSeason==RifleSn2[j]) %>%
#     group_by(Date) %>%  # Group locations by elk by day
#     sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
#     as_Spatial() # turn to sp bc kernelUD func requires sp format
#   
#   # KUD (kernel utilization distribution) and define 95% vol contour
#   kud <- kernelUD(riflepts2, h="href", grid = 10000, kern="epa")
#   vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
#   
#   vol95 <- vol95 %>%
#     st_as_sf() %>% # convert to sf
#     mutate(AnimalID=ids2Animal, Sex=sex2, RifleSeason=RifleSeason2) # attach animal id and sex
#   
#   #kud_vol95 <- mask(raster(kud), vol95)   # clip KUD raster to 95% volume contour
#   
#   #if want to write raster
#   # writeRaster(kud_vol951, filename=paste("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/ANNUAL", "ANNUAL", ids1[i], "_kud_vol95", sep=""), format="GTiff", overwrite=TRUE)
#   
#   # write out
#   # writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON", layer = paste("ns2", RifleSn2[j], ids2[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE) #with RfielSn2[j]
#   
#   #     writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/RIFLESEASON", layer = paste("ns1", ids1Rifle[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
#   # }
#   st_write(vol95, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON", layer=paste("ns2", ids2Rifle[a], sex2,"vol95", sep="_"), driver="ESRI Shapefile", update=T, delete_layer=T)
# }
# 
# 
# #
# # POP Rifle season KUD 
# #
# 
# gps_sp2Rifle <- gps_sf2Rifle%>% 
#   group_by(Date) %>%  # Group locations by elk by day
#   sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
#   as_Spatial()
# 
# kud <- kernelUD(gps_sp2Rifle, h="href", grid = 5000, kern="epa")
# vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
# vol100 <- getverticeshr(kud, percent = 100, ida = NULL, unin = "m", unout = "km2")
# kud.raster <- raster(kud) # Turn KUD into raster
# kud95 <- mask(kud.raster, vol95)  # and clip to volume contour
# 
# 
# # write out
# # writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON", layer = paste("ns2", RifleSn2[j], ids2[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE) #with RfielSn2[j]
# 
# writeOGR(vol100, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Pop_KUDs/RIFLESEASON", layer = paste("ns2", "RifleAll", "vol100", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
# 
# writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Pop_KUDs/RIFLESEASON", layer = paste("ns2", "RifleAll", "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
# writeRaster(kud95, filename="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Pop_KUDs/RIFLESEASON/RIFLESEASON_kud_vol95", format="GTiff", overwrite=TRUE)
# 
# 
# #
# # 3) Output annual 95% KUDs as shapefile & raster for each animal
# #
# 
# samp <- pts %>%
#   group_by(Date) %>% # Group locations by elk by day
#   sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
#   as_Spatial() # turn to sp
# kud <- kernelUD(samp, h="href", grid = 2000, kern="epa") # calculate kud
# vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km")  #Define 95% volume contour, extract area
# vol95 <- vol95 %>%
#   st_as_sf() %>% # convert to sf
#   mutate(AnimalID=ids[i],Sex=sex) # attach animal id
# kud_vol95 <- mask(raster(kud), vol95)   # clip KUD raster to 95% volume contour
# #   writeRaster(kud_vol95, filename=paste("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/ANNUAL", "ANNUAL", ids[i], "_kud_vol95", sep=""), format="GTiff", overwrite=TRUE)
# # st_write(vol95, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/ANNUAL", layer=paste("ANNUAL", ids[i], "vol95", sep="_"), driver="ESRI Shapefile", update=T, delete_layer=T)
# # 
# # }
# 
# writeRaster(kud_vol95, filename=paste("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/TEST", "ANNUAL", ids[i], "_kud_vol95", sep=""), format="GTiff", overwrite=TRUE)
# st_write(vol95, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/TEST", layer=paste("ANNUAL", ids[i], "vol95", sep="_"), driver="ESRI Shapefile", delete_layer=T)
# 
# }
# 
# #
# # 4) Output seasonal 95% KUDs as shapefile for each animal
# # 
# 
# for (j in 1:length(unique(pts$Season))){
#   ssn.pts <- pts %>%
#     filter(Season==unique(pts$Season)[j]) %>%
#     group_by(Date) %>%  # Group locations by elk by day
#     sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
#     as_Spatial() # turn to sp
#   
#   # KUD (kernel utilization distribution) and define 95% & 50% vol contour
#   kud <- kernelUD(ssn.pts, h="href", grid = 2000, kern="epa")
#   vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
#   
#   # write out
#   writeOGR(vol95, dsn = "../Reporting/Ind_KUDs", layer = paste("ns", unique(pts$Season)[j], ids[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
# }
# 
# #pop level
# 
# if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs"))}
# if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_Lines")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_Lines"))}
# 
# # Prepare data for annual & seasonal KUDs ####
# gps_sf <- read_rds("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/ns_gps_data.rds")
# head(gps_sf)
# gps_sp <- gps_sf %>% 
#   group_by(AnimalID, Date) %>% 
#   sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
#   as_Spatial() # turn to sp
# 
# #
# # Annual KUD for the population ####
# #
# 
# kud <- kernelUD(gps_sp, h="href", grid = 1000, kern="epa")
# vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
# vol50 <- getverticeshr(kud, percent = 50, ida = NULL, unin = "m", unout = "km2")
# kud.raster <- raster(kud) # Turn KUD into raster
# kud95 <- mask(kud.raster, vol95)  # and clip to volume contour
# 
# # 
# # # write out
# writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs", layer = "ANNUAL_vol95", driver="ESRI Shapefile", overwrite_layer=TRUE)
# writeOGR(vol50, dsn = "C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs", layer = "ANNUAL_vol50", driver="ESRI Shapefile", overwrite_layer=TRUE)
# writeRaster(kud95, filename="C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting//Pop_KUDs//ANNUAL_kud_vol95", format="GTiff", overwrite=TRUE)
# 
# # 
# # Seasonal KUDs for the population ####
# #
# 
# #  - Loop for seasonal ranges
# ssns2 <- unique(gps_sp2$Season)
# 
# for (i in 1:length(ssns2)){
#   pts.sub2 <- subset(gps_sp2, Season==ssns2[i])
#   
#   # KUD (kernel utilization distribution) and define 95% & 50% vol contour
#   kud <- kernelUD(pts.sub, h="href", grid = 1000, kern="epa")
#   vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
#   vol50 <- getverticeshr(kud, percent = 50, ida = NULL, unin = "m", unout = "km2")
#   kud.raster <- raster(kud) # Turn KUD into raster
#   kud95 <- mask(kud.raster, vol95)  # and clip to volume contour
#   
#   # write out
#   writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs", layer = paste(ssns2[i], "95contour", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
#   writeOGR(vol50, dsn = "C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs", layer = paste(ssns2[i], "50contour", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
#   writeRaster(kud95, filename=paste("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs", paste(ssns2[i], "kud95", sep="_"), sep="\\"), format="GTiff", overwrite=TRUE)
# }
# 
# ```

