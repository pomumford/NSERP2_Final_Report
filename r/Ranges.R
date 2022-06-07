
snippet 
#####################################################################################
######### DERIVE SEAONSAL RANGES FROM BOTH PHASES USING ALL LOCATIONS ###############
#####################################################################################

#script based on C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\ANALYSES\\Spatial\\R\\KUDs_NSERP1&2_code\\r\\KUDs_NSERP1&2.rmd

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------
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



## ---- create base folders---------------------------------------------------------------------------------------------------------------------------------------
# Data Folder
dir.create(paste0(getwd(),"/data"))

# Scripts Folder
dir.create(paste0(getwd(),"/r"))

# Outputs Folder
dir.create(paste0(getwd(),"/output"))


## ---- PHASE 1---------------------------------------------------------------------------------------------------------------------------------------------------

####################### PHASE 1 ##############################


# This section will process the GPS data and write a cleaned .rds file of all the gps point and line data.
# Read in updated GPS datafile and clean it up ####
#  - Datafile in the format that comes straight from the Lotek Webservice

#
# pull NSERP1 locations from DB and clean
#

phase1.db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                            dbq= C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/DATA/DB/SapphireElkProject_ElkDatabase.accdb")

gps.dat1 <- sqlQuery(phase1.db, "SELECT * FROM LocationsAll") #CHECK PROCESSED VS RAW
gps.dat1 <- gps.dat1 %>% 
  #rename_all(list(~gsub("\\.*", "", .))) %>% # get rid of dots in name
  #rename(Date=DateTimeLocal, Lat=Latitude, Long=Longitude) %>%
  filter(Lat > 46) %>% filter(Long < -112) %>% filter(Long > -114.3) %>%   #Get rid bad fixes
  filter(DOP < 10) %>% 
  mutate(AnimalID=as.factor(AnimalID)) %>% 
  filter(!(AnimalID== "140070")) %>% # remove ind west of highway 93 not in sapphire pop, one additional male, but ranges on both sides of highway, at least for annual
    filter(!(AnimalID== "140380")) %>% 
      filter(!(AnimalID== "140650")) %>% 
        filter(!(AnimalID== "141090"))

# group_by(AnimalID) %>% 
# 

head(gps.dat1)

#ns.db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
#dbq=../../../../../DATA/DB/NSERP2/NSERP2.accdb") #shortened way choose file

animal.dat1 <- sqlQuery(phase1.db, "SELECT * FROM AnimalInfo")
animal.dat1 <- animal.dat1 %>%
  rename(CaptureDate=Date) %>%
  dplyr::select(AnimalID, CaptureDate, DeviceID, CaptureArea=Location, Sex) %>% 
  mutate(AnimalID=as.factor(AnimalID)) %>% 
  mutate(CaptureDate=as.Date(CaptureDate, format="%m/%d/%Y")) %>%
  mutate(DateDeployed=as.Date(CaptureDate, format="%m/%d/%Y")) %>% #add date deployed

  filter(!(AnimalID== "140070")) %>% # remove ind west of highway 93 not in sapphire pop
    filter(!(AnimalID== "140380")) %>% 
      filter(!(AnimalID== "140650")) %>% 
        filter(!(AnimalID== "141090")) 
           #filter(!(AnimalID== "141580")) #M all rifle season pts W of highway 93

head(animal.dat1)

#animal.dat1$Sex <-ifelse(animal.dat1$Sex == "M", "Male",
                         #ifelse(animal.dat1$Sex == "F", "Female", NA))

collar.dat1 <- sqlQuery(phase1.db, "SELECT * FROM SapphireCollars")
collar.dat1 <- collar.dat1 %>%
  rename(CollarID="ID & SN")

# collar.dat1$Sex <- 
#   ifelse(collar.dat1$Sex == "Male","Bull",
#                          ifelse(animal.dat1$Sex == "Female", "Cow", NA))


mort.dat1 <-sqlQuery(phase1.db, "SELECT * FROM MortalityInfo")
mort.dat1 <- mort.dat1 %>% 
  rename(AnimalID=`Animal ID`) %>% 
  dplyr::select(AnimalID, MortDate, TransEndDate, Cause1, Cause2, MortOwnrshp, HarvestSeason) %>%
  mutate(AnimalID=as.factor(AnimalID)) %>%
  mutate(DateMortality=as.Date(MortDate, format="%m/%d/%Y")) %>% 
  mutate(DateOff=as.Date(TransEndDate, format="%m/%d/%Y"))
head(mort.dat1)
write_rds(mort.dat1, "../../../../../Reporting/NSERP1/mortalityTable.rds")

odbcCloseAll()

#
#join table to compile necessary columns
#

# gps.dat1 <- left_join(gps.dat1, collar.dat1, by=c("DeviceID" = "CollarID")) %>% 
#   # Make date/time format correct
#   
#   mutate(Date=as.POSIXct(Date*(60*60*24), origin="1899-12-30", tz="GMT"), #tz="MST" would subtract 7 hours, GMT keeps it as is, which is already corrected
#          Time=format(Date, "%H:%M:%S"),
#          Date=as.Date(Date, tz="GMT")) %>% 
#          mutate(AnimalID=as.factor(AnimalID)) %>% 
#   # retain desired fields
#   dplyr::select(AnimalID, Date, Time, Long, Lat, DateDeployed, DateOff)

#Add sex from animal info table
gps.dat1 <- left_join(gps.dat1, animal.dat1, by=c("AnimalID" = "AnimalID"))  
  
gps.dat1 <- gps.dat1 %>% 
  dplyr::select(AnimalID, Date, Time, DateTime, Sex.x, Long, Lat, DateDeployed.y) %>% 
  rename(Sex=Sex.x) %>% 
  rename(DateDeployed=DateDeployed.y)

#join gps dat and mort dat to get DateOff

gps.dat1 <- left_join(gps.dat1, mort.dat1, by=c("AnimalID" = "AnimalID")) %>% 
    dplyr::select(AnimalID, Date, Time, DateTime, Sex, Long, Lat, DateDeployed, DateOff)


#lubridate format

gps.dat1$DateTime <- ymd_hms(gps.dat1$DateTime) %>% 
  with_tz(tzone = "GMT")
  
gps.dat1$Date <- ymd(gps.dat1$Date) %>% 
  with_tz(tzone = "GMT")
  #lubridate so just date and not empty times

gps.dat1$DateDeployed <- ymd(gps.dat1$DateDeployed) %>% 
  with_tz(tzone = "GMT")

gps.dat1$DateOff <- ymd(gps.dat1$DateOff) %>% 
  with_tz(tzone = "GMT")

# Filter each animals gps data to the start and end date 

  gps.dat1 <- gps.dat1 %>% 
  group_by(AnimalID) %>%
 filter(Date > DateDeployed,
         Date < DateOff) %>%
  dplyr::select(AnimalID, Date, Time, DateTime, Sex, Long, Lat)
head(gps.dat1)


# - create a spatial data frame using the sf package
gps_sf1 <- gps.dat1 %>% 
  st_as_sf(coords = c("Long", "Lat"), remove=F, crs = 4326) %>% # wgs84
  st_transform("+init=EPSG:32100") %>% #MT state plane
  #st_transform(mtstateplane) %>% #  project it to the MT state plane
  #mutate(DateTime=as.POSIXct(paste(Date, Time, sep=" "), tz="GMT")) %>% # already did with lubridate above
  ungroup() %>% 
  mutate(UTME = st_coordinates(.)[,1], # add UTMS
         UTMN = st_coordinates(.)[,2]) %>% 
  arrange(AnimalID, DateTime) #order rows by values of a column low to high

# Define seasons ####
#  - Winter: Dec 1 - Mar 31
#  - Spring: Apr 1 - Jun 30
#  - Summer: Jul 1 - Aug 31
#  - Fall: Sep 1 - Nov 30
gps_sf1 <- gps_sf1 %>%
  mutate(MonthDay=format(as.Date(Date), "%m%d"),
         Season=ifelse(MonthDay >= "1201" | MonthDay <= "0331", "WINTER",
                       ifelse(MonthDay >= "0401" & MonthDay <= "0630", "SPRING",
                              ifelse(MonthDay >= "0701" & MonthDay <= "0831", "SUMMER",
                                     ifelse(MonthDay >= "0901" & MonthDay <= "1130", "FALL",  
                                            NA))))) %>%
dplyr::select(-MonthDay) %>% #remove field
 
  mutate(Hunt.Season= ifelse(Date >= "2014-10-25" & Date <= "2014-11-30", "Rifle",
                        ifelse(Date >= "2015-10-24" & Date <= "2015-11-29", "Rifle",
                               ifelse(Date >= "2014-09-06" & Date<= "2014-10-19","Archery",
                                      ifelse(Date >= "2015-09-05" & Date<= "2015-10-18","Archery",
                                             NA))) %>%     

  
summary(as.factor(gps_sf1$Season)) #check n for seasons
summary(as.factor(gps_sf1$RifleSeason)) #check n rifle seasons


# Write gps data ####
# Write .rds file
write_rds(gps_sf1, "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/ns1_gps_data.rds")

# Write shapefile 
st_write(gps_sf1, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1", layer="ns1_gps_data.shp", driver="ESRI Shapefile", update=T, delete_layer=T)


# Summary of the animals collared NEED TO FIX

gps.sum1 <- gps_sf1 %>% 
  st_set_geometry(NULL) %>% # remove geometry
  group_by(AnimalID, Sex) %>%
  summarise(FirstLocation = as.Date(min(Date)),
            LastLocation = as.Date(max(Date)),
            DaysCollected = as.numeric(LastLocation- FirstLocation),
            DaySinceToday = as.numeric(LastLocation- today)) 


NumberTelemetryLocations <- nrow(gps_sf1)
NumberTelemetryLocations # Collected a total of this many locations from all animals



## ---- PHASE 1 output--------------------------------------------------------------------------------------------------------------------------------------------
#This section will estimate and output individual animal seasonal and annual spatial data (points, kud, lines) into Reporting folders
# Create spatial data (points, lines, kud, etc) for individuals 

#  - Create folders to save outputs
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_Locs")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_Locs"))}
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_Lines")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_Lines"))}
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs"))}
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/RIFLESEASON")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/RIFLESEASON"))}

#pop level 

if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/NSERP1/Pop_KUDs")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/NSERP1/Pop_KUDs"))}
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/NSERP1/Pop_Lines")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/NSERP1/Pop_Lines"))}
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Pop_KUDs/RIFLESEASON")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Pop_KUDs/RIFLESEASON"))}


#  - Create spatial data for individual animals
gps_sf1 <- read_rds("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/ns1_gps_data.rds")
head(gps_sf1)

#TEST to see if fix date col, turn from chr to date
gps_sf1$Date <- as.Date(gps_sf1$Date, tz="GMT")


#
# Get number of locations by animal and rifle season combinations
#


gpslocs1<- gps_sf1%>% 
  group_by(AnimalID, RifleSeason)%>%
  summarise(n()) #56 unique animals with points during rifle, vs 76 total collared


gpslocs1<- gpslocs1[!is.na(gpslocs1$RifleSeason),]

gpslocs1$ID.Rifle<- paste(as.character(gpslocs1$AnimalID), gpslocs1$RifleSeason, sep=".")
gps_sf1$ID.Rifle<- paste(gps_sf1$AnimalID,gps_sf1$RifleSeason, sep=".")

gps_sf1Rifle <- gps_sf1 %>% 
    filter(ID.Rifle %in% gpslocs1$ID.Rifle) %>% 

   filter(!(AnimalID== "141580")) #male doesnt have points during rifle E of 93


#
#write all points for rifle seasons
#

    st_write(gps_sf1Rifle, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1", layer=paste("ns1", "All", "Rifle", "pts", sep="_"), driver="ESRI Shapefile", delete_layer=T)


#set ids for rifle season, which is combo of animal id and rifle season id
    
ids1Rifle<-unique(gps_sf1Rifle$ID.Rifle) 
#ids1Animal <- unique(gps_sf1Rifle$AnimalID)

# For loop creates/outputs for each animal: 
#   1) Points, 
#   2) Lines, 
#   3) Annual home ranges, & 
#   4) Seasonal home ranges

#i <-69 #for testing

#INCLUDE FOR-LOOP IN ALL 

#
# Output Rifle Season 95% KUDs as shapefile for each animal
# 


for (i in 1:length(ids1Rifle)) {    
  # Subset to only current animal from list of animals
  riflepts1 <- gps_sf1Rifle %>% 
    filter(ID.Rifle==ids1Rifle[i]) #AnimalID for annual outputs
  
  sex1 <-  unique(riflepts1$Sex) #get sex and retain in shp
    ids1Animal <- unique(riflepts1$AnimalID)
      RifleSeason1 <- unique(riflepts1$RifleSeason) 
 
  
    st_write(riflepts1, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_Locs/RIFLESEASON", layer=paste("ns1", ids1Rifle[i], "Rifle", "pts", sep="_"), driver="ESRI Shapefile", delete_layer=T)


    
#RifleSn2 <- as.character(na.omit(unique(pts2$RifleSeason))) #%>% #as.character cleans na.omit func

 #use if animal does not have locs during rifle season
  #if (rlang::is_empty(RifleSn2)) next 
#  if (length(RifleSn2>0))
    

#j <- 2 #for testing

    # ssn.pts2 <- pts2 %>%
    #   filter(RifleSeason==RifleSn2[j]) %>%
    #   group_by(Date) %>%  # Group locations by elk by day
    #   sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
    #   as_Spatial() # turn to sp bc kernelUD func requires sp format

    riflepts1 <- riflepts1 %>%
    #  filter(RifleSeason==RifleSn2[j]) %>%
      group_by(Date) %>%  # Group locations by elk by day
      sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
      as_Spatial() # turn to sp bc kernelUD func requires sp format

    # KUD (kernel utilization distribution) and define 95% vol contour
    kud <- kernelUD(riflepts1, h="href", grid = 2000, kern="epa")
    vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")

    vol95 <- vol95 %>%
    st_as_sf() %>% # convert to sf
    mutate(AnimalID=ids1Animal, Sex=sex1, RifleSeason=RifleSeason1) # attach animal id, sex, and rifle season so can filter by rifle season later
    
  # mutate(AnimalID=ids1[i]) # attach animal id
  
  #kud_vol95 <- mask(raster(kud), vol95)   # clip KUD raster to 95% volume contour
    
  #if want to write raster
    # writeRaster(kud_vol951, filename=paste("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/ANNUAL", "ANNUAL", ids1[i], "_kud_vol95", sep=""), format="GTiff", overwrite=TRUE)

    # write out
    # writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON", layer = paste("ns2", RifleSn2[j], ids2[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE) #with RfielSn2[j]
    
 #     writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/RIFLESEASON", layer = paste("ns1", ids1Rifle[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
 # }
 st_write(vol95, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/RIFLESEASON", layer=paste("ns1", ids1Rifle[i], sex1,"vol95", sep="_"), driver="ESRI Shapefile", update=T, delete_layer=T)
 }

#
# POP Rifle season KUD *****
#

gps_sp1Rifle <- gps_sf1Rifle%>% 
  group_by(Date) %>%  # Group locations by elk by day
      sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
        filter(!(AnimalID== "141580")) %>% 
      as_Spatial()

    kud <- kernelUD(gps_sp1Rifle, h="href", grid = 5000, kern="epa")
    vol100 <- getverticeshr(kud, percent = 100, ida = NULL, unin = "m", unout = "km2")
    vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
    kud.raster <- raster(kud) # Turn KUD into raster
    kud95 <- mask(kud.raster, vol95)  # and clip to volume contour

    
    
    # write 100%
    
     writeOGR(vol100, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Pop_KUDs/RIFLESEASON", layer = paste("ns1", "All", "Rifle", "vol100", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
     writeRaster(vol100, filename="C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting//NSERP1//Pop_KUDs//RIFLESEASON_kud_vol100", format="GTiff", overwrite=TRUE)
    # write 95%
     writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Pop_KUDs/RIFLESEASON", layer = paste("ns1", "All", "Rifle", "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
     writeRaster(kud95, filename="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Pop_KUDs/RIFLESEASON/RIFLESEASON_kud_vol95", format="GTiff", overwrite=TRUE)



## ---- define rifle subpopulations for NSERP and NSERP2----------------------------------------------------------------------------------------------------------

#subpops derived from visual inspection of annual range centroids of each elk, obvious north/south groups

nserp1s <- st_read(here("data/1S.shp"))
nserp1n <- st_read(here("data/1N.shp"))
nserp2s <- st_read(here("data/2S.shp"))
nserp2n <- st_read(here("data/2N.shp"))


#range for rifle season from nersp 1 and 2

RifleFiles1 <- list.files("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/RIFLESEASON", pattern = "*shp$", full.names = T)
RifleCombined1   <- do.call(rbind, lapply(RifleFiles1, st_read)) # this applies st_read to all file paths listed in RifleFiles to read them all in, and then binds them all together into one geometry collection
  RifleCombined1 <- RifleCombined1 %>% 
    #slice(1:74) %>%  #duplicate shps, so subset by half
    filter(!(AnimalID== "140070")) %>% 
    filter(!(AnimalID== "140380")) %>% #filter individuals with homeranges on west side of highway 93 (not in study population)
    filter(!(AnimalID== "140650")) %>% 
    filter(!(AnimalID== "141090")) %>% 
    filter(!(AnimalID== "141580")) 
  
    
RifleFiles2 <- list.files("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON", pattern = "*shp$", full.names = T)
RifleCombined2 <- do.call(rbind, lapply(RifleFiles2, st_read))  # this applies st_read to all file paths listed in AnnualFiles to read them all in, and then binds them all together into one geometry collection


#
#match animal ids in sub pops with homeranges
#

pop1nRifle <- RifleCombined1 %>% 
  filter(AnimalID %in% nserp1n$AnimalID)

pop1sRifle <- RifleCombined1 %>% 
  filter(AnimalID %in% nserp1s$AnimalID)

pop2nRifle <- RifleCombined2 %>% 
  filter(AnimalID %in% nserp2n$AnimalID)

pop2sRifle <- RifleCombined2 %>% 
  filter(AnimalID %in% nserp2s$AnimalID)

#
#calc which individual homeranges intersect between sub pops
#


#make one polygon from all KUDs in sub pops
pop1nRiflecombined <- RifleCombined1 %>% 
  filter(AnimalID %in% nserp1n$AnimalID) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  mutate(TotalArea = st_area(.)) 

pop1sRiflecombined <- RifleCombined1 %>% 
  filter(AnimalID %in% nserp1s$AnimalID) %>% 
  st_union()%>% 
  st_as_sf() %>% 
  mutate(TotalArea = st_area(.))

pop2nRiflecombined <- RifleCombined2 %>% 
  filter(AnimalID %in% nserp2n$AnimalID) %>% 
  st_union()%>% 
  st_as_sf() %>% 
  mutate(TotalArea = st_area(.))

pop2sRiflecombined <- RifleCombined2 %>% 
  filter(AnimalID %in% nserp2s$AnimalID) %>% 
  st_union()%>% 
  st_as_sf() %>% 
  mutate(TotalArea = st_area(.))

#
#write
#

#1north
st_write(pop1nRiflecombined, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Pop_KUDs/RIFLESEASON", layer=paste("pop2ncombined"), driver="ESRI Shapefile", update=T, delete_layer=T)

#1south
st_write(pop1sRiflecombined, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Pop_KUDs/RIFLESEASON", layer=paste("pop2scombined"), driver="ESRI Shapefile", update=T, delete_layer=T)

#2north
st_write(pop2nRiflecombined, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Pop_KUDs/RIFLESEASON", layer=paste("pop2scombined"), driver="ESRI Shapefile", update=T, delete_layer=T)

#2south
st_write(pop2sRiflecombined, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Pop_KUDs/RIFLESEASON", layer=paste("pop2scombined"), driver="ESRI Shapefile", update=T, delete_layer=T)

#intersectioin
st_write(intersect2, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/ANALYSES/Spatial/R/Calc_Overlap_NSERP1&2_Subpops/output", layer=paste("intersect2"), driver="ESRI Shapefile", update=T, delete_layer=T)

#nserp2 kud all
st_write(pop2all, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/ANALYSES/Spatial/R/Calc_Overlap_NSERP1&2_Subpops/output", layer=paste("pop2all"), driver="ESRI Shapefile", update=T, delete_layer=T)



## ---- define annual subpopulations------------------------------------------------------------------------------------------------------------------------------
#DID WHEN FIRST STARTED MASTERS (JAN 2020) BUT NO LONGER APPLICABLE
#KEEPING CODE TO SEE WHICH INDIVIDUALS CENSORED FROM POPULATION

# #load sub pop shp created from ArcGIS, centroids
# 
# nserp1s <- st_read(here("data/NSERP1&2_subpop_KUD95_shp/1S.shp"))
# nserp1n <- st_read(here("data/NSERP1&2_subpop_KUD95_shp/1N.shp"))
# nserp2s <- st_read(here("data/NSERP1&2_subpop_KUD95_shp/2S.shp"))
# nserp2n <- st_read(here("data/NSERP1&2_subpop_KUD95_shp/2N.shp"))
# 
# 
# #homeranges from nersp 1 and 2
# 
# AnnualsFiles1 <- list.files("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/ANNUAL", pattern = "*shp$", full.names = T)
# Annuals1   <- do.call(rbind, lapply(AnnualsFiles1, st_read)) # this applies st_read to all file paths listed in AnnualFiles to read them all in, and then binds them all together into one geometry collection
#   Annuals1 <- Annuals1 %>% 
#     slice(1:74) %>%  #duplicate shps, so subset by half
#     filter(!(AnimalID== "140070")) %>% 
#     filter(!(AnimalID== "140380")) %>% #filter individuals with homeranges on west side of highway 93 (not in study population)
#     filter(!(AnimalID== "140650")) %>% 
#     filter(!(AnimalID== "141090"))
#   
#     
# AnnualsFiles2 <- list.files("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/ANNUAL", pattern = "*shp$", full.names = T)
# Annuals2 <- do.call(rbind, lapply(AnnualsFiles2, st_read))  # this applies st_read to all file paths listed in AnnualFiles to read them all in, and then binds them all together into one geometry collection
# 
# 
# #
# #match animal ids in sub pops with homeranges
# #
# 
# pop1n <- Annuals1 %>% 
#   filter(AnimalID %in% nserp1n$AnimalID)
# 
# pop1s <- Annuals1 %>% 
#   filter(AnimalID %in% nserp1s$AnimalID)
# 
# pop2n <- Annuals2 %>% 
#   filter(AnimalID %in% nserp2n$AnimalID)
# 
# pop2s <- Annuals2 %>% 
#   filter(AnimalID %in% nserp2s$AnimalID)
# 
# #
# #calc which individual homeranges intersect between sub pops
# #
# 
# 
# #make one polygon from all KUDs in sub pops
# pop1ncombined <- Annuals1 %>% 
#   filter(AnimalID %in% nserp1n$AnimalID) %>% 
#   st_union() %>% 
#   st_as_sf() %>% 
#   mutate(TotalArea = st_area(.)) 
# 
# 
# pop1scombined <- Annuals1 %>% 
#   filter(AnimalID %in% nserp1s$AnimalID) %>% 
#   st_union()%>% 
#   st_as_sf() %>% 
#   mutate(TotalArea = st_area(.))
# 
# pop2ncombined <- Annuals2 %>% 
#   filter(AnimalID %in% nserp2n$AnimalID) %>% 
#   st_union()%>% 
#   st_as_sf() %>% 
#   mutate(TotalArea = st_area(.))
# 
# pop2scombined <- Annuals2 %>% 
#   filter(AnimalID %in% nserp2s$AnimalID) %>% 
#   st_union()%>% 
#   st_as_sf() %>% 
#   mutate(TotalArea = st_area(.))
# 
# #
# #nserp1
# #
# 
# #ind
# subpop1intersect <- st_intersection(pop1n, pop1s)
# plot(st_geometry(subpop1intersect))
# n_distinct(subpop1intersect$AnimalID)
# n_distinct(subpop1intersect[subpop1intersect$Sex == "Male",]$AnimalID) 
# unique(subpop1intersect[subpop1intersect$Sex == "Male",]$AnimalID)
# n_distinct(subpop1intersect[subpop1intersect$Sex == "Female",]$AnimalID) 
# unique(subpop1intersect[subpop1intersect$Sex == "Female",]$AnimalID)
# 
# #combined pop 95 KUD to measure overlap
# 
# intersect1 <- st_intersection(pop1ncombined,pop1scombined) %>%
#   st_as_sf() %>% 
#   mutate(intersect_area = st_area(.))  # create new column with shape area
#   
# pop1all <- Annuals1 %>% 
#   st_union()%>% 
#   st_as_sf() %>% 
#   mutate(TotalArea = st_area(.))
# 
#   
# intersectpct1 <- (intersect1$intersect_area)/(pop1all$TotalArea) #7%
# 
# #
# #write
# #
# 
# #north
# st_write(pop1ncombined, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/ANALYSES/Spatial/R/Calc_Overlap_NSERP1&2_Subpops/output", layer=paste("pop1ncombined"), driver="ESRI Shapefile", update=T, delete_layer=T)
# 
# #south
# st_write(pop1scombined, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/ANALYSES/Spatial/R/Calc_Overlap_NSERP1&2_Subpops/output", layer=paste("pop1scombined"), driver="ESRI Shapefile", update=T, delete_layer=T)
# 
# #intersectioin
# st_write(intersect1, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/ANALYSES/Spatial/R/Calc_Overlap_NSERP1&2_Subpops/output", layer=paste("intersect1"), driver="ESRI Shapefile", update=T, delete_layer=T)
# 
# #nserp2 kud all
# st_write(pop1all, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/ANALYSES/Spatial/R/Calc_Overlap_NSERP1&2_Subpops/output", layer=paste("pop1all"), driver="ESRI Shapefile", update=T, delete_layer=T)
# 
# 
# #
# # PHASE 2
# #
# 
# 
# subpop2intersect <- st_intersection(pop2n, pop2s)
# plot(st_geometry(subpop2intersect))
# n_distinct(subpop2intersect$AnimalID)
# n_distinct(subpop2intersect[subpop2intersect$Sex == "Male",]$AnimalID) 
# unique(subpop2intersect[subpop2intersect$Sex == "Male",]$AnimalID)
# n_distinct(subpop2intersect[subpop2intersect$Sex == "Female",]$AnimalID) 
# unique(subpop2intersect[subpop2intersect$Sex == "Female",]$AnimalID)
# 
# #combined pop 95 KUD to measure overlap
# 
# intersect2 <- st_intersection(pop2ncombined,pop2scombined) %>%
#   st_as_sf() %>% 
#   mutate(intersect_area = st_area(.))  # create new column with shape area
# 
# pop2all <- Annuals2 %>% 
#   st_union()%>% 
#   st_as_sf() %>% 
#   mutate(TotalArea = st_area(.))
# 
# 
# intersectpct2 <- (intersect2$intersect_area)/(pop2all$TotalArea) #19%
# 
# #
# #write
# #
# 
# #north
# st_write(pop2ncombined, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/ANALYSES/Spatial/R/Calc_Overlap_NSERP1&2_Subpops/output", layer=paste("pop2ncombined"), driver="ESRI Shapefile", update=T, delete_layer=T)
# 
# #south
# st_write(pop2scombined, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/ANALYSES/Spatial/R/Calc_Overlap_NSERP1&2_Subpops/output", layer=paste("pop2scombined"), driver="ESRI Shapefile", update=T, delete_layer=T)
# 
# #intersectioin
# st_write(intersect2, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/ANALYSES/Spatial/R/Calc_Overlap_NSERP1&2_Subpops/output", layer=paste("intersect2"), driver="ESRI Shapefile", update=T, delete_layer=T)
# 
# #nserp2 kud all
# st_write(pop2all, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/ANALYSES/Spatial/R/Calc_Overlap_NSERP1&2_Subpops/output", layer=paste("pop2all"), driver="ESRI Shapefile", update=T, delete_layer=T)



## ---- PHASE 1 output cont.--------------------------------------------------------------------------------------------------------------------------------------
#
#
#

#redefine gps_sf1 for other KUDs
     
gps_sf1 <- read_rds("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/ns1_gps_data.rds")

gps_sf1 <- gps_sf1 #%>% 
  #filter(!(AnimalID== "141580")) #remove male if needed, spent time both sides of 93

ids1 <- unique(gps_sf1$AnimalID) #This should be 40 if all collars downloaded


#i=24 #********** USE TO TEST IF PROBLEMS *********

#INCLUDE IN ALL FOR-LOOPS

for (i in 1:length(ids1)) {
  # Subset to only current animal from list of animals
  pts1 <- gps_sf1 %>%
    filter(AnimalID==ids1[i]) 
  
  sex1=unique(pts1$Sex) #get sex and retain in shp
  
    st_write(pts1, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_Locs", layer=paste("ns", ids1[i], "pts", sep="_"), driver="ESRI Shapefile", delete_layer=T)

#
# Output Rifle Season 95% KUDs as shapefile for each animal
# 
    
# RifleSn1 <- as.character(na.omit(unique(pts1$RifleSeason))) #as.character cleans na.omit func
# 
# #j <- 1 #for testing
# 
#  for (j in 1:length(RifleSn)){
#     ssn.pts1 <- pts1 %>%
#       filter(RifleSeason==RifleSn[j]) %>%
#       group_by(Date) %>%  # Group locations by elk by day
#       sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
#       as_Spatial() # turn to sp bc kernelUD func requires sp format
# 
#     # KUD (kernel utilization distribution) and define 95% & 50% vol contour
#     kud <- kernelUD(ssn.pts1, h="href", grid = 2000, kern="epa")
#     vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
# 
#     
#     # write out
#     writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/RIFLESEASON", layer = paste("ns", RifleSn[j], ids1[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
#   }}    
#     
#}
  #
  # 3) Output annual 95% KUDs as shapefile & raster for each animal
  #
  
  samp1 <- pts1 %>%
    group_by(Date) %>% # Group locations by elk by day
    sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
    as_Spatial() # turn to sp
  kud1 <- kernelUD(samp1, h="href", grid = 2000, kern="epa") # calculate kud, 1 signifies phase 1
  vol951 <- getverticeshr(kud1, percent = 95, ida = NULL, unin = "m", unout = "km")  #1 signifies 
  
#phase 1, Define 95% volume contour, extract area
  
  vol951 <- vol951 %>%
    st_as_sf() %>% # convert to sf
    mutate(AnimalID=ids1[i], Sex=sex1) # attach animal id and sex
  # mutate(AnimalID=ids1[i]) # attach animal id
  
  kud_vol951 <- mask(raster(kud1), vol951)   # clip KUD raster to 95% volume contour
#}  

  writeRaster(kud_vol951, filename=paste("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/ANNUAL", "ANNUAL", ids1[i], "_kud_vol95", sep=""), format="GTiff", overwrite=TRUE)
  st_write(vol951, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/ANNUAL", layer=paste("ANNUAL", ids1[i], "vol95", sep="_"), driver="ESRI Shapefile", update=T, delete_layer=T)
 
}   


#
# 4) Output seasonal 95% KUDs as shapefile for each animal
# 

 for (k in 1:length(unique(pts1$Season))){
    ssn.pts1 <- pts1 %>%
      filter(Season==unique(pts1$Season)[j]) %>%
      group_by(Date) %>%  # Group locations by elk by day
      sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
      as_Spatial() # turn to sp

    # KUD (kernel utilization distribution) and define 95% & 50% vol contour
    kud <- kernelUD(ssn.pts1, h="href", grid = 2000, kern="epa")
    vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")

    # write out
    writeOGR(vol95, dsn = "../Reporting/Ind_KUDs", layer = paste("ns", unique(pts1$Season)[k], ids[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
    
  }

#
# 5) Output Rifle Season 95% KUDs as shapefile for each animal
# 


#j <- 1 #for testing

 for (j in 1:length(unique(pts1$RifleSeason))){
    ssn.pts1 <- pts1 %>%
      filter(RifleSeason==unique(pts1$RifleSeason)[j]) %>%
      group_by(Date) %>%  # Group locations by elk by day
      sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
      as_Spatial() # turn to sp

    # KUD (kernel utilization distribution) and define 95% & 50% vol contour
    kud <- kernelUD(ssn.pts1, h="href", grid = 2000, kern="epa")
    vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")

    
    # write out
    writeOGR(vol95, dsn = "../Reporting/Ind_KUDs/RIFLESEASON", layer = paste("ns", unique(pts1$RifleSeason)[j], ids1[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
  }
#pop level

if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs"))}
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_Lines")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_Lines"))}

# Prepare data for annual & seasonal KUDs ####
gps_sf1 <- read_rds("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/ns1_gps_data.rds")
head(gps_sf1)
gps_sp1 <- gps_sf1 %>% 
  group_by(AnimalID, Date) %>% 
  sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
  as_Spatial() # turn to sp

#
# Annual KUD for the population ####
#

kud <- kernelUD(gps_sp1, h="href", grid = 1000, kern="epa")
vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
vol50 <- getverticeshr(kud, percent = 50, ida = NULL, unin = "m", unout = "km2")
kud.raster <- raster(kud) # Turn KUD into raster
kud95 <- mask(kud.raster, vol95)  # and clip to volume contour

# 
# # write out
writeOGR(vol95, dsn = "C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\Reporting\\NSERP1\\Pop_KUDs", layer = "ANNUAL_vol95", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(vol50, dsn = "C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\Reporting\\NSERP1\\Pop_KUDs", layer = "ANNUAL_vol50", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeRaster(kud95, filename="C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\Reporting\\NSERP1\\Pop_KUDs\\ANNUAL", format="GTiff", overwrite=TRUE)

# 
# Seasonal KUDs for the population ####
#

#  - Loop for seasonal ranges
ssns <- unique(gps_sp1$Season)

for (i in 1:length(ssns)){
  pts.sub <- subset(gps_sp1, Season==ssns[i])

  # KUD (kernel utilization distribution) and define 95% & 50% vol contour
  kud <- kernelUD(pts.sub, h="href", grid = 1000, kern="epa")
  vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
  vol50 <- getverticeshr(kud, percent = 50, ida = NULL, unin = "m", unout = "km2")
  kud.raster <- raster(kud) # Turn KUD into raster
  kud95 <- mask(kud.raster, vol95)  # and clip to volume contour

  # write out
  writeOGR(vol95, dsn = "C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\Reporting\\NSERP1\\Pop_KUDs", layer = paste(ssns[i], "95contour", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeOGR(vol50, dsn = "C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\Reporting\\NSERP1\\Pop_KUDs", layer = paste(ssns[i], "50contour", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeRaster(kud95, filename=paste("C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\Reporting\\NSERP1\\Pop_KUDs", paste(ssns[i], "kud95", sep="_"), sep="\\"), format="GTiff", overwrite=TRUE)
}




## ---- PHASE 2---------------------------------------------------------------------------------------------------------------------------------------------------

################## PHASE 2 #########################


# This section will process the GPS data and write a cleaned .rds file of all the gps point and line data.
# Read in updated GPS datafile and clean it up ####
#  - Datafile in the format that comes straight from the Lotek Webservice

gps.dat2 <- read.csv("C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\DATA\\Telemetry\\GPS_122121_ALL_NSERP2.CSV", head=T) #FINAL STUDY PERIOD DURATION, SEE CSV DATES

gps.dat2 <- gps.dat2 %>% 
  rename_all(list(~gsub("\\.*", "", .))) %>% # get rid of dots in name
  rename(Date=DateTimeLocal, Lat=Latitude, Long=Longitude) %>%
  filter(Lat > 46) %>% filter(Long < -112) %>% filter(Long > -114.3) %>%   #Get rid bad fixes
  filter(DOP < 10)
head(gps.dat2)

# Update animal info from dbase stored on shared drive
ns.db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                            dbq= C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/DATA/DB/NSERP2/NSERP2.accdb")
animal.dat2 <- sqlQuery(ns.db, "SELECT * FROM AnimalInfo")
animal.dat2 <- animal.dat2 %>%
  dplyr::select(AnimalID, CaptureDate, CollarID, Location=CaptureArea, Sex) %>% 
  mutate(AnimalID=as.factor(AnimalID)) %>% 
  mutate(CaptureDate=as.Date(CaptureDate, format="%m/%d/%Y"))
head(animal.dat2)

animal.dat2$Sex <-ifelse(animal.dat2$Sex == "M", "Male",
                        ifelse(animal.dat2$Sex == "F", "Female", NA))

collar.dat2 <- sqlQuery(ns.db, "SELECT * FROM CollarInfo")
collar.dat2 <- collar.dat2 %>%
  dplyr::select(AnimalID, CollarID, DateDeployed, DateOff) %>% 
  mutate(AnimalID=as.factor(AnimalID)) %>% 
  mutate(CaptureDate=as.Date(DateDeployed, format="%m/%d/%Y")) %>% #
  mutate(CaptureDate=as.Date(DateOff, format="%m/%d/%Y"))
head(collar.dat2)

odbcCloseAll()

# Link collar info to GPS data and clean it up
gps.dat2 <- left_join(gps.dat2, collar.dat2, by=c("DeviceID" = "CollarID")) %>% 
  # Make date/time format correct
  mutate(Date=as.POSIXct(Date, origin="1899-12-30", tz="GMT"), #tz="MST" would subtract 7 hours, GMT keeps it as is, which is already corrected
         Time=format(Date, "%H:%M:%S"),
         Date=as.Date(Date, tz="GMT")) %>% 
  # remove low accuracy locations
  filter(DOP < 10) %>% 
  filter(Lat > 0) %>% 
  filter(Long < -113) %>% #Filter out bad records with long = -112
  mutate(AnimalID=as.factor(AnimalID)) %>% 
  # retain desired fields
  dplyr::select(AnimalID, Date, Time, Long, Lat, DateDeployed, DateOff)

#Add sex from animal info table
gps.dat2 <- left_join(gps.dat2, animal.dat2, by=c("AnimalID" = "AnimalID")) %>% 
  dplyr::select(AnimalID, Date, Time, Sex, Long, Lat, DateDeployed, DateOff)

# Filter each animals gps data to the start and end date 
gps.dat2 <- gps.dat2  %>% 
  # Filter the GPS data for each animal to start the day after capture and end the day before the mortality.                
  group_by(AnimalID) %>%
  filter(Date > DateDeployed,
         Date < DateOff) %>%
  dplyr::select(AnimalID, Date, Time, Sex, Long, Lat)
head(gps.dat2)

#Inspect and filter out any bad locations manually with lines of code here

# - create a spatial data frame using the sf package
gps_sf2 <- gps.dat2 %>% 
  st_as_sf(coords = c("Long", "Lat"), remove=F, crs = 4326) %>% # wgs84
  st_transform("+init=EPSG:32100") %>% # ensure in MT state lane - may not be necessary depending on what projection your data already is in
  
  #st_transform(mtstateplane) %>% #  project it to the MT state plane
  mutate(DateTime=as.POSIXct(paste(Date, Time, sep=" "), tz="GMT")) %>% # create DateTime for creating lines
  ungroup() %>% 
  mutate(UTME = st_coordinates(.)[,1], # add UTMS
         UTMN = st_coordinates(.)[,2]) %>% 
  arrange(AnimalID, DateTime)
str(gps_sf2$DateTime)

# Define seasons ####
#  - Winter: Dec 1 - Mar 31
#  - Spring: Apr 1 - Jun 30
#  - Summer: Jul 1 - Aug 31
#  - Fall: Sep 1 - Nov 30
gps_sf2 <- gps_sf2 %>%
  mutate(MonthDay=format(Date, "%m%d"),
         Season=ifelse(MonthDay >= "1201" | MonthDay <= "0331", "WINTER",
                       ifelse(MonthDay >= "0401" & MonthDay <= "0630", "SPRING",
                              ifelse(MonthDay >= "0701" & MonthDay <= "0831", "SUMMER",
                                     ifelse(MonthDay >= "0901" & MonthDay <= "1130", "FALL",
   NA))))) %>% 
  
  dplyr::select(-MonthDay) %>%  # remove field

#rifle season, year specific  
  mutate(Hunt.Season= ifelse(Date >= "2019-10-26" & Date <= "2019-12-01", "Rifle",
                             ifelse(Date >= "2020-10-24" & Date <= "2020-11-29", "Rifle",
                                   ifelse(Date >= "2021-10-23" & Date <= "2020-11-28", "Rifle",
                                      ifelse(Date >= "2019-09-07" & Date<= "2019-10-20","Archery",
                                             ifelse(Date >= "2020-09-05" & Date<= "2020-10-18","Archery",
                                                    ifelse(Date >= "2021-09-04" & Date<= "2021-10-17","Archery",NA))))))) 

summary(as.factor(gps_sf2$Season)) #check n for seasons
summary(as.factor(gps_sf2$RifleSeason)) #check n rifle seasons
                    
                                         
# Write gps data ####
# Write .rds file
write_rds(gps_sf2, "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/ns2_gps_data.rds")

# Write shapefile 
st_write(gps_sf2, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting", layer="ns2_gps_data.shp", driver="ESRI Shapefile", update=T, delete_layer=T)


# This section creates a summary of gps location data and North Sapphire mortalities to date. Update the monthly report with these values
# Summarize location data ####
gps_sf2 <- read_rds("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/ns2_gps_data.rds")
head(gps_sf2)

# Identify 1st date, last date, & total days of data per individual
today<-Sys.time()
today=as.Date(today, format="%Y-%d-%m %H:%M:%S")

# Summary of the animals collared
gps.sum2 <- gps_sf2 %>% 
  st_set_geometry(NULL) %>% # remove geometry
  group_by(AnimalID, Sex) %>%
  summarise(FirstLocation = as.Date(min(Date)),
            LastLocation = as.Date(max(Date)),
            DaysCollected = as.numeric(LastLocation- FirstLocation),
            DaySinceToday = as.numeric(LastLocation- today)) 


NumberTelemetryLocations <- nrow(gps_sf2)
NumberTelemetryLocations # Collected a total of this many locations from the all animals



#This section will estimate and output individual animal seasonal and annual spatial data (points, kud, lines) into Reporting folders
# Create spatial data (points, lines, kud, etc) for individuals 
#  - Create folders to save outputs
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_Locs")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_Locs"))}
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_Lines")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_Lines"))}
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs"))}
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON"))}

#  load all gps points

gps_sf2 <- read_rds("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/ns2_gps_data.rds")
head(gps_sf2)

#ids2 <- unique(gps_sf2$AnimalID) #This should be 40 if all collars downloaded

#
# Get number of locations by animal and rifle season combinations
#


gpslocs2<- gps_sf2%>% 
  group_by(AnimalID, RifleSeason)%>%
  summarise(n())


gpslocs2<- gpslocs2[!is.na(gpslocs2$RifleSeason),] 

gpslocs2$ID.Rifle<- paste(as.character(gpslocs2$AnimalID), gpslocs2$RifleSeason, sep=".")
gps_sf2$ID.Rifle<- paste(gps_sf2$AnimalID,gps_sf2$RifleSeason, sep=".")

# gps_sf2<- gps_sf2 %>%
#   filter(ID.Rifle %in% gpslocs2$ID.Rifle)

gps_sf2Rifle<- gps_sf2 %>% 
  filter(ID.Rifle %in% gpslocs2$ID.Rifle)

#
#write all points for rifle seasons
#

st_write(gps_sf2Rifle, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting", layer=paste("ns2", "All", "Rifle", "pts", sep="_"), driver="ESRI Shapefile", delete_layer=T)


#set ids for rifle season, which is combo of animal id and rifle season id
    
#19709 only has 8 points during RIFLE19, can write KUD, returns error, screen out
    
ids2Rifle<-unique(gps_sf2Rifle$ID.Rifle)

#examine n locs for each ind during rifle

nrowInd2Rifle <- gps_sf2Rifle %>% 
    st_set_geometry(NULL) %>% # remove geometry
  group_by(ID.Rifle, Sex) %>% 
  summarise(numPts = n())
  #19709 and 20287 problematic since low n


#
# Output Rifle Season 95% KUDs as shapefile for each animal
# 

#i <-3 #for testing
#a <- 59    #testing 

for (a in 1:length(ids2Rifle)) {    
  # Subset to only current animal from list of animals
  riflepts2 <- gps_sf2Rifle %>% 
  filter(ID.Rifle==ids2Rifle[a]) #RifleID for annual outputs
  
  if (nrow(riflepts2)<24) {next} #only estimate ind with more than one day (24 locs/ day with 1 hour fix rate) of locs during rifle
    
    sex2 <-  unique(riflepts2$Sex) #get sex and retain in shp
    ids2Animal <- unique(riflepts2$AnimalID)
    RifleSeason2 <- unique(riflepts2$RifleSeason) 

  
  st_write(riflepts2, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_Locs/RIFLESEASON", layer=paste("ns2", ids2Rifle[a], sex2, "pts", sep="_"), driver="ESRI Shapefile", delete_layer=T)


    
#RifleSn2 <- as.character(na.omit(unique(pts2$RifleSeason))) #%>% #as.character cleans na.omit func

 #use if animal does not have locs during rifle season
  #if (rlang::is_empty(RifleSn2)) next 
#  if (length(RifleSn2>0))
    

#a <- 2 #for testing

    # ssn.pts2 <- pts2 %>%
    #   filter(RifleSeason==RifleSn2[j]) %>%
    #   group_by(Date) %>%  # Group locations by elk by day
    #   sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
    #   as_Spatial() # turn to sp bc kernelUD func requires sp format

    riflepts2 <- riflepts2 %>%
    #  filter(RifleSeason==RifleSn2[j]) %>%
      group_by(Date) %>%  # Group locations by elk by day
      sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
      as_Spatial() # turn to sp bc kernelUD func requires sp format

    # KUD (kernel utilization distribution) and define 95% vol contour
    kud <- kernelUD(riflepts2, h="href", grid = 10000, kern="epa")
    vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")

    vol95 <- vol95 %>%
    st_as_sf() %>% # convert to sf
    mutate(AnimalID=ids2Animal, Sex=sex2, RifleSeason=RifleSeason2) # attach animal id and sex

  #kud_vol95 <- mask(raster(kud), vol95)   # clip KUD raster to 95% volume contour
    
  #if want to write raster
    # writeRaster(kud_vol951, filename=paste("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/ANNUAL", "ANNUAL", ids1[i], "_kud_vol95", sep=""), format="GTiff", overwrite=TRUE)

    # write out
    # writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON", layer = paste("ns2", RifleSn2[j], ids2[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE) #with RfielSn2[j]
    
 #     writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/NSERP1/Ind_KUDs/RIFLESEASON", layer = paste("ns1", ids1Rifle[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
 # }
 st_write(vol95, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON", layer=paste("ns2", ids2Rifle[a], sex2,"vol95", sep="_"), driver="ESRI Shapefile", update=T, delete_layer=T)
 }


#
# POP Rifle season KUD 
#

gps_sp2Rifle <- gps_sf2Rifle%>% 
  group_by(Date) %>%  # Group locations by elk by day
      sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
      as_Spatial()

    kud <- kernelUD(gps_sp2Rifle, h="href", grid = 5000, kern="epa")
    vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
    vol100 <- getverticeshr(kud, percent = 100, ida = NULL, unin = "m", unout = "km2")
    kud.raster <- raster(kud) # Turn KUD into raster
    kud95 <- mask(kud.raster, vol95)  # and clip to volume contour

    
    # write out
    # writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/RIFLESEASON", layer = paste("ns2", RifleSn2[j], ids2[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE) #with RfielSn2[j]
    
     writeOGR(vol100, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Pop_KUDs/RIFLESEASON", layer = paste("ns2", "RifleAll", "vol100", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
 
     writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Pop_KUDs/RIFLESEASON", layer = paste("ns2", "RifleAll", "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
    writeRaster(kud95, filename="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Pop_KUDs/RIFLESEASON/RIFLESEASON_kud_vol95", format="GTiff", overwrite=TRUE)


  #
  # 3) Output annual 95% KUDs as shapefile & raster for each animal
  #
  
  samp <- pts %>%
    group_by(Date) %>% # Group locations by elk by day
    sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
    as_Spatial() # turn to sp
  kud <- kernelUD(samp, h="href", grid = 2000, kern="epa") # calculate kud
  vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km")  #Define 95% volume contour, extract area
  vol95 <- vol95 %>%
    st_as_sf() %>% # convert to sf
    mutate(AnimalID=ids[i],Sex=sex) # attach animal id
    kud_vol95 <- mask(raster(kud), vol95)   # clip KUD raster to 95% volume contour
#   writeRaster(kud_vol95, filename=paste("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/ANNUAL", "ANNUAL", ids[i], "_kud_vol95", sep=""), format="GTiff", overwrite=TRUE)
# st_write(vol95, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/ANNUAL", layer=paste("ANNUAL", ids[i], "vol95", sep="_"), driver="ESRI Shapefile", update=T, delete_layer=T)
# 
# }

    writeRaster(kud_vol95, filename=paste("C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/TEST", "ANNUAL", ids[i], "_kud_vol95", sep=""), format="GTiff", overwrite=TRUE)
    st_write(vol95, dsn="C:/Users/pm132031/Documents/UM/SAPPHIRE_PROJECT/Reporting/Ind_KUDs/TEST", layer=paste("ANNUAL", ids[i], "vol95", sep="_"), driver="ESRI Shapefile", delete_layer=T)
    
}

#
# 4) Output seasonal 95% KUDs as shapefile for each animal
# 

 for (j in 1:length(unique(pts$Season))){
    ssn.pts <- pts %>%
      filter(Season==unique(pts$Season)[j]) %>%
      group_by(Date) %>%  # Group locations by elk by day
      sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
      as_Spatial() # turn to sp

    # KUD (kernel utilization distribution) and define 95% & 50% vol contour
    kud <- kernelUD(ssn.pts, h="href", grid = 2000, kern="epa")
    vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")

    # write out
    writeOGR(vol95, dsn = "../Reporting/Ind_KUDs", layer = paste("ns", unique(pts$Season)[j], ids[i], "vol95", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
  }

#pop level

if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs"))}
if(!file.exists("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_Lines")) {dir.create(file.path("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_Lines"))}

# Prepare data for annual & seasonal KUDs ####
gps_sf <- read_rds("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/ns_gps_data.rds")
head(gps_sf)
gps_sp <- gps_sf %>% 
  group_by(AnimalID, Date) %>% 
  sample_n(5, replace = TRUE) %>% # Sample n locations per day per elk to reduce autocor
  as_Spatial() # turn to sp

#
# Annual KUD for the population ####
#

kud <- kernelUD(gps_sp, h="href", grid = 1000, kern="epa")
vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
vol50 <- getverticeshr(kud, percent = 50, ida = NULL, unin = "m", unout = "km2")
kud.raster <- raster(kud) # Turn KUD into raster
kud95 <- mask(kud.raster, vol95)  # and clip to volume contour

# 
# # write out
writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs", layer = "ANNUAL_vol95", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeOGR(vol50, dsn = "C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs", layer = "ANNUAL_vol50", driver="ESRI Shapefile", overwrite_layer=TRUE)
writeRaster(kud95, filename="C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting//Pop_KUDs//ANNUAL_kud_vol95", format="GTiff", overwrite=TRUE)

# 
# Seasonal KUDs for the population ####
#

#  - Loop for seasonal ranges
ssns2 <- unique(gps_sp2$Season)

for (i in 1:length(ssns2)){
  pts.sub2 <- subset(gps_sp2, Season==ssns2[i])

  # KUD (kernel utilization distribution) and define 95% & 50% vol contour
  kud <- kernelUD(pts.sub, h="href", grid = 1000, kern="epa")
  vol95 <- getverticeshr(kud, percent = 95, ida = NULL, unin = "m", unout = "km2")
  vol50 <- getverticeshr(kud, percent = 50, ida = NULL, unin = "m", unout = "km2")
  kud.raster <- raster(kud) # Turn KUD into raster
  kud95 <- mask(kud.raster, vol95)  # and clip to volume contour

  # write out
  writeOGR(vol95, dsn = "C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs", layer = paste(ssns2[i], "95contour", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeOGR(vol50, dsn = "C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs", layer = paste(ssns2[i], "50contour", sep="_"), driver="ESRI Shapefile", overwrite_layer=TRUE)
  writeRaster(kud95, filename=paste("C:/Users/pm132031/Documents/UM/SAPPIRE_PROJECT/Reporting/Pop_KUDs", paste(ssns2[i], "kud95", sep="_"), sep="\\"), format="GTiff", overwrite=TRUE)
}


