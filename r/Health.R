#   Script Details                                                          ####

# Author: Peter Mumford

# Date:2022-05-18 

# Purpose: Report elk body condition, pregnancy, and disease exposure

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####

#      Functions                                                            ####

#      Data                                                                 ####

###############################################################################


#####################################################################################
######### BODY CONDITION ############################################################
#####################################################################################



# Packages
library(RODBC)
library(tidyverse)
library(formattable)
library(ggplot2)
library(EnvStats)

# Set working directory
#    -  will find and set your directory from list of everyone's directories
wd <- list()
wd[1] <- "C:\\Users\\cf2752\\Desktop\\MASTER DATABASES\\Body Condition and Blood"
wd[2] <- "K:\\MASTER DATABASES\\Body Condition and Blood"
wd[3] <- "C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\DATA\\DB"

for (i in 1:length(wd)) { if (file.exists(wd[[i]])) { setwd(wd[[i]]) } }; getwd()

# Connect to Body Condition And Blood.accdb
locdb <- odbcDriverConnect("DRIVER=Microsoft Access Driver (*.mdb, *.accdb);
                             DBQ=./Other/Body Condition And Blood_12042020/Body Condition And Blood_12042020.accdb; 
                             ReadOnly=False")

elkdb <- odbcDriverConnect("DRIVER=Microsoft Access Driver (*.mdb, *.accdb);
                             DBQ=./NSERP2/NSERP2.accdb; 
                             ReadOnly=False")
#odbcCloseAll() # Closes connection to database


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~      SUMMARIZE & PLOT DATA          #####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# create folder
if(!file.exists("Summaries")) {dir.create(file.path(getwd(), "Summaries"))}

all.data <- sqlQuery(locdb, "SELECT * FROM BodyConditionData_PROCESSED") 

elk.data <- sqlQuery(elkdb, "SELECT * FROM AnimalInfo") #for age analyses

# Attach age & herd info

capt.data <- sqlQuery(locdb, "SELECT * FROM CaptureInfo")

all.data <- all.data %>% 
  left_join(capt.data, by="SampleID") 
# mutate(Pregnant=factor(Pregnant))

#clarify capture season bc only FALL and SPR in DB, no matter the date
all.data <- all.data %>%
  mutate(MonthDay=format(CaptureDate, "%m%d"),
         Season=ifelse(MonthDay >= "1201" | MonthDay <= "0331", "WINTER",
                       ifelse(MonthDay >= "0401" & MonthDay <= "0630", "SPRING",
                              ifelse(MonthDay >= "0701" & MonthDay <= "0831", "SUMMER",
                                     ifelse(MonthDay >= "0901" & MonthDay <= "1130", "FALL", NA))))) %>% 
  select(-(c(MonthDay, SEASON))) %>%  
  relocate(Season, .after = SampleID)

head(all.data)
str(all.data)

#-------------------------------#
####    Summary Tables       ####
#-------------------------------#

#
# > Summarise estimated and tooth age data ####
#

#
#can't figure out if/else statement on not to double count elk since some
#both Estage and Tooth age for capture

#if else, 

elk.data$AgeAll<- ifelse(is.na(elk.data$ToothAge), elk.data$EstAge, elk.data$ToothAge)

age <- elk.data %>%
  group_by(Sex) %>%
  filter (AgeAll > 1.5) %>% 
  summarise(No.elk = n_distinct(`AnimalID`),
            Avg.age = mean(AgeAll,, na.rm=T),
            SD.age = sd(AgeAll, na.rm=T),
            Med.age = median(AgeAll, na.rm=T),
            Min.age = min(AgeAll, na.rm=T),
            Max.age = max(AgeAll, na.rm=T))

View(age)

#
# > Summarise IFBF by CaptureEvent ####
#

# Define seasons ####
#  - Winter: Dec 1 - Mar 31
#  - Spring: Apr 1 - Jun 30
#  - Summer: Jul 1 - Aug 31
#  - Fall: Sep 1 - Nov 30



s1 <- all.data %>% 
  group_by(CaptureEvent) %>% 
  summarise(No.elk = n(),
            Avg.IFBF = mean(IFBF, na.rm=T),
            SD.IFBF = sd(IFBF, na.rm=T),
            Med.IFBF = median(IFBF, na.rm=T),
            Min.IFBF = min(IFBF, na.rm=T),
            Max.IFBF = max(IFBF, na.rm=T),
            Prop.preg = mean(Pregnant, na.rm=T)) %>% 
  filter(CaptureEvent %in% c("Sapphire 2014", "Sapphire 2019",
                             "Tobacco Roots 2014", "Tendoys 2019", "Blackfoot 2019", "EF 2013",
                             "Elkhorns 2015", "Madison 2018", 
                             "Ruby 2020", "WF 2013"))  #SW MT populations

View(s1)
formattable(s1)

# avg IFBF during other capture events in SW MT populations

s1.avgIFBF <- all.data %>% 
  filter(CaptureEvent %in% c("Sapphire 2014", "Tendoys 2019", "Madison 2018", 
                             "Blackfoot 2019","Ruby 2020")) %>%   #SW MT populations
  summarise(No.elk = n(),
            Avg.IFBF = mean(IFBF, na.rm=T),
            SD.IFBF = sd(IFBF, na.rm=T),
            Med.IFBF = median(IFBF, na.rm=T),
            Min.IFBF = min(IFBF, na.rm=T),
            Max.IFBF = max(IFBF, na.rm=T))

formattable(s1.avgIFBF)         
View(s1.avgIFBF)

#
# > Summarize IFBF by CaptureEvent and preg status ####
#

s2 <- all.data %>% 
  group_by(CaptureEvent, Pregnant) %>% 
  
  summarise(No.elk = n(),
            Avg.IFBF = mean(IFBF, na.rm=T),
            SD.IFBF = sd(IFBF, na.rm=T),
            Med.IFBF = median(IFBF, na.rm=T),
            Min.IFBF = min(IFBF, na.rm=T),
            Max.IFBF = max(IFBF, na.rm=T)) %>% 
  filter(CaptureEvent %in% c("Sapphire 2014", "Sapphire 2019",
                             "Tobacco Roots 2014", "Tendoys 2019", "Blackfoot 2019", "EF 2013",
                             "Elkhorns 2015", "Madison 2018", 
                             "Ruby 2020", "WF 2013")) %>% 
  ungroup()

View(s2)
formattable(s2)

#mean IFBF for preg vs non-preg

#preg

s2.preg <- all.data %>% 
  filter(Pregnant=="1") %>% 
  filter(CaptureEvent %in% c("Sapphire 2014", "Tendoys 2019", "Madison 2018", 
                             "Blackfoot 2019","Ruby 2020")) %>%   #SW MT populations
  summarise(No.elk = sum(n()),
            Avg.IFBF = mean(IFBF, na.rm=T),
            Med.IFBF = median(IFBF, na.rm=T),
            Min.IFBF = min(IFBF, na.rm=T),
            Max.IFBF = max(IFBF, na.rm=T))

formattable(s2.preg)
View(s2.preg)

#non-preg

s2.nonpreg <- all.data %>% 
  #group_by(CaptureEvent) %>% #if want to see how many Capture events
  filter(Pregnant=="0") %>% 
  filter(CaptureEvent %in% c("Sapphire 2014", "Tendoys 2019", "Madison 2018", 
                             "Blackfoot 2019","Ruby 2020")) %>%   #SW MT populations
  summarise(No.elk = sum(n()),
            Avg.IFBF = mean(IFBF, na.rm=T),
            Med.IFBF = median(IFBF, na.rm=T),
            Min.IFBF = min(IFBF, na.rm=T),
            Max.IFBF = max(IFBF, na.rm=T))

formattable(s2.nonpreg)
View(s2.nonpreg)

s2table <- write.csv(s2, file = "C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\ANALYSES\\Other\\Body_Condition_Data_Code_12040202T214925Z-001\\Body Condition Data and Code\\Figures_and_tables\\s2.csv", row.names = FALSE)

#
# > Summarize IFBF by CaptureEvent and lactation status #### 
#

s3 <- all.data %>% 
  filter(SEASON=="FALL") %>% 
  group_by(CaptureEvent, LactStatus) %>% 
  summarise(No.elk = n(),
            Avg.IFBF = mean(IFBF, na.rm=T),
            SD.IFBF = sd(IFBF, na.rm=T),
            Med.IFBF = median(IFBF, na.rm=T),
            Min.IFBF = min(IFBF, na.rm=T),
            Max.IFBF = max(IFBF, na.rm=T)) %>% 
  filter(CaptureEvent %in% c("Sapphire 2019", "Sapphire 2014")) 

View(s3)

s3table <- write.csv(s3, file = "C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\ANALYSES\\Other\\Body_Condition_Data_Code_12040202T214925Z-001\\Body Condition Data and Code\\Figures_and_tables\\s3.csv", row.names = FALSE)

#
# > Summarize Pregnancy by CaptureEvent #### 
#

s4 <- all.data %>%
  group_by(CaptureEvent) %>% 
  #filter(CaptureEvent == c("Sapphire 2019")) %>% #comment out if want all populations
  filter(CaptureEvent %in% c("Sapphire 2014", "Sapphire 2019",
                             "Tobacco Roots 2014", "Tendoys 2019", 
                             "Blackfoot 2019","Blackfoot 2020", "EF 2013",
                             "Elkhorns 2015", "Madison 2018", 
                             "Ruby 2020", "WF 2012", "WF 2013")) %>% #SW MT populations
  filter(Season=="WINTER") %>% 
  filter(!is.na(Pregnant)) %>%
  #mutate(Prop.preg = mean(Pregnant, na.rm=T)) %>%
  summarise(No.elk = n(),
            Avg.preg = mean(Pregnant, na.rm=T),
            SD.preg = sd(Pregnant, na.rm=T)) %>% 
  ungroup()

View(s4)

s4table <- write.csv(s4, file = "C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\ANALYSES\\Other\\Body_Condition_Data_Code_12040202T214925Z-001\\Body Condition Data and Code\\Figures_and_tables\\s4.csv", row.names = FALSE)

#------------------------------------------------#
#--- Figure: Winter Body Fat by Capture Event ####
#------------------------------------------------#

cbPalette <- rep("#CCCCCC", nlevels(all.data$CaptureEvent))

p.ifbf.winter <- all.data %>% 
  filter(CaptureEvent %in% c("Sapphire 2014", "Sapphire 2019",
                             "Tendoys 2019", "Blackfoot 2019", 
                             "Madison 2018", 
                             "Ruby 2020")) %>% #SW MT populations
  filter(Season=="WINTER") %>%  
  #filter(CaptureEvent %in% c("Sapphire 2019", "Sapphire 2014")) %>% #comment out if want just Sapphire elk
  #mutate(CaptureEvent = factor(CaptureEvent, levels =   filter(CaptureEvent %in% c("Sapphire 2014", "Sapphire 2019",
  
  
  
  # ~ ~ plot ~ ~
  ggplot(aes(x=CaptureEvent, y=IFBF, fill=CaptureEvent)) + 
  geom_boxplot(lwd=0.75) + 
  scale_y_continuous(breaks=seq(0, 16, 2))  +   # Set y-axis limits from 2-12, specify tick marks every 2
  xlab(" ") + 
  ylab("Ingesta-free body fat (%)") + 
  theme(legend.position="none",
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))

p.ifbf.winter

# jpeg("Summaries\\IFBF_Winter_All.jpg", width=4, height=4, units="in", res=400, quality=100)
# p.ifbf.winter
# dev.off()

#------------------------------------------------#
#----- Figure: Winter Body Fat by Preg Status ####
#------------------------------------------------#

preg.cols <- c("#E69F00","#56B4E9") # Define colors for preg vs. not-preg

p.ifbf.preg.winter <- all.data %>% 
  #filter(CaptureEvent == c("Sapphire 2019")) %>% #comment out if want all populations
  filter(CaptureEvent %in% c("Sapphire 2014", "Sapphire 2019",
                             "Tendoys 2019", "Blackfoot 2019", 
                             "Madison 2018", 
                             "Ruby 2020")) %>% #SW MT populations
  filter(Season=="WINTER") %>% 
  filter(!is.na(Pregnant)) %>%
  mutate(Pregnant = as.factor(Pregnant)) %>% #fill by factor of pregnant
  
  # ~ ~ plot ~ ~
  ggplot(aes(x=CaptureEvent, y=IFBF, fill=Pregnant)) +
  geom_boxplot(lwd=0.75) + 
  scale_y_continuous(breaks=seq(0, 16, 2))  +   
  scale_fill_manual(values=preg.cols, breaks=c("0","1"),labels=c("No","Yes")) + # Turn 0 and 1 to No and Yes
  ylab("Ingesta-free body fat (%)") + 
  xlab(" ") + 	 
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

p.ifbf.preg.winter

#---------------------------------------------------#
#---  Figure: Winter Body Fat by Lactation Status  ####
#---------------------------------------------------#

lact.cols <- c("#E69F00","#56B4E9") # Define colors for lact'ing vs. non-lact'ing

p.ifbf.lact <- all.data %>% 
  #filter(CaptureEvent == "Sapphire 2019") %>% #comment out if want all populations
  filter(CaptureEvent %in% c("Sapphire 2014", "Sapphire 2019",
                             "Tendoys 2019", "Blackfoot 2019", 
                             "Madison 2018", 
                             "Ruby 2020")) %>% #SW MT populations
  
  filter(!is.na(LactStatus)) %>% 
  filter(LactStatus %in% c("Yes", "No")) %>% 
  filter(Season=="WINTER") %>% 
  
  
  # ~ ~ plot ~ ~
  ggplot(aes(x=CaptureEvent, y=IFBF, fill=LactStatus)) +
  geom_boxplot(lwd=0.75) + 
  scale_y_continuous(breaks=seq(0, 16, 2))  +   
  #scale_fill_manual(values=lact.cols) +
  ylab("Ingesta-free body fat (%)") + 
  xlab(" ") + 	 
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
p.ifbf.lact 

#---------------------------------------------------#
#---  Figure: Pregnancy by CaptureEvent  ####
#---------------------------------------------------#

cbPalette <- rep("#CCCCCC", nlevels(p.preg.winter$CaptureEvent))

p.preg.winter <- all.data %>%
  filter(CaptureEvent %in% c("Sapphire 2014", "Sapphire 2019",
                             "Tendoys 2019", "Blackfoot 2019", 
                             "Madison 2018", 
                             "Ruby 2020")) %>% #SW MT populations
  filter(Season=="WINTER") %>% 
  filter(!is.na(Pregnant)) %>% 
  group_by(CaptureEvent) %>% 
  summarise(Prop.preg = mean(Pregnant, na.rm=T), N= n()) %>%
  ungroup() 

# ~ ~ plot ~ ~
preg.fig <- ggplot(aes(x= CaptureEvent, y = Prop.preg), data = p.preg.winter) +
  geom_col(fill="dark blue", colour="black", width = 0.5) +
  geom_text(aes(label= N, y=0.1, colour="white"), show.legend=FALSE) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))  +   
  #scale_fill_manual(values=preg.cols, breaks=c("0","1"),labels=c("No","Yes")) + # Turn 0 and 1 to No and Yes
  ylab("Proportion Pregnant") + 
  xlab("") +  
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

preg.fig




############################################################################
#################### Survival Code #######################################

#Develop code to build survival dataset (I have code to run analysis and will do that and writeup)

#
#steps to build
#

#1connect to the NSERP2 DB

#2 pull from mort table, animalinfo 
#join tables

#3 animalID sex age causeofmort , capture date, mort date

#*********pull code from NSERP data summary************
#add sex age cap date

#4 define biological year
#june1 to May31

#5 assign "entry" and "exit" dates (when come into study and when leave "die" or )
#assign numbers to entry and exit, eg animal that lived for two years and one day, first year exit number is 365
#then exit for second year is also 365, and next year value would be 1 (=died on jun 1)
#DOUBLE CHECK

#5.a create and "event indication", 1 dead, 0 survived



snippet 
#####################################################################################
######### SEROLOGY  #################################################################
#####################################################################################

#########################################

#Serology data for NSERP2 12042020, DeVoe edits

#########################################

#
# load packages
#

library(tidyverse)
library(here)


#
# set wd
#

#here() #if use here() function

wd <- list()
wd[1] <- "D:\\FWP_DeVoe\\Others\\PeterMumford"
wd[2] <- "C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\ANALYSES\\Other
\\Body_Condition_Data_Code_12040202T214925Z-001\\Body Condition Data and Code"

for (i in 1:length(wd)) { if (file.exists(wd[[i]])) { setwd(wd[[i]]) } }; getwd()

#
#load data
#

blood <- read.csv(".\\data\\Sapphire_Serology_Data_12_4_2020.csv") 

str(blood)


#
# Results by disease
#


disease.results <- blood %>% 
  group_by(Disease, Exposure) %>%  
  summarise(Number.Exposed.Elk = n_distinct(`Field.ID`)) %>% 
  mutate(Number.Sampled = sum(Number.Exposed.Elk)) %>% 
  mutate(Estimated.Prevalence = Number.Exposed.Elk / Number.Sampled) %>% #need pos exposure
  #mutate(Estimated.Prevalence = (Number.Exposed.Elk[disease.results$Exposure=="Positive"] / Number.Sampled)) %>% #need pos exposure
  ungroup()

disease.results <- disease.results %>% 
  # Expand the dataset to include every type of disease and exposure (e.g., in effect creating a "Positive" attribute for every disease)
  expand(Disease = factor(Disease), Exposure = factor(Exposure)) %>% 
  # Join back on the original dataset by disease and exposure to get the exposure and prevalence data 
  #   (e.g., everywhere a new "Positive" attribute occurs, there is nothing to join from the original, so it is NA)
  full_join(disease.results, by = c("Disease", "Exposure")) %>% 
  # Now fill in those NA's - if no. of elk exposed is NA, put a 0, etc
  mutate(Number.Exposed.Elk = ifelse(is.na(Number.Exposed.Elk), 0, Number.Exposed.Elk),
         Estimated.Prevalence = ifelse(is.na(Estimated.Prevalence), 0, Estimated.Prevalence)) %>% 
  # To fill in the NA's for number sampled, just group by Disease and take the first value 
  #   (e.g., for each disease that was missing a "positive" attribute, take the first value within that disease,
  #    which will be the value from the "negative" row (it ignores values that are NA in the first() function unless every value is NA))
  group_by(Disease) %>% 
  mutate(Number.Sampled = first(Number.Sampled)) %>% 
  mutate(Estimated.Prevalence = sprintf("%0.3f", Estimated.Prevalence))

View(disease.results)

#
#write table
#

disease.table <- write.csv(disease.results, file = "C:\\Users\\pm132031\\Documents\\UM\\SAPPHIRE_PROJECT\\ANALYSES\\Other\\Body_Condition_Data_Code_12040202T214925Z-001\\Body Condition Data and Code\\Figures_and_tables\\disease.results.csv", row.names = FALSE)













