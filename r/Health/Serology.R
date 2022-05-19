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

