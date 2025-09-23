library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

#Pre-flight checks:
#The GECKO-A mechanism was generated treating all peroxy radicals as PERO1
#The files contained in the "OUT" directory of GECKO-A are in a folder labeled with their VOC in this location

#set these to desired values

setwd("G:/Shared drives/RKW_Photolysis/Model_testing/GECKOA")
VOC = "butanone_IUPACF_nopero"

temp = 297 #set the temp of your system (must be same as in GECKO-A mechanism generation)
M = 2.46E19 #set the "pressure" in molecules/cm3. Used to calculate EXTRA and FALLOFF rates
mcmRO2 = T # if you want to treat RO2 + NO -> products in the same way as the mcm, set to TRUE


#main code 
source("unwrap_lines.R")      #imports data and fixes lines that contain unwanted line breaks
source("label_handling.R")    #handles FALLOFF, EXTRA, HV, TBODY, PERO1, OXYGEN geckoA labels in an mcm manner
source("ROwait.R") 
source("multipliers.R")       #changes integer amounts of chemicals more than 1 to that many separate occurrences of that chemical
source("fractions.R")         #creates separate equations with their own rates when a reaction has a fractional product
source("maths_HVmatch.R")     #calculates rates based on inputs, and matches geckoA HV labels to their mcm counterparts

unknown_HV

#Check:
#consider if the reactions in unknown_HV need to be sorted out (these are not included in output)
#

#Peroxy sum and variable list ----
pero1 = read.csv( paste0(VOC, "/pero1.dat"), header = F) %>% #read in GECKO-A's peroxy radical list (assuming all ro2 are in pero1)
  slice(-1)

NoN = pero1 %>% 
  filter(!grepl("N|V|P|records", V1)) #remove species that are N-containing
#the mcm uses only non-nitrated peroxy radicals for it's peroxy radical sum, but GECKO includes them

dictionary = read.fwf(paste0(VOC, "/dictionary.out"), widths = c(7)) %>% 
  filter(row_number() <= n()-1) %>% 
  slice(-1) %>% 
  mutate(V1 = paste0("G", V1))

VAR = str_squish(paste(dictionary$V1, collapse = " "))
RO2 = paste(NoN$V1,  collapse = " + ")


#write outputs----

template = data.frame(V1= c("VARIABLE", VAR, ";", "RO2 = ", RO2, ";", "*Reaction;"))
OUT = full_join(template, MCM, by = "V1")

write.table(OUT, file = paste0(VOC,"allROwait.fac"), row.names = F, quote = F , col.names = F)

