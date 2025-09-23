#Put reaction data into a neat dataframe-----------------------
df = data.frame(RAW_GECKO)

reactions = separate(df, RAW_GECKO, into = c("equation","col_A","col_n","col_E"), sep = c(-24, -13, -8))

GECKO = reactions[5:(nrow(reactions)-1),] %>%   #at 73 we cut off inorganic reactions, for the full file use 5:(nrow(reactions)-1)
  mutate(across(everything(), as.character))

GECKO$equation = str_replace_all(GECKO$equation, fixed(">"), "")
GECKO$equation = str_replace_all(GECKO$equation, fixed("NOTHING"), "")

cat("INFO: Splitting data by label", "\n")

#pull HV reactions and treat them seperately------------------
HV = GECKO %>% 
  filter(grepl("HV", equation)) %>% 
  select(equation)

HVE = GECKO %>% 
  unite(col = "all", sep = "") %>% 
  filter(grepl("HV /", all)) 
HVE = separate_wider_position(HVE ,all, widths = setNames(c(7,6,6), c(NA, "label", "multiplier")),too_many = "drop")

HV$label = HVE$label
HV$mult = HVE$multiplier
HV$equation = lapply(HV$equation, gsub, pattern = "+ HV", replacement = "", fixed = TRUE)
HV = HV %>% 
  mutate(across(everything(), as.character))

#pull FALLOFF and ISOM reactions to handle their labels---------
source("FandI labels.R")

#pull EXTRA reactions to handle each type of equation-------------

EXTRA = GECKO %>% 
  filter(grepl("\\+ EXTRA", equation)) %>% 
  mutate(species = "")%>%  
  mutate(across(equation, str_squish))

EXTRAE = GECKO %>% 
  unite(col = "all", sep = "") %>% 
  filter(grepl("EXTRA /", all)) 
EXTRAE = separate_wider_position(EXTRAE ,all, widths = setNames(c(10,3,100), c(NA, "Ecode", "detail")),too_few = "align_start") %>%  
  mutate(across(detail, str_squish))

EXTRA$Ecode = EXTRAE$Ecode
EXTRA$detail = EXTRAE$detail


for (i in 1:nrow(EXTRA)){
  if (EXTRA$Ecode[i] == "100") {EXTRA$species[i] = "*O2*M"} 
  if (EXTRA$Ecode[i] == "500") {EXTRA$species[i] = "*H2O"}
  
  if (EXTRA$Ecode[i] == "501") {cat("ignoring *EXTRA 501* reaction \n")}
  
  if (EXTRA$Ecode[i] == "550") {
    Eline = EXTRA[i,] %>% separate(detail, into = c("A2","n2","E2","A3","n3","E3",NA), sep = " ") %>% 
      select(-Ecode,-species,-equation) %>% 
      mutate_if(is.character, as.numeric) %>% 
      mutate(col_A = col_A * (temp**col_n) * exp(-col_E/temp)) %>% 
      mutate(A2 = A2 * (temp**n2) * exp(-E2/temp)) %>% 
      mutate(A3 = A3 * (temp**n3) * exp(-E3/temp)) %>% 
      mutate(k=col_A+A3*M/(1+A3*M/A2))%>% 
      mutate(k = signif(k, 4))
    EXTRA$col_A[i] = Eline$k
    EXTRA$col_n[i] = "0"
    EXTRA$col_E[i] = "0"
  }
}
  
EXTRA$equation = str_remove_all(EXTRA$equation, "\\+\\s*EXTRA")
EXTRA = EXTRA %>% 
  select(-Ecode,-detail)

#add Falloff, ISOM, and EXTRA reactions back in -------------------
BASIC =GECKO %>%                                    #unlabled reactions only
  filter(!grepl("FALLOFF|HV|EXTRA|ISOM", equation))%>% 
  filter(!grepl("FALLOFF|HV|EXTRA|ISOM|EX|XT|TR|RA", col_A)) %>%  
  mutate(across(equation, str_squish))


#stitch FandI and BASIC, as they're all now the same format, 
thermal = full_join(BASIC, FandI, by = join_by(equation, col_A, col_n, col_E)) %>% 
  mutate(species = "")
thermal = full_join(thermal, EXTRA, by = join_by(equation, col_A, col_n, col_E,species))


#add fac format equivalents for RO2 and O2 reactions
thermal$species[grepl("PERO1", thermal$equation)] <- "*RO2"
thermal$species[grepl("OXYGEN", thermal$equation)] <- "*O2"
thermal$species[grepl("TBODY", thermal$equation)] <- "*M"

thermal$equation = str_remove_all(thermal$equation, "\\+\\s*TBODY")
thermal$equation = str_remove_all(thermal$equation, "\\+\\s*OXYGEN")
thermal$equation = str_remove_all(thermal$equation, "\\+\\s*PERO1")




