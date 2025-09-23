#do the maths and last bit of formatting
cat("INFO: Calculating single rate constants", "\n")

mcm_thermal = thermal3 %>% 
  mutate(col_A = as.numeric(col_A)) %>% 
  mutate(col_n = as.numeric(col_n)) %>% 
  mutate(col_E = as.numeric(col_E)) %>% 
  mutate(rate = col_A * (temp**col_n) * exp(-col_E/temp)) %>% 
  mutate(rate = signif(rate, 4)) %>% 
  select(equation, rate, species) %>% 
  mutate(A="%", M = ":", Z =";") %>% 
  mutate(rate = as.character(rate)) %>% 
  mutate(output=paste(A,rate,species,M,equation,Z)) %>% 
  select(output)

#make the HV into mcm format, by looking up label references
#the unknown_HV file will tell you if there are any files not associated with mcm photolyis. 
#       If any of these reactions are important, add lines for them in the labels.csv file
cat("INFO: Matching mcm photolysis labels", "\n")

labels = read.csv("labels.csv") %>% 
  select(label, MCM) %>% 
  mutate(label = as.character(label))

unknown_HV = left_join(HV3, labels, by = "label") %>%  #if this gives "many-to-many" warning, fix. (check labels file for duplicates)
  filter(!grepl("j", MCM))

mcm_HV = left_join(HV3, labels, by = "label") %>%  #if this gives "many-to-many" warning, fix. (check labels file for duplicates)
  filter(grepl("j", MCM))

mcm_HV$MCM = str_replace_all(mcm_HV$MCM, fixed("j"), "J<") 

mcm_HV = mcm_HV %>% 
  mutate(A="%", B="*", M="> :", Z=";") %>% 
  mutate(output = paste(A,mult,B,MCM,M,equation,Z)) %>% 
  select(output) 

MCM = full_join(mcm_thermal, mcm_HV, by = "output")%>% 
  rename(V1 = output)