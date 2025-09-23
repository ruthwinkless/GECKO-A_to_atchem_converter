#fraccode for after multiplier code

#thermal2 file
cat("INFO: Separating fractional products (thermal)", "\n")

fractemp = thermal2 %>% 
  mutate(left = word(equation,1,sep = "="),
         right = word(equation,2,sep = "=")) %>% 
  filter(grepl("\\.", right)) %>%                 #selects lines where the product of the reaction contains a decimal point
  #create a new row for each individual product
  separate_wider_delim(right,delim="+", names = c("A","B", "C", "D", "E", "F", "G", "H", "I", "J"), too_few = "align_start") %>% 
  pivot_longer(cols = c("A","B", "C", "D", "E", "F", "G", "H", "I", "J"), names_to = NULL, values_to = "right") %>% 
  drop_na() 

#separate the number and the chemical code
fractemp = fractemp  %>% 
  separate_wider_delim(right,delim=" G", names = c("frac","chemical"), too_few = "align_end") %>% 
  mutate(chemical = paste0("G",chemical))%>% 
  mutate(col_A = as.numeric(col_A)) %>%
  mutate(frac = as.numeric(frac)) 
  
#multiply each fraction by the rate constant (col_A) to represent the fraction of the "initial" reaction that creates that product
fractemp = fractemp %>% replace(is.na(.), 1) %>% 
  mutate(col_A = col_A * frac) %>% 
  mutate(col_A = signif(col_A, 4)) %>% 
  mutate(col_A = as.character(col_A)) %>% 
  mutate(equation = paste0(left, "= ", chemical)) %>% 
  select(-left,-frac,-chemical)

#select all other lines from thermal2 (lines with no decimal point)
nofractemp = thermal2 %>% 
  filter(!grepl("\\.", equation))

#combine the newly processed fractional data with the rest of the thermal code
thermal3 = full_join(fractemp, nofractemp, by = join_by(col_A, col_n, col_E, species, equation))

#
#
#
#HV2 file
cat("INFO: Separating fractional products (photolysis)", "\n")

fractemp = HV2 %>% 
  mutate(left = word(equation,1,sep = "="),
         right = word(equation,2,sep = "=")) %>% 
  filter(grepl("\\.", right)) %>% 
  separate_wider_delim(right,delim="+", names = c("A","B", "C", "D", "E", "F", "G", "H", "I", "J"), too_few = "align_start") %>% 
  pivot_longer(cols = c("A","B", "C", "D", "E", "F", "G", "H", "I", "J"), names_to = NULL, values_to = "right") %>% 
  drop_na() 

fractemp = fractemp  %>% 
  separate_wider_delim(right,delim=" G", names = c("frac","chemical"), too_few = "align_end") %>% 
  mutate(chemical = paste0("G",chemical))%>% 
  mutate(mult = as.numeric(mult)) %>%
  mutate(frac = as.numeric(frac)) 

fractemp = fractemp %>% replace(is.na(.), 1) %>% 
  mutate(mult = mult * frac) %>% 
  mutate(mult = signif(mult, 4)) %>% 
  mutate(mult = as.character(mult)) %>% 
  mutate(equation = paste0(left, "= ", chemical)) %>% 
  select(-left,-frac,-chemical)


nofractemp = HV2 %>% 
  filter(!grepl("\\.", equation))

HV3 = full_join(fractemp, nofractemp, by = join_by(equation, label, mult))
