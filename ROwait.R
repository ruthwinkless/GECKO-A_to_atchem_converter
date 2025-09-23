if (mcmRO2 == TRUE) {
RO_to_wait = thermal %>% 
  filter(grepl("\\+ GNO \\= GNO2 \\+ 0\\.", equation)) %>% 
  mutate(left = word(equation,1,sep = "="),
         right = word(equation,2,sep = "=")) %>% 
  separate_wider_delim(right, delim="+", names = c("NO2","first_R"), too_many = "drop") %>% 
  separate_wider_delim(first_R, delim=" G", names = c(NA,"chemical"), too_few = "align_end") %>% 
  mutate(chemical = paste0("G",chemical)) %>% 
  mutate(equation = paste0(left, "= ", NO2, " + ",chemical)) %>% 
  select(-NO2, -chemical, -left)


thermal = thermal %>% 
  filter(!grepl("\\+ GNO \\= GNO2 \\+ 0\\.", equation))

thermal = full_join(thermal, RO_to_wait, by = join_by(equation, col_A, col_n, col_E,species))

}
