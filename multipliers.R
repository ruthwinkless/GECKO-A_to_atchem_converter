#Code written by Will Drysdale on 09/06/2025, minor edits made from original

#library(purrr)
#library(tidyr)
#library(stringr)

parse_component = function(meta){
  
  meta$component_out = meta$component
  
  # loop over each row of meta - a single species on a given side of the equation
  for(i in 1:nrow(meta)){
    
    if(meta$isMult[i]){
      n = word(meta$component[i], 1) %>% # find out how many
        as.numeric()
      
      meta$component_out[i] = rep(word(meta$component[i], 2), n) %>% # repeat that that many times
        paste0(collapse = " + ")
      
    }
    
    if(meta$isFrac[i]){
      # ?
      n = word(meta$component[i], 1) %>% # find out the frac as number
        as.numeric()
      #can I find each unique fraction, group the components by it? need to return an error if "unique fractions" don't = 1
    }
  }
  
  # Return
  meta
  
}

cat("INFO: Separating multipliers (thermal), slow step", "\n")

test = thermal %>% 
  tibble() %>% 
  mutate(left = word(equation,1,sep = "="),
         right = word(equation,2,sep = "=")) %>% 
  mutate(across(everything(), function(x) str_trim(x,"both"))) %>% 
  pivot_longer(c(left, right), names_to = "side") %>% 
  mutate(hasMult = str_detect(value, "\\. ") | str_detect(value, "\\.000"), # does the side of the eqn contain any mults or fracs
         hasFrac = str_detect(value, "\\.") & !hasMult) %>% 
  mutate(value = str_split(value, "\\+") %>% # do mult / frac logic per species
           map(~str_trim(.x,"both")),
         isMult = map(value, ~{str_detect(.x, "\\. ") | str_detect(.x, "\\.000")}),
         isFrac = map2(value, isMult, ~{str_detect(.x, "\\.") & !.y})
         ) %>% 
  rowwise() %>% 
  mutate(meta = tibble(component = value, isMult = isMult, isFrac = isFrac) %>%  # combine value, mult, frac info into df
           parse_component() %>% 
           list(),
         value = paste0(meta$component_out, collapse = " + ")
         ) %>% 
  pivot_wider(id_cols = equation:species, names_from = "side", values_from = "value") %>% 
  mutate(equation_out = paste(left, right, sep = " = "))

thermal2 = test %>% 
  select(col_A:species, equation_out) %>% 
  rename(equation = equation_out)


cat("INFO: Separating multipliers (photolysis), slow step", "\n")

test = HV %>% 
  tibble() %>% 
  mutate(left = word(equation,1,sep = "="),
         right = word(equation,2,sep = "=")) %>% 
  mutate(across(everything(), function(x) str_trim(x,"both"))) %>% 
  pivot_longer(c(left, right), names_to = "side") %>% 
  mutate(hasMult = str_detect(value, "\\. ") | str_detect(value, "\\.000"), # does the side of the eqn contain any mults or fracs
         hasFrac = str_detect(value, "\\.") & !hasMult) %>% 
  mutate(value = str_split(value, "\\+") %>% # do mult / frac logic per species
           map(~str_trim(.x,"both")),
         isMult = map(value, ~{str_detect(.x, "\\. ") | str_detect(.x, "\\.000")}),
         isFrac = map2(value, isMult, ~{str_detect(.x, "\\.") & !.y})
  ) %>% 
  rowwise() %>% 
  mutate(meta = tibble(component = value, isMult = isMult, isFrac = isFrac) %>%  # combine value, mult, frac info into df
           parse_component() %>% 
           list(),
         value = paste0(meta$component_out, collapse = " + ")
  ) %>% 
  distinct() %>%  #this will delete duplicate lines (GECKO-A makes them for some reason)
  pivot_wider(id_cols = equation:mult, names_from = "side") %>% 
  mutate(equation_out = paste(left, right, sep = " = "))

HV2 = test %>% 
  select(equation_out, label, mult) %>% 
  rename(equation = equation_out)








