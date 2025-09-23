#Select FALLOFF equations and correctly name constants----
FalloffO = GECKO %>% 
  filter(grepl("\\+ FALLOFF", equation)) %>% 
  rename(Binf = col_A) %>% 
  rename(minf = col_n) %>% 
  rename(Einf = col_E) %>% 
  mutate(across(equation, str_squish))


FalloffE = GECKO %>% 
  unite(col = "all", sep = "") %>% 
  filter(grepl("FALLOFF /", all))%>%  
  mutate(across(all, str_squish))

FalloffE$all = str_remove_all(FalloffE$all, "FALLOFF /| /")
FalloffE = FalloffE %>%  
  mutate(across(all, str_squish))

FalloffE = separate_wider_delim(FalloffE ,all, delim = " ", 
                                names = c("B0", "m0", "E0", "a1", "a2", "a3", "a4"))
  

Falloff=bind_cols(FalloffO, FalloffE) 


#I hope you're feeling mathsy it's time for troe equations ----
cat("Using [M] =",M , "molecules/cm3 to calculate Troe equations\n")

Falloff = Falloff %>% 
  mutate(col_F = "") %>% 
  mutate(across(-equation, as.numeric)) %>% 
  mutate(k0 = B0 * ((temp/300)**m0) * exp(-E0/temp)) %>% 
  mutate(kinf = Binf * ((temp/300)**minf) * exp(-Einf/temp)) %>% 
  select(equation, k0, kinf, col_F, a1, a2, a3, a4) 

for (i in 1:nrow(Falloff)){
  if (Falloff$a2[i] == 0) {Falloff$col_F[i] = Falloff$a1[i]} 
  if (Falloff$a2[i] != 0 & Falloff$a4[i] != 0) {Falloff$col_F[i] = 
        (1-Falloff$a1[i]) * exp(-T/Falloff$a2[i]) + Falloff$a1[i] * exp(-T/Falloff$a3[i]) + exp(-Falloff$a4[i]/T)} 
  if (Falloff$a2[i] != 0 & Falloff$a4[i] == 0) {Falloff$col_F[i] = 
        (1-Falloff$a1[i])*exp(-T/Falloff$a2[i]) + Falloff$a1[i]*exp(-T/Falloff$a3[i])  } 
}

Falloff = Falloff %>%
  mutate(k1 = k0*M/kinf) %>% 
  mutate(col_A = (k0*M/(1+k1)) * col_F ** (1/(1+log10(k1)**2))   ) %>% 
  select(equation, col_A) %>% 
  mutate(col_A = signif(col_A, 4)) %>% 
  mutate(across(col_A, as.character)) %>% 
  mutate(col_n = "0")%>% 
  mutate(col_E = "0")


#Here is what GECKO-A code has to say:
#  !        k0 = B*(T/300)^m*exp(-E/T) and a similar expression           *
#  !        for Kinfinite. The rate constant is computed according        *
#  !        to the Troe expression :                                      *
#  !           k= [k0[M]/(1+k0[M]/kinf)] * F ^ {1/[1+[log(k0[M]/kinf)]^2]}*
#  !        Various expression for F can be set, depending on the value   *
#  !        of a2 and a4 :                                                *
#  !            - IF (a2 equal 0) THEN F=a1                               *
#  !            - IF (a2 not equal 0) and (a4 not equal 0) THEN           *
#  !                 F=(1-a1)*exp(-T/a2) + a1*exp(-T/a3) + exp(-a4/T)     *
#  !            - IF (a2 not equal 0) and (a4 equal 0) THEN               *
#  !                 F=(1-a1)*exp(-T/a2) + a1*exp(-T/a3)                  *
  


#Select ISOM equations and correctly name constants----
ISOMO = GECKO %>% 
  filter(grepl("\\+ ISOM", equation)) %>% 
  rename(k = col_A) %>% 
  mutate(across(equation, str_squish))


ISOME = GECKO %>% 
  unite(col = "all", sep = "") %>% 
  filter(grepl("ISOM /", all))%>%  
  mutate(across(all, str_squish))

ISOME$all = str_remove_all(ISOME$all, "ISOM /| /")
ISOME = ISOME %>%  
  mutate(across(all, str_squish))

ISOME = separate_wider_delim(ISOME ,all, delim = " ", 
                                names = c("i1", "i2", "i3", "i4", "i5"))

ISOM=bind_cols(ISOMO, ISOME) 

ISOM = ISOM %>% 
  mutate(across(-equation, as.numeric)) %>% 
  mutate(col_A = k*(i1*temp**4 + i2*temp**3 + i3*temp**2 + i4 + i5 ))%>% 
  mutate(col_A = signif(col_A, 4))%>%  
  select(equation, col_A, col_n, col_E) %>% 
  mutate(across(everything(), as.character))

FandI = full_join(ISOM, Falloff, by = join_by(equation, col_A, col_n, col_E))

FandI$equation = str_remove_all(FandI$equation, "\\+\\s*ISOM")
FandI$equation = str_remove_all(FandI$equation, "\\+\\s*FALLOFF")

