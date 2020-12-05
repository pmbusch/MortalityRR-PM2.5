### MortalityRR-PM2.5
## Calculate deaths avoided by reducing air quality
## PBH Dec 2020

# PM2.5 standard
standard <- 12

# beta used for CDP, per 10 ug/m3
beta <- 0.057025
exp(beta)

# Calculate delta C
df <- data_model %>% 
  filter(!is.na(mp25)) %>% 
  mutate(delta_MP25=mp25-standard, 
         delta_MP25=if_else(delta_MP25>0,delta_MP25,0))
df$delta_MP25 %>% range(na.rm = T)

# Calculate reduction in RR
df <- df %>% 
  mutate(reduction_RR=exp(-delta_MP25*beta/10))
df$reduction_RR %>% range(na.rm = T)
# df %>% dplyr::select(nombre_comuna, mp25, delta_MP25, reduction_RR) %>% view()

# Calculate new mortality rate, and new deaths
df <- df %>% 
  mutate(mrAdj_CDP_new=mrAdj_CDP*reduction_RR,
         deathsAdj_CDP_new=mrAdj_CDP_new*population/1e5,
         deathsAdj_CDP_new=as.integer(deathsAdj_CDP_new),
         avoided_deaths=deathsAdj_CDP-deathsAdj_CDP_new)
df$mrAdj_CDP_new %>% range()
df$deathsAdj_CDP_new %>% range()
df$avoided_deaths %>% range()
# df %>% dplyr::select(nombre_comuna, mp25, delta_MP25, reduction_RR,
#                      mrAdj_CDP,mrAdj_CDP_new,
#                      deathsAdj_CDP,deathsAdj_CDP_new,avoided_deaths) %>% view()

# Total avoided deaths
df$avoided_deaths %>% sum()


## EoF