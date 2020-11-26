### MortalityRR-PM2.5
## Aggregate MINVU data to commune level
## PBH Septiembre 2020


## overcrowding ---------------
# Load data
source("Scripts/Load_Data/minvu_load.R", encoding = "UTF-8")


## Percentages per commune ---------------
df_minvu %>% names()
df_minvu <- df_minvu %>% 
  mutate(perc_overcrowding_low=viv_sinHac_total/viv_total*100,
         perc_overcrowding_medium=viv_HacMedio_total/viv_total*100,
         perc_overcrowding_high=viv_HacCritico_total/viv_total*100) %>% 
  select(codigo_comuna, perc_overcrowding_low, perc_overcrowding_medium, perc_overcrowding_high)


## EoF